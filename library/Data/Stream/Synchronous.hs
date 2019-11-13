{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Stream.Synchronous
  ( -- * Stream views
    Stream,

    -- * Pure (unordered) streams
    MonadUnordered (fbyWith, statefulWith),
    fby,
    fby',
    stateful,
    stateful',

    -- * Effecting (ordered) streams
    MonadOrdered (fbyAWith, statefulAWith),
    fbyA,
    fbyA',
    statefulA,
    statefulA',

    -- * Untransformed streams
    Source,
    runAsCofree,
    runAsList,
    runAsStream,
  )
where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Primitive.Unsafe (unsafeDupableCollect)
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (bimap, second)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Primitive (newMutVar, readMutVar, writeMutVar)
import qualified Data.Stream.Infinite as Infinite (Stream ((:>)))

-- * Stream views

newtype Stream t a = Stream {runStream :: ST t a}

instance Functor (Stream t) where
  fmap f = Stream . fmap f . runStream

instance Applicative (Stream t) where

  pure = Stream . pure

  (<*>) fstream = Stream . (<*>) (runStream fstream) . runStream

instance Monad (Stream t) where
  lstream >>= f =
    Stream $ do
      a <- runStream lstream
      runStream (f a)

instance MonadFix (Stream t) where
  mfix f = Stream $ mfix $ runStream . f

-- * Pure (unordered) streams

class MonadFix m => MonadUnordered t m | m -> t where

  fbyWith :: (forall r. a -> r -> r) -> a -> Stream t a -> m (Stream t a)

  statefulWith ::
    (forall r. a -> r -> r) ->
    a ->
    Stream t (a -> a) ->
    m (Stream t a)
  statefulWith before initial step =
    mfix $ \a -> fbyWith before initial (step <*> a)

fby :: MonadUnordered t m => a -> Stream t a -> m (Stream t a)
fby = fbyWith (const id)

fby' :: MonadUnordered t m => a -> Stream t a -> m (Stream t a)
fby' = fbyWith seq

stateful :: MonadUnordered t m => a -> Stream t (a -> a) -> m (Stream t a)
stateful = statefulWith (const id)

stateful' :: MonadUnordered t m => a -> Stream t (a -> a) -> m (Stream t a)
stateful' = statefulWith seq

-- * Effecting (ordered) streams

class
  (Applicative f, MonadUnordered t m) =>
  MonadOrdered f t m
    | m -> f t where

  fbyAWith :: (forall r. a -> r -> r) -> a -> Stream t (f a) -> m (Stream t a)

  statefulAWith ::
    (forall r. a -> r -> r) ->
    a ->
    Stream t (a -> f a) ->
    m (Stream t a)
  statefulAWith before initial step =
    mfix $ \a -> fbyAWith before initial (step <*> a)

fbyA :: MonadOrdered f t m => a -> Stream t (f a) -> m (Stream t a)
fbyA = fbyAWith (const id)

fbyA' :: MonadOrdered f t m => a -> Stream t (f a) -> m (Stream t a)
fbyA' = fbyAWith seq

statefulA :: MonadOrdered f t m => a -> Stream t (a -> f a) -> m (Stream t a)
statefulA = statefulAWith (const id)

statefulA' :: MonadOrdered f t m => a -> Stream t (a -> f a) -> m (Stream t a)
statefulA' = statefulAWith seq

-- * Untransformed streams

newtype Source f t a = Source {runSource :: ST t (a, ST t (f (ST t ())))}

instance Functor (Source f t) where
  fmap f = Source . fmap (bimap f id) . runSource

instance Applicative f => Applicative (Source f t) where

  pure = Source . pure . (,pure (pure mempty))

  (<*>) fsource = Source . liftA2 merge (runSource fsource) . runSource
    where
      merge ~(f, fgather) ~(a, gather) =
        (f a, liftA2 (liftA2 (<>)) fgather gather)

instance Applicative f => Monad (Source f t) where
  lsource >>= f =
    Source $ do
      ~(a, lgather) <- runSource lsource
      second (liftA2 (liftA2 (<>)) lgather) <$> runSource (f a)

instance Applicative f => MonadFix (Source f t) where
  mfix f = Source $ mfix $ runSource . f . fst

instance Applicative f => MonadUnordered t (Source f t) where
  fbyWith before initial future =
    Source $ (stream &&& gather) <$> newMutVar initial
    where
      stream = Stream . readMutVar
      gather previous = pure . scatter previous <$> runStream future
      scatter previous a = a `before` writeMutVar previous a

instance Applicative f => MonadOrdered f t (Source f t) where
  fbyAWith before initial future =
    Source $ (stream &&& gather) <$> newMutVar initial
    where
      stream = Stream . readMutVar
      gather previous = fmap (scatter previous) <$> runStream future
      scatter previous a = a `before` writeMutVar previous a

runWith ::
  Functor f =>
  (a -> f b -> b) ->
  (forall t. Source f t (Stream t a)) ->
  b
runWith f source =
  runST $ do
    ~(stream, gather) <- runSource source
    let xs = do
          process <- gather
          liftA2 f (runStream stream) (unsafeDupableCollect (*> xs) process)
    xs

runAsCofree :: Functor f => (forall t. Source f t (Stream t a)) -> Cofree f a
runAsCofree = runWith (:<)

runAsList :: (forall t. Source Identity t (Stream t a)) -> [a]
runAsList = runWith (\a -> (a :) . runIdentity)

runAsStream :: (forall t. Source Identity t (Stream t a)) -> Infinite.Stream a
runAsStream = runWith (\a -> (a Infinite.:>) . runIdentity)
