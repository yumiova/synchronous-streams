{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Stream.Synchronous
  ( Stream,
    Source,
    fbyWith,
    fby,
    fby',
    statefulWith,
    stateful,
    stateful',
    SourceA,
    fbyTWith,
    fbyT,
    fbyT',
    statefulTWith,
    statefulT,
    statefulT',
    toList,
    toStream,
    toCofree,
  )
where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Primitive.Unsafe (unsafeDupableCollect)
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Primitive (newMutVar, readMutVar, writeMutVar)
import qualified Data.Stream.Infinite as Infinite (Stream ((:>)))

infixr 5 `fby`, `fby'`, `fbyT`, `fbyT'`

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

type Source t = SourceA t Identity

fbyWith ::
  Applicative f =>
  (forall r. a -> r -> r) ->
  a ->
  Stream t a ->
  SourceA t f (Stream t a)
fbyWith before initial future =
  SourceA $ (stream &&& gather) <$> newMutVar initial
  where
    stream = Stream . readMutVar
    gather previous = pure . scatter previous <$> runStream future
    scatter previous a = a `before` writeMutVar previous a

fby :: Applicative f => a -> Stream t a -> SourceA t f (Stream t a)
fby = fbyWith (const id)

fby' :: Applicative f => a -> Stream t a -> SourceA t f (Stream t a)
fby' = fbyWith seq

statefulWith ::
  Applicative f =>
  (forall r. a -> r -> r) ->
  a ->
  Stream t (a -> a) ->
  SourceA t f (Stream t a)
statefulWith before initial step =
  mfix $ \a -> fbyWith before initial (step <*> a)

stateful :: Applicative f => a -> Stream t (a -> a) -> SourceA t f (Stream t a)
stateful = statefulWith (const id)

stateful' :: Applicative f => a -> Stream t (a -> a) -> SourceA t f (Stream t a)
stateful' = statefulWith seq

newtype SourceA t f a = SourceA {runSourceA :: ST t (a, ST t (f (ST t ())))}

instance Functor (SourceA t f) where
  fmap f = SourceA . fmap (first f) . runSourceA

instance Applicative f => Applicative (SourceA t f) where

  pure = SourceA . pure . (,pure (pure mempty))

  (<*>) fsource = SourceA . liftA2 merge (runSourceA fsource) . runSourceA
    where
      merge ~(f, fgather) ~(a, gather) =
        (f a, liftA2 (liftA2 (<>)) fgather gather)

instance Applicative f => Monad (SourceA t f) where
  lsource >>= f =
    SourceA $ do
      ~(a, lgather) <- runSourceA lsource
      second (liftA2 (liftA2 (<>)) lgather) <$> runSourceA (f a)

instance Applicative f => MonadFix (SourceA t f) where
  mfix f = SourceA $ mfix $ runSourceA . f . fst

statefulTWith ::
  Applicative f =>
  (forall r. a -> r -> r) ->
  a ->
  Stream t (a -> f a) ->
  SourceA t f (Stream t a)
statefulTWith before initial step =
  mfix $ \a -> fbyTWith before initial (step <*> a)

statefulT ::
  Applicative f =>
  a ->
  Stream t (a -> f a) ->
  SourceA t f (Stream t a)
statefulT = statefulTWith (const id)

statefulT' ::
  Applicative f =>
  a ->
  Stream t (a -> f a) ->
  SourceA t f (Stream t a)
statefulT' = statefulTWith seq

fbyTWith ::
  Functor f =>
  (forall r. a -> r -> r) ->
  a ->
  Stream t (f a) ->
  SourceA t f (Stream t a)
fbyTWith before initial future =
  SourceA $ (stream &&& gather) <$> newMutVar initial
  where
    stream = Stream . readMutVar
    gather previous = fmap (scatter previous) <$> runStream future
    scatter previous a = a `before` writeMutVar previous a

fbyT :: Functor f => a -> Stream t (f a) -> SourceA t f (Stream t a)
fbyT = fbyTWith (const id)

fbyT' :: Functor f => a -> Stream t (f a) -> SourceA t f (Stream t a)
fbyT' = fbyTWith seq

accumulate ::
  Functor f =>
  (a -> f b -> b) ->
  (forall t. SourceA t f (Stream t a)) ->
  b
accumulate f source =
  runST $ do
    ~(stream, gather) <- runSourceA source
    let xs = do
          process <- gather
          liftA2 f (runStream stream) (unsafeDupableCollect (*> xs) process)
    xs

toCofree :: Functor f => (forall t. SourceA t f (Stream t a)) -> Cofree f a
toCofree = accumulate (:<)

toList :: (forall t. Source t (Stream t a)) -> [a]
toList = accumulate (\a -> (a :) . runIdentity)

toStream :: (forall t. Source t (Stream t a)) -> Infinite.Stream a
toStream = accumulate (\a -> (a Infinite.:>) . runIdentity)
