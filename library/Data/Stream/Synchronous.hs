{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Stream.Synchronous
  ( Stream,
    Source,
    SourceA,
    fbyWith,
    fby,
    fby',
    statefulWith,
    stateful,
    stateful',
    toList,
    toStream,
  )
where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Primitive (newMutVar, readMutVar, writeMutVar)
import qualified Data.Stream.Infinite as Infinite (Stream ((:>)))

infixr 5 `fby`, `fby'`

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

type Source = SourceA Identity

newtype SourceA f t a = SourceA {runSourceA :: ST t (a, ST t (f (ST t ())))}

instance Functor (SourceA f t) where
  fmap f = SourceA . fmap (first f) . runSourceA

instance Applicative f => Applicative (SourceA f t) where

  pure = SourceA . pure . (,pure (pure mempty))

  (<*>) fsource = SourceA . liftA2 merge (runSourceA fsource) . runSourceA
    where
      merge ~(f, fgather) ~(a, gather) =
        (f a, liftA2 (liftA2 (<>)) fgather gather)

instance Applicative f => Monad (SourceA f t) where
  lsource >>= f =
    SourceA $ do
      ~(a, lgather) <- runSourceA lsource
      second (liftA2 (liftA2 (<>)) lgather) <$> runSourceA (f a)

instance Applicative f => MonadFix (SourceA f t) where
  mfix f = SourceA $ mfix $ runSourceA . f . fst

fbyWith :: (forall r. a -> r -> r) -> a -> Stream t a -> Source t (Stream t a)
fbyWith before initial future =
  SourceA $ (stream &&& gather) <$> newMutVar initial
  where
    stream = Stream . readMutVar
    gather previous = pure . scatter previous <$> runStream future
    scatter previous a = a `before` writeMutVar previous a

fby :: a -> Stream t a -> Source t (Stream t a)
fby = fbyWith (const id)

fby' :: a -> Stream t a -> Source t (Stream t a)
fby' = fbyWith seq

statefulWith ::
  (forall r. a -> r -> r) ->
  a ->
  Stream t (a -> a) ->
  Source t (Stream t a)
statefulWith before initial step =
  mfix $ \a -> fbyWith before initial (step <*> a)

stateful :: a -> Stream t (a -> a) -> Source t (Stream t a)
stateful = statefulWith (const id)

stateful' :: a -> Stream t (a -> a) -> Source t (Stream t a)
stateful' = statefulWith seq

toList :: (forall t. Source t (Stream t a)) -> [a]
toList source =
  runST $ do
    ~(stream, gather) <- runSourceA source
    let xs = do
          process <- gather
          let scatter = runIdentity process
          scatter
          liftA2 (:) (runStream stream) (unsafeInterleaveST xs)
    liftA2 (:) (runStream stream) (unsafeInterleaveST xs)

toStream :: (forall t. Source t (Stream t a)) -> Infinite.Stream a
toStream source =
  runST $ do
    ~(stream, gather) <- runSourceA source
    let xs = do
          process <- gather
          let scatter = runIdentity process
          scatter
          liftA2 (Infinite.:>) (runStream stream) (unsafeInterleaveST xs)
    liftA2 (Infinite.:>) (runStream stream) (unsafeInterleaveST xs)
