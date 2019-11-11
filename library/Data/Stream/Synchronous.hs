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

newtype Source t a = Source {runSource :: ST t (a, ST t (ST t ()))}

instance Functor (Source t) where
  fmap f = Source . fmap (first f) . runSource

instance Applicative (Source t) where

  pure = Source . pure . (,mempty)

  (<*>) fsource = Source . liftA2 merge (runSource fsource) . runSource
    where
      merge ~(f, fgather) ~(a, gather) = (f a, fgather <> gather)

instance Monad (Source t) where
  lsource >>= f =
    Source $ do
      ~(a, lgather) <- runSource lsource
      second (lgather <>) <$> runSource (f a)

instance MonadFix (Source t) where
  mfix f = Source $ mfix $ runSource . f . fst

fbyWith :: (forall r. a -> r -> r) -> a -> Stream t a -> Source t (Stream t a)
fbyWith before initial future =
  Source $ (stream &&& gather) <$> newMutVar initial
  where
    stream = Stream . readMutVar
    gather previous = scatter previous <$> runStream future
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
    ~(stream, gather) <- runSource source
    let xs = do
          scatter <- gather
          scatter
          liftA2 (:) (runStream stream) (unsafeInterleaveST xs)
    liftA2 (:) (runStream stream) (unsafeInterleaveST xs)

toStream :: (forall t. Source t (Stream t a)) -> Infinite.Stream a
toStream source =
  runST $ do
    ~(stream, gather) <- runSource source
    let xs = do
          scatter <- gather
          scatter
          liftA2 (Infinite.:>) (runStream stream) (unsafeInterleaveST xs)
    liftA2 (Infinite.:>) (runStream stream) (unsafeInterleaveST xs)
