{-# LANGUAGE TupleSections #-}

module Data.Stream.Synchronous
  ( Stream,
    Source,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.ST (ST)
import Data.Bifunctor (first, second)

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
