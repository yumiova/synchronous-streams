{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Stream.Synchronous
  ( -- * Stream views
    Stream,

    -- * Pure (unordered) streams
    MonadUnordered (first, fbyWith, statefulWith, upon),
    fby,
    fby',
    stateful,
    stateful',

    -- * Dynamically reconfigurable streams
    MonadDynamic (until),

    -- * Effecting (ordered) streams
    MonadOrdered (fbyAWith, statefulAWith),
    fbyA,
    fbyA',
    statefulA,
    statefulA',

    -- * Untransformed streams
    Source,
    toCofree,
    toList,

    -- * I/O transformed streams
    SourceIO,
    ioToCofree,
    ioToList,
  )
where

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive
  ( PrimMonad (PrimState, primitive),
    RealWorld,
    ioToPrim,
    primToIO,
  )
import Control.Monad.Primitive.Unsafe (unsafeDupableCollect)
import Control.Monad.ST (ST, runST)
import Data.AdditiveGroup (AdditiveGroup ((^+^), (^-^), negateV, zeroV))
import Data.AffineSpace (AffineSpace ((.+^), (.-.), Diff))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Primitive (newMutVar, readMutVar, writeMutVar)
import Data.String (IsString (fromString))
import Data.VectorSpace (InnerSpace ((<.>)), VectorSpace ((*^), Scalar))

infixr 5 `fby`, `fby'`, `fbyA`, `fbyA'`

infixl 4 `until`, `upon`

-- * Stream views

newtype Stream t a = Stream {runStream :: ST t a}

instance Bounded a => Bounded (Stream t a) where

  minBound = pure minBound

  maxBound = pure maxBound

instance Num a => Num (Stream t a) where

  (+) = liftA2 (+)

  (-) = liftA2 (-)

  (*) = liftA2 (*)

  abs = fmap abs

  signum = fmap signum

  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Stream t a) where

  (/) = liftA2 (/)

  fromRational = pure . fromRational

instance Floating a => Floating (Stream t a) where

  pi = pure pi

  exp = fmap exp

  log = fmap log

  sin = fmap sin

  cos = fmap cos

  asin = fmap asin

  acos = fmap acos

  atan = fmap atan

  sinh = fmap sinh

  cosh = fmap cosh

  asinh = fmap asinh

  acosh = fmap acosh

  atanh = fmap atanh

instance IsString a => IsString (Stream t a) where
  fromString = pure . fromString

instance Semigroup a => Semigroup (Stream t a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Stream t a) where
  mempty = pure mempty

instance AdditiveGroup a => AdditiveGroup (Stream t a) where

  zeroV = pure zeroV

  (^+^) = liftA2 (^+^)

  negateV = fmap negateV

  (^-^) = liftA2 (^-^)

instance AffineSpace a => AffineSpace (Stream t a) where

  type Diff (Stream t a) = Stream t (Diff a)

  (.-.) = liftA2 (.-.)

  (.+^) = liftA2 (.+^)

instance VectorSpace a => VectorSpace (Stream t a) where

  type Scalar (Stream t a) = Stream t (Scalar a)

  (*^) = liftA2 (*^)

instance InnerSpace a => InnerSpace (Stream t a) where
  (<.>) = liftA2 (<.>)

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

  first :: Stream t a -> m a

  fbyWith :: (forall r. a -> r -> r) -> a -> Stream t a -> m (Stream t a)

  statefulWith ::
    (forall r. a -> r -> r) ->
    a ->
    Stream t (a -> a) ->
    m (Stream t a)
  statefulWith before initial step =
    mfix $ \a -> fbyWith before initial (step <*> a)

  upon :: m (Stream t a) -> Stream t Bool -> m (Stream t a)

fby :: MonadUnordered t m => a -> Stream t a -> m (Stream t a)
fby = fbyWith (const id)

fby' :: MonadUnordered t m => a -> Stream t a -> m (Stream t a)
fby' = fbyWith seq

stateful :: MonadUnordered t m => a -> Stream t (a -> a) -> m (Stream t a)
stateful = statefulWith (const id)

stateful' :: MonadUnordered t m => a -> Stream t (a -> a) -> m (Stream t a)
stateful' = statefulWith seq

-- * Dynamically reconfigurable streams

class MonadUnordered t m => MonadDynamic t m where
  until :: m (Stream t a) -> Stream t Bool -> m (Stream t a)

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
  fmap f source =
    Source $ do
      ~(a, gather) <- runSource source
      pure (f a, gather)

instance Applicative f => Applicative (Source f t) where

  pure a = Source $ pure (a, pure (pure (pure ())))

  fsource <*> source =
    Source $ do
      ~(f, fgather) <- runSource fsource
      ~(a, gather) <- runSource source
      pure (f a, liftA2 (liftA2 (*>)) fgather gather)

instance Applicative f => Monad (Source f t) where
  lsource >>= f =
    Source $ do
      ~(a, lgather) <- runSource lsource
      ~(b, rgather) <- runSource (f a)
      pure (b, liftA2 (liftA2 (*>)) lgather rgather)

instance Applicative f => MonadFix (Source f t) where
  mfix f = Source $ mfix $ runSource . f . fst

instance Applicative f => MonadUnordered t (Source f t) where

  first stream =
    Source $ do
      a <- runStream stream
      pure (a, pure (pure (pure ())))

  fbyWith before initial future =
    Source $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process a = pure (scatter a)
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

  upon source continue =
    Source $ do
      ~(stream, original) <- runSource source
      let gather = do
            condition <- runStream continue
            if condition
              then original
              else pure (pure (pure ()))
      pure (stream, gather)

instance Applicative f => MonadDynamic t (Source f t) where
  until source restart =
    Source $ do
      ~(originalStream, originalGather) <- runSource source
      previousStream <- newMutVar originalStream
      previousGather <- newMutVar originalGather
      let stream =
            Stream $ do
              current <- readMutVar previousStream
              runStream current
          gather = do
            condition <- runStream restart
            if condition
              then uncurry process <$> runSource source
              else do
                current <- readMutVar previousGather
                current
          process newStream newGather = pure (scatter newStream newGather)
          scatter newStream newGather = do
            writeMutVar previousStream newStream
            writeMutVar previousGather newGather
      pure (stream, gather)

instance Applicative f => MonadOrdered f t (Source f t) where
  fbyAWith before initial future =
    Source $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process action = scatter <$> action
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

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

toCofree :: Functor f => (forall t. Source f t (Stream t a)) -> Cofree f a
toCofree = runWith (:<)

toList :: (forall t. Source Identity t (Stream t a)) -> [a]
toList = runWith (\a -> (a :) . runIdentity)

-- * I/O transformed streams

newtype SourceIO f t a
  = SourceIO
      { runSourceIO :: t ~ RealWorld => ST t (a, ST t (f (ST t ())))
      }

instance Functor (SourceIO f t) where
  fmap f source =
    SourceIO $ do
      ~(a, gather) <- runSourceIO source
      pure (f a, gather)

instance Applicative f => Applicative (SourceIO f t) where

  pure a = SourceIO $ pure (a, pure (pure (pure ())))

  fsource <*> source =
    SourceIO $ do
      ~(f, fgather) <- runSourceIO fsource
      ~(a, gather) <- runSourceIO source
      pure (f a, liftA2 (liftA2 (*>)) fgather gather)

instance Applicative f => Monad (SourceIO f t) where
  lsource >>= f =
    SourceIO $ do
      ~(a, lgather) <- runSourceIO lsource
      ~(b, rgather) <- runSourceIO (f a)
      pure (b, liftA2 (liftA2 (*>)) lgather rgather)

instance Applicative f => MonadFix (SourceIO f t) where
  mfix f = SourceIO $ mfix $ runSourceIO . f . fst

instance Applicative f => MonadIO (SourceIO f t) where
  liftIO action =
    SourceIO $ do
      a <- ioToPrim action
      pure (a, pure (pure (pure ())))

instance Applicative f => PrimMonad (SourceIO f t) where

  type PrimState (SourceIO f t) = RealWorld

  primitive f =
    SourceIO $ do
      a <- primitive f
      pure (a, pure (pure (pure ())))

instance Applicative f => MonadUnordered t (SourceIO f t) where

  first stream =
    SourceIO $ do
      a <- runStream stream
      pure (a, pure (pure (pure ())))

  fbyWith before initial future =
    SourceIO $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process a = pure (scatter a)
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

  upon source continue =
    SourceIO $ do
      ~(stream, original) <- runSourceIO source
      let gather = do
            condition <- runStream continue
            if condition
              then original
              else pure (pure (pure ()))
      pure (stream, gather)

instance MonadIO f => MonadDynamic t (SourceIO f t) where
  until source restart =
    SourceIO $ do
      ~(originalStream, originalGather) <- runSourceIO source
      previousStream <- newMutVar originalStream
      previousGather <- newMutVar originalGather
      let stream =
            Stream $ do
              current <- readMutVar previousStream
              runStream current
          gather = do
            condition <- runStream restart
            if condition
              then pure process
              else do
                current <- readMutVar previousGather
                current
          process = scatter <$> liftIO (primToIO (runSourceIO source))
          scatter ~(newStream, newGather) = do
            writeMutVar previousStream newStream
            writeMutVar previousGather newGather
      pure (stream, gather)

instance Applicative f => MonadOrdered f t (SourceIO f t) where
  fbyAWith before initial future =
    SourceIO $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process = fmap scatter
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

runIOWith ::
  (MonadIO m, Functor f) =>
  (a -> f b -> b) ->
  (forall t. SourceIO f t (Stream t a)) ->
  m b
runIOWith f source =
  liftIO $ primToIO $ do
    ~(stream, gather) <- runSourceIO source
    let xs = do
          process <- gather
          liftA2 f (runStream stream) (unsafeDupableCollect (*> xs) process)
    xs

ioToCofree ::
  (MonadIO m, Functor f) =>
  (forall t. SourceIO f t (Stream t a)) ->
  m (Cofree f a)
ioToCofree = runIOWith (:<)

ioToList :: MonadIO m => (forall t. SourceIO Identity t (Stream t a)) -> m [a]
ioToList = runIOWith (\a -> (a :) . runIdentity)
