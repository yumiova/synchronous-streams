{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
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
    Moment,
    toCofree,
    toList,

    -- * I/O transformed streams
    MomentIO,
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

newtype Moment f t a = Moment {runMoment :: ST t (a, ST t (f (ST t ())))}

instance Functor (Moment f t) where
  fmap f moment =
    Moment $ do
      ~(a, gather) <- runMoment moment
      pure (f a, gather)

instance Applicative f => Applicative (Moment f t) where

  pure a = Moment $ pure (a, pure (pure (pure ())))

  fmoment <*> moment =
    Moment $ do
      ~(f, fgather) <- runMoment fmoment
      ~(a, gather) <- runMoment moment
      pure (f a, liftA2 (liftA2 (*>)) fgather gather)

instance Applicative f => Monad (Moment f t) where
  lmoment >>= f =
    Moment $ do
      ~(a, lgather) <- runMoment lmoment
      ~(b, rgather) <- runMoment (f a)
      pure (b, liftA2 (liftA2 (*>)) lgather rgather)

instance Applicative f => MonadFix (Moment f t) where
  mfix f = Moment $ mfix $ runMoment . f . fst

instance Applicative f => MonadUnordered t (Moment f t) where

  first stream =
    Moment $ do
      a <- runStream stream
      pure (a, pure (pure (pure ())))

  fbyWith before initial future =
    Moment $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process a = pure (scatter a)
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

  upon moment continue =
    Moment $ do
      ~(stream, original) <- runMoment moment
      let gather = do
            condition <- runStream continue
            if condition
              then original
              else pure (pure (pure ()))
      pure (stream, gather)

instance Applicative f => MonadDynamic t (Moment f t) where
  until moment restart =
    Moment $ do
      ~(originalStream, originalGather) <- runMoment moment
      previousStream <- newMutVar originalStream
      previousGather <- newMutVar originalGather
      let stream =
            Stream $ do
              current <- readMutVar previousStream
              runStream current
          gather = do
            condition <- runStream restart
            if condition
              then uncurry process <$> runMoment moment
              else do
                current <- readMutVar previousGather
                current
          process newStream newGather = pure (scatter newStream newGather)
          scatter newStream newGather = do
            writeMutVar previousStream newStream
            writeMutVar previousGather newGather
      pure (stream, gather)

instance Applicative f => MonadOrdered f t (Moment f t) where
  fbyAWith before initial future =
    Moment $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process action = scatter <$> action
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

runWith ::
  Functor f =>
  (a -> f b -> b) ->
  (forall t. Moment f t (Stream t a)) ->
  b
runWith f moment =
  runST $ do
    ~(stream, gather) <- runMoment moment
    let xs = do
          process <- gather
          liftA2 f (runStream stream) (unsafeDupableCollect (*> xs) process)
    xs

toCofree :: Functor f => (forall t. Moment f t (Stream t a)) -> Cofree f a
toCofree = runWith (:<)

toList :: (forall t. Moment Identity t (Stream t a)) -> [a]
toList = runWith (\a -> (a :) . runIdentity)

-- * I/O transformed streams

newtype MomentIO f t a
  = MomentIO
      { runMomentIO :: t ~ RealWorld => ST t (a, ST t (f (ST t ())))
      }

instance Functor (MomentIO f t) where
  fmap f moment =
    MomentIO $ do
      ~(a, gather) <- runMomentIO moment
      pure (f a, gather)

instance Applicative f => Applicative (MomentIO f t) where

  pure a = MomentIO $ pure (a, pure (pure (pure ())))

  fmoment <*> moment =
    MomentIO $ do
      ~(f, fgather) <- runMomentIO fmoment
      ~(a, gather) <- runMomentIO moment
      pure (f a, liftA2 (liftA2 (*>)) fgather gather)

instance Applicative f => Monad (MomentIO f t) where
  lmoment >>= f =
    MomentIO $ do
      ~(a, lgather) <- runMomentIO lmoment
      ~(b, rgather) <- runMomentIO (f a)
      pure (b, liftA2 (liftA2 (*>)) lgather rgather)

instance Applicative f => MonadFix (MomentIO f t) where
  mfix f = MomentIO $ mfix $ runMomentIO . f . fst

instance Applicative f => MonadIO (MomentIO f t) where
  liftIO action =
    MomentIO $ do
      a <- ioToPrim action
      pure (a, pure (pure (pure ())))

instance Applicative f => PrimMonad (MomentIO f t) where

  type PrimState (MomentIO f t) = RealWorld

  primitive f =
    MomentIO $ do
      a <- primitive f
      pure (a, pure (pure (pure ())))

instance Applicative f => MonadUnordered t (MomentIO f t) where

  first stream =
    MomentIO $ do
      a <- runStream stream
      pure (a, pure (pure (pure ())))

  fbyWith before initial future =
    MomentIO $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process a = pure (scatter a)
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

  upon moment continue =
    MomentIO $ do
      ~(stream, original) <- runMomentIO moment
      let gather = do
            condition <- runStream continue
            if condition
              then original
              else pure (pure (pure ()))
      pure (stream, gather)

instance MonadIO f => MonadDynamic t (MomentIO f t) where
  until moment restart =
    MomentIO $ do
      ~(originalStream, originalGather) <- runMomentIO moment
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
          process = scatter <$> liftIO (primToIO (runMomentIO moment))
          scatter ~(newStream, newGather) = do
            writeMutVar previousStream newStream
            writeMutVar previousGather newGather
      pure (stream, gather)

instance Applicative f => MonadOrdered f t (MomentIO f t) where
  fbyAWith before initial future =
    MomentIO $ do
      previous <- newMutVar initial
      let stream = Stream (readMutVar previous)
          gather = process <$> runStream future
          process = fmap scatter
          scatter a = a `before` writeMutVar previous a
      pure (stream, gather)

runIOWith ::
  (MonadIO m, Functor f) =>
  (a -> f b -> b) ->
  (forall t. MomentIO f t (Stream t a)) ->
  m b
runIOWith f moment =
  liftIO $ primToIO $ do
    ~(stream, gather) <- runMomentIO moment
    let xs = do
          process <- gather
          liftA2 f (runStream stream) (unsafeDupableCollect (*> xs) process)
    xs

ioToCofree ::
  (MonadIO m, Functor f) =>
  (forall t. MomentIO f t (Stream t a)) ->
  m (Cofree f a)
ioToCofree = runIOWith (:<)

ioToList :: MonadIO m => (forall t. MomentIO Identity t (Stream t a)) -> m [a]
ioToList = runIOWith (\a -> (a :) . runIdentity)
