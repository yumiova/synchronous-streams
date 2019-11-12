{-# LANGUAGE UnboxedTuples #-}

module Control.Monad.Primitive.Unsafe
  ( unsafeDupableCollect,
  )
where

import Control.Monad.Primitive (PrimBase (internal), PrimMonad (primitive))

unsafeDupableCollect :: (Functor f, PrimBase m) => (a -> m b) -> f a -> m (f b)
unsafeDupableCollect f action =
  primitive $ \world ->
    (#
      world,
      (\a -> let (# _, b #) = internal (f a) world in b) <$> action
    #)
{-# NOINLINE unsafeDupableCollect #-}
