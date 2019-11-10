module Data.Stream.Synchronous (Stream) where

import Control.Monad.ST (ST)

newtype Stream t a = Stream {runStream :: ST t a}
