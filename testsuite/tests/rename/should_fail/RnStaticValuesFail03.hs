{-# LANGUAGE StaticValues #-}

module RnStaticValuesFail03 where

f x = static (x . id)
