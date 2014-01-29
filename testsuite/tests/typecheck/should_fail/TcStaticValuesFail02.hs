{-# LANGUAGE StaticValues   #-}
{-# LANGUAGE ImpredicativeTypes #-}

module StaticValuesFail02 where

import GHC.Ref

f1 :: Ref ((forall a . a -> a) -> b)
f1 = static (undefined :: (forall a . a -> a) -> b)

f2 :: Ref (Monad m => a -> m a)
f2 = static return
