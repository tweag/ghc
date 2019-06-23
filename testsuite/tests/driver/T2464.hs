{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTEST #-}
#ifdef TEST
{-# LANGUAGE DeriveFunctor #-}
#endif
{-# LANGUAGE NullaryTypeClasses #-}  -- generate a deprecation warning

module Test2464 where
data T a deriving (Functor)
