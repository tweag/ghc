{-# LANGUAGE StaticPointers #-}

module RnStaticValuesFail02 where

f = static T

data T = TDataCons
