{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module T11077 (module X, foo) where
import Data.List as X
foo = undefined
