-- Tests that when the StaticPointers extension is not enabled
-- the static identifier can be used as a regular Haskell
-- identifier.
module RdrNoStaticValues01 where

f :: Int -> Int
f static = static
