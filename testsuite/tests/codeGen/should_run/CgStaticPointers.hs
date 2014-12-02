{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers     #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
module Main(main) where

import Data.Typeable
import GHC.StaticPtr

main :: IO ()
main = do
  print $ deRefStaticPtr (static (id . id)) (1 :: Int)
  print $ deRefStaticPtr (static method :: StaticPtr (Char -> Int)) 'a'
  print $ deRefStaticPtr (static g)
  print $ deRefStaticPtr p0 'a'
  print $ deRefStaticPtr (static t_field) $ T 'b'

g :: String
g = "found"

p0 :: Typeable a => StaticPtr (a -> a)
p0 = static (\x -> x)

data T a = T { t_field :: a }
  deriving Typeable

class C1 a where
  method :: a -> Int

instance C1 Char where
  method = const 0
