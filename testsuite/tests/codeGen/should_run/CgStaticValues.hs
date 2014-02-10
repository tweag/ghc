{-# LANGUAGE StaticValues #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main(main) where

import Data.List ( isPrefixOf )
import System.Process

main = readProcess "sh" [] "nm --defined-only CgStaticValues.o"
        >>= const putStr (static g, static (id . (+)), static (id . show))
               . unlines
               . filter isInteresting
               . map (dropWhile (/=' '))
               . lines
  where
    isInteresting :: String -> Bool
    isInteresting x = any (`isPrefixOf` x)
      [ " D Main_g_closure"
      , " D Main_stableZZC0ZZCmainZZCMainZZCstatic_closure"
      , " D Main_stableZZC1ZZCmainZZCMainZZCstatic_closure"
      ]

g = ""

