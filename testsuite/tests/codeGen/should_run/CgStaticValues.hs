{-# LANGUAGE StaticValues #-}

module Main(main) where

import System.Process

main = readProcess "sh" [] "nm --defined-only CgStaticValues.o | grep Main_g_closure"
        >>= const putStr (static g) . dropWhile (/=' ')

g = ""


