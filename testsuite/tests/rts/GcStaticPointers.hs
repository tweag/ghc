-- A test to show that -XStaticPointers keeps generated CAFs alive.
{-# LANGUAGE StaticPointers #-}
module Main where

import GHC.StaticPtr

import Control.Concurrent
import System.Mem
import System.Mem.Weak

nats :: [Integer]
nats = [0 .. ]

-- Just a StaticPtr to some CAF from another package so that we can deRef it.
nats_ref :: StaticPtr [Integer]
nats_ref = static nats

main = do
  let z = nats !! 400
  print z
  performGC
  addFinalizer z (putStrLn "finalizer z")
  print z
  performGC
  threadDelay 1000000
  print . (!!800) $ deRefStaticPtr
    (StaticPtr (StaticName "main" "Main" "sptEntry:0"):: StaticPtr [Integer])
  -- uncommenting the next line keeps primes alive and prevents the segfault
  -- print (nats !! 900)
