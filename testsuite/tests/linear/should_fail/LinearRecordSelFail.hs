{-# LANGUAGE LinearTypes #-}

module LinearRecordSel where

-- Only one-field one-constructor records are linear
data T1 = C { x1, x2 :: Int }
data T2 = D1 { u :: Int } | D2 { v :: Int }

f :: T1 ->. Int
f = x1

g :: T2 ->. Int
g = u
