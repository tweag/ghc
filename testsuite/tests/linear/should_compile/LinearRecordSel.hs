{-# LANGUAGE LinearTypes #-}

module LinearRecordSel where

-- f should be multiplicity-polymorphic
data T = T { f :: Int }

f1 :: T ->. Int
f1 = f

f2 :: T -> Int
f2 = f
