{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Linear4 where

correctCase :: Bool ⊸ a ⊸ a
correctCase x n =
  case x of
    True -> n
    False -> n

-- Once this is fixed, correctIf from Linear3 can be removed.
correctIf :: Bool ⊸ a ⊸ a
correctIf x n =
   if x then n else n
