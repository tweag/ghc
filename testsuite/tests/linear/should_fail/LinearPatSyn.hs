{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module LinearPatSyn where

-- Linearity and pattern synonyms should eventually play well together, but it
-- seems to require changes to the desugarer. So currently pattern synonyms are
-- disallowed in linear patterns.

pattern P :: b #-> a #-> (a, b)
pattern P y x = (x, y)

s :: (a, b) #-> (b, a)
s (P y x) = (y, x)
