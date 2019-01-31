module Repro.A where

import Repro.C

data License

foo :: Doc
foo = text "" `append` (text "" `append` pretV )

class Pretty a where
  pretty :: a -> Doc

instance Pretty License where
  pretty _   = foo

---
optionalFieldDefAla
    :: (Pretty a)
    => (a -> a)
    -> Doc

optionalFieldDefAla pa = case pretty (pa (pa (pa u))) of
   TextBeside _ -> u
   x -> B x

u = u
