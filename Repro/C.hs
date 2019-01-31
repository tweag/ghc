module Repro.C where

data Doc
  = Empty
  | TextBeside !AnnotDetails
  | Beside Doc Doc
  | B Doc

data AnnotDetails = NoAnnot TextDetails !Int

data TextDetails = Str String

text :: String -> Doc
text s = TextBeside (NoAnnot (Str s) (length s))

hcat :: [Int] -> Doc
hcat [] = Empty
hcat xs = hcat xs

pretV = hcat []

append :: Doc -> Doc -> Doc
append p Empty = p
append p q     = Beside p q
