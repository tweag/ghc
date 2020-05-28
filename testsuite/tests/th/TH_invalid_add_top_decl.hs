{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

$(do
  invalidDecl <- valD (varP (mkName "emptyDo")) (normalB (doE Nothing [])) []
  addTopDecls [invalidDecl]
  return [])
