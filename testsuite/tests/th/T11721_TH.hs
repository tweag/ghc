{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module T11721_TH where

import Language.Haskell.TH

data T a where
  MkT :: forall b a. b -> T a

$(return [])

main :: IO ()
main = print
  $(do let varName :: TyVarBndr -> Maybe String
           varName (KindedTV v _) = Just (nameBase v)
           varName _ = Nothing

       TyConI (DataD _ _ _ _
                     [ForallC con_tvbs1 _ _] _) <- reify ''T
       DataConI _ (ForallT con_tvbs2 _ _) _ <- reify 'MkT

       let actual   = (mapM varName con_tvbs1, mapM varName con_tvbs2)
           expected = (Just ["b", "a"]       , Just ["n", "b", "a"])
       if actual == expected
          then [| () |]
          else fail $ "TH11721_TH failed " ++ show (actual, expected))
