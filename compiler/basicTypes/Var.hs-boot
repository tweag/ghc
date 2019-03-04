module Var where

import GhcPrelude()

data Var
type Id = Var
type TyVar = Id
type TyCoVar = Id

data ArgFlag
