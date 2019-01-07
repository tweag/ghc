module Var where

import GhcPrelude
import Outputable
import Data.Data

data Var
type Id = Var
type TyVar = Id
type TyCoVar = Id

data ArgFlag
