{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Arith where

import MLIR.AST
import Data.Array.IArray

cmpf :: Int -> Location -> Type -> Name -> Name -> Operation
cmpf op loc ty lhs rhs = Operation
  { opName = "arith.cmpf"
  , opLocation = loc
  , opResultTypes = Explicit [IntegerType Signless 1]
  , opOperands = [lhs, rhs]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "predicate" (IntegerAttr (IntegerType Signless 64) op)
  }

cmpi :: Int -> Location -> Type -> Name -> Name -> Operation
cmpi op loc ty lhs rhs = Operation
  { opName = "arith.cmpi"
  , opLocation = loc
  , opResultTypes = Explicit [IntegerType Signless 1]
  , opOperands = [lhs, rhs]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "predicate" (IntegerAttr (IntegerType Signless 64) op)
  }