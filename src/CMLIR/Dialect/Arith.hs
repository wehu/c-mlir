{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Arith where

import MLIR.AST

cmpf :: Int -> Location -> Type -> Name -> Name -> Operation
cmpf op loc ty lhs rhs = Operation
  { opName = "arith.cmpf"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [lhs, rhs]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "predicate" (IntegerAttr (IntegerType Signless 64) op)
  }

cmpi :: Int -> Location -> Type -> Name -> Name -> Operation
cmpi op loc ty lhs rhs = Operation
  { opName = "arith.cmpi"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [lhs, rhs]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "predicate" (IntegerAttr (IntegerType Signless 64) op)
  }