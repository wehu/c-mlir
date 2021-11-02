{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Affine where

import MLIR.AST
import MLIR.AST.Dialect.Affine

for :: Location -> Int -> Int -> Int -> Region -> Operation
for loc lb ub step region = Operation
  { opName = "affine.for"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = []
  , opRegions = [region]
  , opSuccessors = []
  , opAttributes = namedAttribute "lower_bound" (AffineMapAttr (Map 0 0 [Constant lb]))
                   <> namedAttribute "upper_bound" (AffineMapAttr (Map 0 0 [Constant ub]))
                   <> namedAttribute "step" (IntegerAttr (IntegerType Signless 64) step)
  }

yield :: Location -> [Name] -> Operation
yield loc args = Operation
  { opName = "affine.yield"
  , opLocation = loc
  , opResultTypes = Explicit [] 
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs 
  }