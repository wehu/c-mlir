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

load :: Location -> Type -> Name -> [Name] -> Operation
load loc ty src indices = Operation
  { opName = "affine.load"
  , opLocation = loc
  , opResultTypes = Explicit [ty] 
  , opOperands = src:indices
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "map" (AffineMapAttr (Map (length indices) 0 [Dimension i| (i, _) <- zip [0..] indices]))
  }

store :: Location -> Name -> Name -> [Name] -> Operation
store loc v dst indices = Operation
  { opName = "affine.store"
  , opLocation = loc
  , opResultTypes = Explicit [] 
  , opOperands = v:dst:indices
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "map" (AffineMapAttr (Map (length indices) 0 [Dimension i| (i, _) <- zip [0..] indices]))
  }

apply :: Location -> Map -> [Name] -> Operation
apply loc map operands = Operation
  { opName = "affine.apply"
  , opLocation = loc
  , opResultTypes = Explicit [IndexType]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "map" (AffineMapAttr map)
  }