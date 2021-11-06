{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Affine where

import MLIR.AST
import MLIR.AST.Dialect.Affine

for :: Location -> Name -> Name -> Int -> Region -> Operation
for loc lb ub step region = Operation
  { opName = "affine.for"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lb, ub]
  , opRegions = [region]
  , opSuccessors = []
  , opAttributes = namedAttribute "lower_bound" (AffineMapAttr (Map 1 0 [Dimension 0]))
                   <> namedAttribute "upper_bound" (AffineMapAttr (Map 1 0 [Dimension 0]))
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

dmaStart :: Location -> Name -> [Name] -> Name -> [Name] -> Name -> [Name] -> Name -> Operation
dmaStart loc src srcIndices dst dstIndices tag tagIndices size = Operation
  { opName = "affine.dma_start"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = src:srcIndices ++ dst:dstIndices ++ tag:tagIndices++[size]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "src_map" (AffineMapAttr (Map (length srcIndices) 0 [Dimension i| (i, _) <- zip [0..] srcIndices])) <>
                   namedAttribute "dst_map" (AffineMapAttr (Map (length dstIndices) 0 [Dimension i| (i, _) <- zip [0..] dstIndices])) <>
                   namedAttribute "tag_map" (AffineMapAttr (Map (length dstIndices) 0 [Dimension i| (i, _) <- zip [0..] tagIndices]))
  }

dmaWait :: Location -> Name -> [Name] -> Name -> Operation 
dmaWait loc tag tagIndices size = Operation
  { opName = "affine.dma_wait"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = tag:tagIndices++[size]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "tag_map" (AffineMapAttr (Map (length tagIndices) 0 [Dimension i| (i, _) <- zip [0..] tagIndices]))
  }