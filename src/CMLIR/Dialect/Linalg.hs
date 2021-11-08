{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Linalg where

import MLIR.AST
import MLIR.AST.Dialect.Affine
import Data.Array.IArray

conv1d :: Location -> Name -> Name -> Name -> Block -> Operation
conv1d loc lhs rhs output block = Operation
  { opName = "linalg.conv_1d"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 2 0 [Add (Dimension 0) (Dimension 1)])
                                  ,AffineMapAttr (Map 2 0 [Dimension 1])
                                  ,AffineMapAttr (Map 2 0 [Dimension 0])])
  }

conv2d :: Location -> Name -> Name -> Name -> Block -> Operation
conv2d loc lhs rhs output block = Operation
  { opName = "linalg.conv_2d"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 4 0 [Add (Dimension 0) (Dimension 2), Add (Dimension 1) (Dimension 3)])
                                  ,AffineMapAttr (Map 4 0 [Dimension 2, Dimension 3])
                                  ,AffineMapAttr (Map 4 0 [Dimension 0, Dimension 1])])
  }

yield2 :: Location -> [Name] -> Operation
yield2 loc args = Operation
  { opName = "linalg.yield"
  , opLocation = loc
  , opResultTypes = Explicit [] 
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs 
  }