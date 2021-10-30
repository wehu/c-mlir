{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.MemRef where

import MLIR.AST
import Data.Array.IArray

alloca :: Location -> Type -> [Name] -> [Name] -> Operation
alloca loc ty dyns syms = Operation
  { opName = "memref.alloca"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = dyns ++ syms
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes" $
                       DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [length dyns, length syms]
  }