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

alloc :: Location -> Type -> [Name] -> [Name] -> Operation
alloc loc ty dyns syms = Operation
  { opName = "memref.alloc"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = dyns ++ syms
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes" $
                       DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [length dyns, length syms]
  }

dealloc :: Location -> Name -> Operation
dealloc loc src = Operation
  { opName = "memref.dealloc"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [src]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

cast :: Location -> Type -> Name -> Operation
cast loc ty src = Operation
  { opName = "memref.cast"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [src]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

view :: Location -> Type -> Name -> Name -> [Name] -> Operation
view loc ty src offset sizes = Operation
  { opName = "memref.view"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [src, offset]++sizes
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

copy :: Location -> Name -> Name -> Operation
copy loc src dst = Operation
  { opName = "memref.copy"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [src, dst]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }