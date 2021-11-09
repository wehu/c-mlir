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

reinterpretCast :: Location -> Type -> Name -> [Name] -> [Name] -> [Name] -> [Int] -> [Int] -> [Int] -> Operation
reinterpretCast loc ty src offsets sizes strides staticOffsets staticSizes staticStrides = Operation
  { opName = "memref.reinterpret_cast"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = src:offsets++sizes++strides
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [4] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 3) $ fromIntegral <$> [1, length offsets, length sizes, length strides])
                  <> namedAttribute "static_offsets" 
                      (ArrayAttr $ IntegerAttr (IntegerType Signless 64) . fromIntegral <$> staticOffsets)
                  <> namedAttribute "static_sizes"
                      (ArrayAttr $ IntegerAttr (IntegerType Signless 64) . fromIntegral <$> staticSizes)
                  <> namedAttribute "static_strides" 
                      (ArrayAttr $ IntegerAttr (IntegerType Signless 64) . fromIntegral <$> staticStrides)
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

dmaStart :: Location -> Name -> [Name] -> Name -> [Name] -> Name -> [Name] -> Name -> Operation
dmaStart loc src srcIndices dst dstIndices tag tagIndices size = Operation
  { opName = "memref.dma_start"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = src:srcIndices ++ dst:dstIndices ++ [size] ++ tag:tagIndices
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

dmaWait :: Location -> Name -> [Name] -> Name -> Operation 
dmaWait loc tag tagIndices size = Operation
  { opName = "memref.dma_wait"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = tag:tagIndices ++ [size]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

allocaScope :: Location -> Region -> Operation
allocaScope loc body = Operation
  { opName = "memref.alloca_scope"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = []
  , opRegions = [body]
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

allocaScopeReturn :: Location -> [Type] -> [Name] -> Operation
allocaScopeReturn loc types args = Operation
  { opName = "memref.alloca_scope.return"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

dim :: Location -> Name -> Name -> Operation
dim loc src d = Operation
  { opName = "memref.dim"
  , opLocation = loc
  , opResultTypes = Explicit [IndexType]
  , opOperands = [src, d]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

reshape :: Location -> Type -> Name -> Name -> Operation
reshape loc ty src shape = Operation
  { opName = "memref.reshape"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [src, shape]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }
