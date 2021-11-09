{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Linalg where

import MLIR.AST
import MLIR.AST.Dialect.Affine
import Data.Array.IArray

conv1dNwcWcf :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
conv1dNwcWcf loc lhs rhs output attrs block = Operation
  { opName = "linalg.conv_1d_nwc_wcf"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 5 0 [Dimension 0, Add (Mul (Dimension 1) (Constant (head attrs))) (Mul (Dimension 3) (Constant (attrs !! 1))), Dimension 4])
                                  ,AffineMapAttr (Map 5 0 [Dimension 3, Dimension 4, Dimension 2])
                                  ,AffineMapAttr (Map 5 0 [Dimension 0, Dimension 1, Dimension 2])])
                   <> namedAttribute "strides"
                       (DenseElementsAttr (VectorType [1] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 0) $ fromIntegral <$> [head attrs])
                   <> namedAttribute "dilations"
                       (DenseElementsAttr (VectorType [1] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 0) $ fromIntegral <$> [attrs !! 1])
  }

conv1d :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
conv1d loc lhs rhs output attrs block = Operation
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


conv2dNchwFchw :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
conv2dNchwFchw loc lhs rhs output attrs block = Operation
  { opName = "linalg.conv_2d_nchw_fchw"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 7 0 [Dimension 0, Dimension 4,
                                                           Add (Mul (Dimension 2) (Constant (head attrs))) (Mul (Dimension 5) (Constant (attrs !! 2))),
                                                           Add (Mul (Dimension 3) (Constant (attrs !! 1))) (Mul (Dimension 6) (Constant (attrs !! 3)))])
                                  ,AffineMapAttr (Map 7 0 [Dimension 1, Dimension 4, Dimension 5, Dimension 6])
                                  ,AffineMapAttr (Map 7 0 [Dimension 0, Dimension 1, Dimension 2, Dimension 3])])
                   <> namedAttribute "strides"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 1) $ fromIntegral <$> take 2 attrs)
                   <> namedAttribute "dilations"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 1) $ fromIntegral <$> drop 2 attrs)
  }

conv2dNhwcHwcf :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
conv2dNhwcHwcf loc lhs rhs output attrs block = Operation
  { opName = "linalg.conv_2d_nhwc_hwcf"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 7 0 [Dimension 0, Add (Mul (Dimension 1) (Constant (head attrs))) (Mul (Dimension 4) (Constant (attrs !! 2))),
                                                           Add (Mul (Dimension 2) (Constant (attrs !! 1))) (Mul (Dimension 5) (Constant (attrs !! 3))), Dimension 6])
                                  ,AffineMapAttr (Map 7 0 [Dimension 4, Dimension 5, Dimension 6, Dimension 3])
                                  ,AffineMapAttr (Map 7 0 [Dimension 0, Dimension 1, Dimension 2, Dimension 3])])
                   <> namedAttribute "strides"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 1) $ fromIntegral <$> take 2 attrs)
                   <> namedAttribute "dilations"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Signless 64) $
                         DenseUInt64 $ listArray (0 :: Int, 1) $ fromIntegral <$> drop 2 attrs)
  }

conv2d :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
conv2d loc lhs rhs output attrs block = Operation
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

matmul :: Location -> Name -> Name -> Name -> [Int] -> Block -> Operation
matmul loc lhs rhs output attrs block = Operation
  { opName = "linalg.matmul"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = [lhs, rhs, output]
  , opRegions = [Region [block]]
  , opSuccessors = []
  , opAttributes = namedAttribute "operand_segment_sizes"
                       (DenseElementsAttr (VectorType [2] $ IntegerType Unsigned 32) $
                         DenseUInt32 $ listArray (0 :: Int, 1) $ fromIntegral <$> [2, 1])
                   <> namedAttribute "linalg.memoized_indexing_maps"
                       (ArrayAttr [AffineMapAttr (Map 3 0 [Dimension 0, Dimension 2])
                                  ,AffineMapAttr (Map 3 0 [Dimension 2, Dimension 1])
                                  ,AffineMapAttr (Map 3 0 [Dimension 0, Dimension 1])])
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