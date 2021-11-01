{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.TranslatorSpec where

import Test.Hspec
import Text.RawString.QQ
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.List
import CMLIR.Parser
import CMLIR.Translator

shouldBeTranslatedAs code ir = do
  let ast = processString code
  output <- case ast of
              Left errs -> return $ show errs
              Right ast -> translateToMLIR ast
  removeEmptyLines (BU.fromString output) `shouldBe` removeEmptyLines ir
  where removeEmptyLines s = 
          BS.intercalate "\n" [l | l <- BS.split (fromIntegral $ ord '\n') s, 
                   [c | c<- BU.toString l, c /= ' '] /= ""]

spec :: Spec
spec = do
  describe "translator" $ do
    it "can translate builtin types" $ do
      [r|
void foo() {
  char v0;
  unsigned char v1;
  short v2;
  unsigned short v3;
  int v4;
  unsigned int v5;
  long v6;
  unsigned long v7;
  float v8;
  double v9;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi8>
    %1 = memref.alloca() : memref<1xi8>
    %2 = memref.alloca() : memref<1xi16>
    %3 = memref.alloca() : memref<1xi16>
    %4 = memref.alloca() : memref<1xi32>
    %5 = memref.alloca() : memref<1xi32>
    %6 = memref.alloca() : memref<1xi64>
    %7 = memref.alloca() : memref<1xi64>
    %8 = memref.alloca() : memref<1xf32>
    %9 = memref.alloca() : memref<1xf64>
    return
  }
}
      |]
    
    it "can translate static array types" $ do
      [r|
void foo() {
  int v0[4][5];
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<4x5xi32>
    return
  }
}
      |]

    it "can translate static array access" $ do
      [r|
void foo() {
  int v0[4][5];
  v0[2][3] = v0[2][1];
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<4x5xi32>
    %c2_i32 = arith.constant 2 : i32
    %c1_i32 = arith.constant 1 : i32
    %1 = arith.index_cast %c2_i32 : i32 to index
    %2 = arith.index_cast %c1_i32 : i32 to index
    %3 = memref.load %0[%1, %2] : memref<4x5xi32>
    %c2_i32_0 = arith.constant 2 : i32
    %c3_i32 = arith.constant 3 : i32
    %4 = arith.index_cast %c2_i32_0 : i32 to index
    %5 = arith.index_cast %c3_i32 : i32 to index
    memref.store %3, %0[%4, %5] : memref<4x5xi32>
    return
  }
}
      |]

    it "can translate literals" $ do
      [r|
void foo() {
  char v0 = 'a';
  int v1 = 1;
  long v2 = 1L;
  float v3 = 0.1;
  double v4 = 0.1L;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %c97_i8 = arith.constant 97 : i8
    %0 = memref.alloca() : memref<1xi8>
    %c0 = arith.constant 0 : index
    memref.store %c97_i8, %0[%c0] : memref<1xi8>
    %c1_i32 = arith.constant 1 : i32
    %1 = memref.alloca() : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    memref.store %c1_i32, %1[%c0_0] : memref<1xi32>
    %c1_i64 = arith.constant 1 : i64
    %2 = memref.alloca() : memref<1xi64>
    %c0_1 = arith.constant 0 : index
    memref.store %c1_i64, %2[%c0_1] : memref<1xi64>
    %cst = arith.constant 1.000000e-01 : f32
    %3 = memref.alloca() : memref<1xf32>
    %c0_2 = arith.constant 0 : index
    memref.store %cst, %3[%c0_2] : memref<1xf32>
    %cst_3 = arith.constant 1.000000e-01 : f64
    %4 = memref.alloca() : memref<1xf64>
    %c0_4 = arith.constant 0 : index
    memref.store %cst_3, %4[%c0_4] : memref<1xf64>
    return
  }
}
      |]

    it "can translate int arith operations" $ do
      [r|
void foo() {
  int v0,v1;
  v0 + v1;
  v0 - v1;
  v0 * v1;
  v0 / v1;
  v0 % v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.addi %2, %3 : i32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xi32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xi32>
    %7 = arith.subi %5, %6 : i32
    %c0_3 = arith.constant 0 : index
    %8 = memref.load %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %9 = memref.load %1[%c0_4] : memref<1xi32>
    %10 = arith.muli %8, %9 : i32
    %c0_5 = arith.constant 0 : index
    %11 = memref.load %0[%c0_5] : memref<1xi32>
    %c0_6 = arith.constant 0 : index
    %12 = memref.load %1[%c0_6] : memref<1xi32>
    %13 = arith.divsi %11, %12 : i32
    %c0_7 = arith.constant 0 : index
    %14 = memref.load %0[%c0_7] : memref<1xi32>
    %c0_8 = arith.constant 0 : index
    %15 = memref.load %1[%c0_8] : memref<1xi32>
    %16 = arith.remsi %14, %15 : i32
    return
  }
}
      |]

    it "can translate unsigned int arith operations" $ do
      [r|
void foo() {
  unsigned int v0,v1;
  v0 + v1;
  v0 - v1;
  v0 * v1;
  v0 / v1;
  v0 % v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.addi %2, %3 : i32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xi32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xi32>
    %7 = arith.subi %5, %6 : i32
    %c0_3 = arith.constant 0 : index
    %8 = memref.load %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %9 = memref.load %1[%c0_4] : memref<1xi32>
    %10 = arith.muli %8, %9 : i32
    %c0_5 = arith.constant 0 : index
    %11 = memref.load %0[%c0_5] : memref<1xi32>
    %c0_6 = arith.constant 0 : index
    %12 = memref.load %1[%c0_6] : memref<1xi32>
    %13 = arith.divui %11, %12 : i32
    %c0_7 = arith.constant 0 : index
    %14 = memref.load %0[%c0_7] : memref<1xi32>
    %c0_8 = arith.constant 0 : index
    %15 = memref.load %1[%c0_8] : memref<1xi32>
    %16 = arith.remui %14, %15 : i32
    return
  }
}
      |]
    
    it "can translate float arith operations" $ do
      [r|
void foo() {
  float v0,v1;
  v0 + v1;
  v0 - v1;
  v0 * v1;
  v0 / v1;
  v0 % v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xf32>
    %1 = memref.alloca() : memref<1xf32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xf32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xf32>
    %4 = arith.addf %2, %3 : f32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xf32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xf32>
    %7 = arith.subf %5, %6 : f32
    %c0_3 = arith.constant 0 : index
    %8 = memref.load %0[%c0_3] : memref<1xf32>
    %c0_4 = arith.constant 0 : index
    %9 = memref.load %1[%c0_4] : memref<1xf32>
    %10 = arith.mulf %8, %9 : f32
    %c0_5 = arith.constant 0 : index
    %11 = memref.load %0[%c0_5] : memref<1xf32>
    %c0_6 = arith.constant 0 : index
    %12 = memref.load %1[%c0_6] : memref<1xf32>
    %13 = arith.divf %11, %12 : f32
    %c0_7 = arith.constant 0 : index
    %14 = memref.load %0[%c0_7] : memref<1xf32>
    %c0_8 = arith.constant 0 : index
    %15 = memref.load %1[%c0_8] : memref<1xf32>
    %16 = arith.remf %14, %15 : f32
    return
  }
}
      |]

    it "can translate int compare operations" $ do
      [r|
void foo() {
  int v0,v1;
  v0 == v1;
  v0 != v1;
  v0 > v1;
  v0 < v1;
  v0 >= v1;
  v0 <= v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xi32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xi32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %c0_3 = arith.constant 0 : index
    %8 = memref.load %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %9 = memref.load %1[%c0_4] : memref<1xi32>
    %10 = arith.cmpi sgt, %8, %9 : i32
    %c0_5 = arith.constant 0 : index
    %11 = memref.load %0[%c0_5] : memref<1xi32>
    %c0_6 = arith.constant 0 : index
    %12 = memref.load %1[%c0_6] : memref<1xi32>
    %13 = arith.cmpi slt, %11, %12 : i32
    %c0_7 = arith.constant 0 : index
    %14 = memref.load %0[%c0_7] : memref<1xi32>
    %c0_8 = arith.constant 0 : index
    %15 = memref.load %1[%c0_8] : memref<1xi32>
    %16 = arith.cmpi sge, %14, %15 : i32
    %c0_9 = arith.constant 0 : index
    %17 = memref.load %0[%c0_9] : memref<1xi32>
    %c0_10 = arith.constant 0 : index
    %18 = memref.load %1[%c0_10] : memref<1xi32>
    %19 = arith.cmpi sle, %17, %18 : i32
    return
  }
}
      |]
    
    it "can translate unsigned int compare operations" $ do
      [r|
void foo() {
  unsigned int v0,v1;
  v0 == v1;
  v0 != v1;
  v0 > v1;
  v0 < v1;
  v0 >= v1;
  v0 <= v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xi32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xi32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %c0_3 = arith.constant 0 : index
    %8 = memref.load %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %9 = memref.load %1[%c0_4] : memref<1xi32>
    %10 = arith.cmpi ugt, %8, %9 : i32
    %c0_5 = arith.constant 0 : index
    %11 = memref.load %0[%c0_5] : memref<1xi32>
    %c0_6 = arith.constant 0 : index
    %12 = memref.load %1[%c0_6] : memref<1xi32>
    %13 = arith.cmpi ult, %11, %12 : i32
    %c0_7 = arith.constant 0 : index
    %14 = memref.load %0[%c0_7] : memref<1xi32>
    %c0_8 = arith.constant 0 : index
    %15 = memref.load %1[%c0_8] : memref<1xi32>
    %16 = arith.cmpi uge, %14, %15 : i32
    %c0_9 = arith.constant 0 : index
    %17 = memref.load %0[%c0_9] : memref<1xi32>
    %c0_10 = arith.constant 0 : index
    %18 = memref.load %1[%c0_10] : memref<1xi32>
    %19 = arith.cmpi ule, %17, %18 : i32
    return
  }
}
      |]

    it "can translate logic operations" $ do
      [r|
void foo() {
  unsigned int v0,v1;
  v0 == v1 && v0 == v1;
  v0 == v1 || v0 == v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_1 = arith.constant 0 : index
    %5 = memref.load %0[%c0_1] : memref<1xi32>
    %c0_2 = arith.constant 0 : index
    %6 = memref.load %1[%c0_2] : memref<1xi32>
    %7 = arith.cmpi eq, %5, %6 : i32
    %8 = arith.andi %4, %7 : i1
    %c0_3 = arith.constant 0 : index
    %9 = memref.load %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %10 = memref.load %1[%c0_4] : memref<1xi32>
    %11 = arith.cmpi eq, %9, %10 : i32
    %c0_5 = arith.constant 0 : index
    %12 = memref.load %0[%c0_5] : memref<1xi32>
    %c0_6 = arith.constant 0 : index
    %13 = memref.load %1[%c0_6] : memref<1xi32>
    %14 = arith.cmpi eq, %12, %13 : i32
    %15 = arith.ori %11, %14 : i1
    return
  }
}
      |]

    it "can translate assign operations" $ do
      [r|
void foo() {
  int i;
  i += 1;
  i -= 1;
  i *= 1;
  i /= 1;
  i %= 1;
  i &= 1;
  i |= 1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %1 = memref.load %0[%c0] : memref<1xi32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    %c0_0 = arith.constant 0 : index
    memref.store %2, %0[%c0_0] : memref<1xi32>
    %c0_1 = arith.constant 0 : index
    %3 = memref.load %0[%c0_1] : memref<1xi32>
    %c1_i32_2 = arith.constant 1 : i32
    %4 = arith.subi %3, %c1_i32_2 : i32
    %c0_3 = arith.constant 0 : index
    memref.store %4, %0[%c0_3] : memref<1xi32>
    %c0_4 = arith.constant 0 : index
    %5 = memref.load %0[%c0_4] : memref<1xi32>
    %c1_i32_5 = arith.constant 1 : i32
    %6 = arith.muli %5, %c1_i32_5 : i32
    %c0_6 = arith.constant 0 : index
    memref.store %6, %0[%c0_6] : memref<1xi32>
    %c0_7 = arith.constant 0 : index
    %7 = memref.load %0[%c0_7] : memref<1xi32>
    %c1_i32_8 = arith.constant 1 : i32
    %8 = arith.divsi %7, %c1_i32_8 : i32
    %c0_9 = arith.constant 0 : index
    memref.store %8, %0[%c0_9] : memref<1xi32>
    %c0_10 = arith.constant 0 : index
    %9 = memref.load %0[%c0_10] : memref<1xi32>
    %c1_i32_11 = arith.constant 1 : i32
    %10 = arith.remsi %9, %c1_i32_11 : i32
    %c0_12 = arith.constant 0 : index
    memref.store %10, %0[%c0_12] : memref<1xi32>
    %c0_13 = arith.constant 0 : index
    %11 = memref.load %0[%c0_13] : memref<1xi32>
    %c1_i32_14 = arith.constant 1 : i32
    %12 = arith.andi %11, %c1_i32_14 : i32
    %c0_15 = arith.constant 0 : index
    memref.store %12, %0[%c0_15] : memref<1xi32>
    %c0_16 = arith.constant 0 : index
    %13 = memref.load %0[%c0_16] : memref<1xi32>
    %c1_i32_17 = arith.constant 1 : i32
    %14 = arith.ori %13, %c1_i32_17 : i32
    %c0_18 = arith.constant 0 : index
    memref.store %14, %0[%c0_18] : memref<1xi32>
    return
  }
}
      |]
    
    it "can translate function arguments" $ do
      [r|
void foo(int arg0, float arg1) {
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo(%arg0: i32, %arg1: f32) {
    return
  }
}
      |]

    it "can translate function call" $ do
      [r|
int bar(int arg0) {
  return arg0;
}
void foo(int arg0, float arg1) {
  bar(arg0);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @bar(%arg0: i32) -> i32 {
    return %arg0 : i32
  }
  func @foo(%arg0: i32, %arg1: f32) {
    %0 = call @bar(%arg0) : (i32) -> i32
    return
  }
}
      |]
    
    it "can translate extern functions" $ do
      [r|
int bar(int arg0);
void foo(int arg0, float arg1) {
  bar(arg0) + 1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func private @bar(i32) -> i32
  func @foo(%arg0: i32, %arg1: f32) {
    %0 = call @bar(%arg0) : (i32) -> i32
    %c1_i32 = arith.constant 1 : i32
    %1 = arith.addi %0, %c1_i32 : i32
    return
  }
}
      |]

    it "can translate for loop to affine for" $ do
      [r|
void foo() {
  for (int i=0; i < 10; i += 2) {
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    affine.for %arg0 = 0 to 10 step 2 {
      %0 = arith.index_cast %arg0 : index to i32
    }
    return
  }
}
      |]

    it "can translate for loop to scf for" $ do
      [r|
void foo() {
  int j = 0;
  for (int i=j; i < 10; i += 2) {
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %c0_i32 = arith.constant 0 : i32
    %0 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    memref.store %c0_i32, %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %1 = memref.load %0[%c0_0] : memref<1xi32>
    %c10_i32 = arith.constant 10 : i32
    %c2_i32 = arith.constant 2 : i32
    %2 = arith.index_cast %1 : i32 to index
    %3 = arith.index_cast %c10_i32 : i32 to index
    %4 = arith.index_cast %c2_i32 : i32 to index
    scf.for %arg0 = %2 to %3 step %4 {
      %5 = arith.index_cast %arg0 : index to i32
    }
    return
  }
}
      |]

    it "can translate ifelse to scf if" $ do
      [r|
void foo() {
  int i,j;
  if (i > j) {
  } else {
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi sgt, %2, %3 : i32
    scf.if %4 {
    } else {
    }
    return
  }
}
      |]

    it "can translate if to scf if" $ do
      [r|
void foo() {
  int i,j;
  if (i > j) {
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi sgt, %2, %3 : i32
    scf.if %4 {
    }
    return
  }
}
      |]

    it "can translate nested for/if" $ do
      [r|
void foo() {
  int i,j;
  for (int i=0; i<10; i+=1) {
    if (i > j) {
    }
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    affine.for %arg0 = 0 to 10 {
      %2 = arith.index_cast %arg0 : index to i32
      %c0 = arith.constant 0 : index
      %3 = memref.load %1[%c0] : memref<1xi32>
      %4 = arith.cmpi sgt, %2, %3 : i32
      scf.if %4 {
      }
    }
    return
  }
}
      |]
    
    it "can translate nested for/for" $ do
      [r|
void foo() {
  for (int i=0; i<10; i+=1) {
    for (int j=0; j<10; j+=2) {
    }
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    affine.for %arg0 = 0 to 10 {
      %0 = arith.index_cast %arg0 : index to i32
      affine.for %arg1 = 0 to 10 step 2 {
        %1 = arith.index_cast %arg1 : index to i32
      }
    }
    return
  }
}
      |]

    it "can translate nested if/for" $ do
      [r|
void foo() {
  int i, j;
  if (i != j) {
    for (int j=0; j<10; j+=2) {
    }
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi32>
    %1 = memref.alloca() : memref<1xi32>
    %c0 = arith.constant 0 : index
    %2 = memref.load %0[%c0] : memref<1xi32>
    %c0_0 = arith.constant 0 : index
    %3 = memref.load %1[%c0_0] : memref<1xi32>
    %4 = arith.cmpi ne, %2, %3 : i32
    scf.if %4 {
      affine.for %arg0 = 0 to 10 step 2 {
        %5 = arith.index_cast %arg0 : index to i32
      }
    }
    return
  }
}
      |]

    it "can translate cast to integer" $ do
      [r|
void foo() {
  char v0;
  short v1;
  int v2;
  long v3;
  float v4;
  double v5;
  (char)v0;
  (char)v1;
  (char)v2;
  (char)v3;
  (char)v4;
  (char)v5;
  (short)v0;
  (short)v1;
  (short)v2;
  (short)v3;
  (short)v4;
  (short)v5;
  (int)v0;
  (int)v1;
  (int)v2;
  (int)v3;
  (int)v4;
  (int)v5;
  (long)v0;
  (long)v1;
  (long)v2;
  (long)v3;
  (long)v4;
  (long)v5;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi8>
    %1 = memref.alloca() : memref<1xi16>
    %2 = memref.alloca() : memref<1xi32>
    %3 = memref.alloca() : memref<1xi64>
    %4 = memref.alloca() : memref<1xf32>
    %5 = memref.alloca() : memref<1xf64>
    %c0 = arith.constant 0 : index
    %6 = memref.load %0[%c0] : memref<1xi8>
    %c0_0 = arith.constant 0 : index
    %7 = memref.load %1[%c0_0] : memref<1xi16>
    %8 = arith.trunci %7 : i16 to i8
    %c0_1 = arith.constant 0 : index
    %9 = memref.load %2[%c0_1] : memref<1xi32>
    %10 = arith.trunci %9 : i32 to i8
    %c0_2 = arith.constant 0 : index
    %11 = memref.load %3[%c0_2] : memref<1xi64>
    %12 = arith.trunci %11 : i64 to i8
    %c0_3 = arith.constant 0 : index
    %13 = memref.load %4[%c0_3] : memref<1xf32>
    %14 = arith.fptosi %13 : f32 to i32
    %15 = arith.trunci %14 : i32 to i8
    %c0_4 = arith.constant 0 : index
    %16 = memref.load %5[%c0_4] : memref<1xf64>
    %17 = arith.fptosi %16 : f64 to i64
    %18 = arith.trunci %17 : i64 to i8
    %c0_5 = arith.constant 0 : index
    %19 = memref.load %0[%c0_5] : memref<1xi8>
    %20 = arith.extsi %19 : i8 to i16
    %c0_6 = arith.constant 0 : index
    %21 = memref.load %1[%c0_6] : memref<1xi16>
    %c0_7 = arith.constant 0 : index
    %22 = memref.load %2[%c0_7] : memref<1xi32>
    %23 = arith.trunci %22 : i32 to i16
    %c0_8 = arith.constant 0 : index
    %24 = memref.load %3[%c0_8] : memref<1xi64>
    %25 = arith.trunci %24 : i64 to i16
    %c0_9 = arith.constant 0 : index
    %26 = memref.load %4[%c0_9] : memref<1xf32>
    %27 = arith.fptosi %26 : f32 to i32
    %28 = arith.trunci %27 : i32 to i16
    %c0_10 = arith.constant 0 : index
    %29 = memref.load %5[%c0_10] : memref<1xf64>
    %30 = arith.fptosi %29 : f64 to i64
    %31 = arith.trunci %30 : i64 to i16
    %c0_11 = arith.constant 0 : index
    %32 = memref.load %0[%c0_11] : memref<1xi8>
    %33 = arith.extsi %32 : i8 to i32
    %c0_12 = arith.constant 0 : index
    %34 = memref.load %1[%c0_12] : memref<1xi16>
    %35 = arith.extsi %34 : i16 to i32
    %c0_13 = arith.constant 0 : index
    %36 = memref.load %2[%c0_13] : memref<1xi32>
    %c0_14 = arith.constant 0 : index
    %37 = memref.load %3[%c0_14] : memref<1xi64>
    %38 = arith.trunci %37 : i64 to i32
    %c0_15 = arith.constant 0 : index
    %39 = memref.load %4[%c0_15] : memref<1xf32>
    %40 = arith.fptosi %39 : f32 to i32
    %c0_16 = arith.constant 0 : index
    %41 = memref.load %5[%c0_16] : memref<1xf64>
    %42 = arith.fptosi %41 : f64 to i64
    %43 = arith.trunci %42 : i64 to i32
    %c0_17 = arith.constant 0 : index
    %44 = memref.load %0[%c0_17] : memref<1xi8>
    %45 = arith.extsi %44 : i8 to i64
    %c0_18 = arith.constant 0 : index
    %46 = memref.load %1[%c0_18] : memref<1xi16>
    %47 = arith.extsi %46 : i16 to i64
    %c0_19 = arith.constant 0 : index
    %48 = memref.load %2[%c0_19] : memref<1xi32>
    %49 = arith.extsi %48 : i32 to i64
    %c0_20 = arith.constant 0 : index
    %50 = memref.load %3[%c0_20] : memref<1xi64>
    %c0_21 = arith.constant 0 : index
    %51 = memref.load %4[%c0_21] : memref<1xf32>
    %52 = arith.fptosi %51 : f32 to i32
    %53 = arith.extsi %52 : i32 to i64
    %c0_22 = arith.constant 0 : index
    %54 = memref.load %5[%c0_22] : memref<1xf64>
    %55 = arith.fptosi %54 : f64 to i64
    return
  }
}
      |]

    it "can translate cast to float" $ do
      [r|
void foo() {
  char v0;
  short v1;
  int v2;
  long v3;
  float v4;
  double v5;
  (float)v1;
  (float)v2;
  (float)v3;
  (float)v4;
  (float)v5;
  (double)v1;
  (double)v2;
  (double)v3;
  (double)v4;
  (double)v5;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() {
    %0 = memref.alloca() : memref<1xi8>
    %1 = memref.alloca() : memref<1xi16>
    %2 = memref.alloca() : memref<1xi32>
    %3 = memref.alloca() : memref<1xi64>
    %4 = memref.alloca() : memref<1xf32>
    %5 = memref.alloca() : memref<1xf64>
    %c0 = arith.constant 0 : index
    %6 = memref.load %1[%c0] : memref<1xi16>
    %7 = arith.sitofp %6 : i16 to f16
    %8 = arith.extf %7 : f16 to f32
    %c0_0 = arith.constant 0 : index
    %9 = memref.load %2[%c0_0] : memref<1xi32>
    %10 = arith.sitofp %9 : i32 to f32
    %c0_1 = arith.constant 0 : index
    %11 = memref.load %3[%c0_1] : memref<1xi64>
    %12 = arith.sitofp %11 : i64 to f64
    %13 = arith.truncf %12 : f64 to f32
    %c0_2 = arith.constant 0 : index
    %14 = memref.load %4[%c0_2] : memref<1xf32>
    %c0_3 = arith.constant 0 : index
    %15 = memref.load %5[%c0_3] : memref<1xf64>
    %16 = arith.truncf %15 : f64 to f32
    %c0_4 = arith.constant 0 : index
    %17 = memref.load %1[%c0_4] : memref<1xi16>
    %18 = arith.sitofp %17 : i16 to f16
    %19 = arith.extf %18 : f16 to f64
    %c0_5 = arith.constant 0 : index
    %20 = memref.load %2[%c0_5] : memref<1xi32>
    %21 = arith.sitofp %20 : i32 to f32
    %22 = arith.extf %21 : f32 to f64
    %c0_6 = arith.constant 0 : index
    %23 = memref.load %3[%c0_6] : memref<1xi64>
    %24 = arith.sitofp %23 : i64 to f64
    %c0_7 = arith.constant 0 : index
    %25 = memref.load %4[%c0_7] : memref<1xf32>
    %26 = arith.extf %25 : f32 to f64
    %c0_8 = arith.constant 0 : index
    %27 = memref.load %5[%c0_8] : memref<1xf64>
    return
  }
}
      |]
