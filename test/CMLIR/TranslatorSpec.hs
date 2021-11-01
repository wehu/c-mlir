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

