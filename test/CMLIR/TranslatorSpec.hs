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
              Right ast -> translateToMLIR defaultOptions{simplize=False} ast
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi8>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi8>
    %c1_1 = arith.constant 1 : index
    %2 = memref.alloca(%c1_1) : memref<?xi16>
    %c1_2 = arith.constant 1 : index
    %3 = memref.alloca(%c1_2) : memref<?xi16>
    %c1_3 = arith.constant 1 : index
    %4 = memref.alloca(%c1_3) : memref<?xi32>
    %c1_4 = arith.constant 1 : index
    %5 = memref.alloca(%c1_4) : memref<?xi32>
    %c1_5 = arith.constant 1 : index
    %6 = memref.alloca(%c1_5) : memref<?xi64>
    %c1_6 = arith.constant 1 : index
    %7 = memref.alloca(%c1_6) : memref<?xi64>
    %c1_7 = arith.constant 1 : index
    %8 = memref.alloca(%c1_7) : memref<?xf32>
    %c1_8 = arith.constant 1 : index
    %9 = memref.alloca(%c1_8) : memref<?xf64>
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
  func @foo() attributes {llvm.emit_c_interface} {
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
#map0 = affine_map<() -> (2)>
#map1 = affine_map<() -> (1)>
#map2 = affine_map<() -> (3)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<4x5xi32>
    %c2_i32 = arith.constant 2 : i32
    %c1_i32 = arith.constant 1 : i32
    %1 = arith.index_cast %c2_i32 : i32 to index
    %2 = arith.index_cast %c1_i32 : i32 to index
    %3 = affine.apply #map0()
    %4 = affine.apply #map1()
    %5 = affine.load %0[%3, %4] : memref<4x5xi32>
    %c2_i32_0 = arith.constant 2 : i32
    %c3_i32 = arith.constant 3 : i32
    %6 = arith.index_cast %c2_i32_0 : i32 to index
    %7 = arith.index_cast %c3_i32 : i32 to index
    %8 = affine.apply #map0()
    %9 = affine.apply #map2()
    affine.store %5, %0[%8, %9] : memref<4x5xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c97_i8 = arith.constant 97 : i8
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi8>
    %c0 = arith.constant 0 : index
    affine.store %c97_i8, %0[%c0] : memref<?xi8>
    %c1_i32 = arith.constant 1 : i32
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    affine.store %c1_i32, %1[%c0_1] : memref<?xi32>
    %c1_i64 = arith.constant 1 : i64
    %c1_2 = arith.constant 1 : index
    %2 = memref.alloca(%c1_2) : memref<?xi64>
    %c0_3 = arith.constant 0 : index
    affine.store %c1_i64, %2[%c0_3] : memref<?xi64>
    %cst = arith.constant 1.000000e-01 : f32
    %c1_4 = arith.constant 1 : index
    %3 = memref.alloca(%c1_4) : memref<?xf32>
    %c0_5 = arith.constant 0 : index
    affine.store %cst, %3[%c0_5] : memref<?xf32>
    %cst_6 = arith.constant 1.000000e-01 : f64
    %c1_7 = arith.constant 1 : index
    %4 = memref.alloca(%c1_7) : memref<?xf64>
    %c0_8 = arith.constant 0 : index
    affine.store %cst_6, %4[%c0_8] : memref<?xf64>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.addi %2, %3 : i32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xi32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xi32>
    %7 = arith.subi %5, %6 : i32
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xi32>
    %10 = arith.muli %8, %9 : i32
    %c0_6 = arith.constant 0 : index
    %11 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %12 = affine.load %1[%c0_7] : memref<?xi32>
    %13 = arith.divsi %11, %12 : i32
    %c0_8 = arith.constant 0 : index
    %14 = affine.load %0[%c0_8] : memref<?xi32>
    %c0_9 = arith.constant 0 : index
    %15 = affine.load %1[%c0_9] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.addi %2, %3 : i32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xi32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xi32>
    %7 = arith.subi %5, %6 : i32
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xi32>
    %10 = arith.muli %8, %9 : i32
    %c0_6 = arith.constant 0 : index
    %11 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %12 = affine.load %1[%c0_7] : memref<?xi32>
    %13 = arith.divui %11, %12 : i32
    %c0_8 = arith.constant 0 : index
    %14 = affine.load %0[%c0_8] : memref<?xi32>
    %c0_9 = arith.constant 0 : index
    %15 = affine.load %1[%c0_9] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xf32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xf32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xf32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xf32>
    %4 = arith.addf %2, %3 : f32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xf32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xf32>
    %7 = arith.subf %5, %6 : f32
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %0[%c0_4] : memref<?xf32>
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xf32>
    %10 = arith.mulf %8, %9 : f32
    %c0_6 = arith.constant 0 : index
    %11 = affine.load %0[%c0_6] : memref<?xf32>
    %c0_7 = arith.constant 0 : index
    %12 = affine.load %1[%c0_7] : memref<?xf32>
    %13 = arith.divf %11, %12 : f32
    %c0_8 = arith.constant 0 : index
    %14 = affine.load %0[%c0_8] : memref<?xf32>
    %c0_9 = arith.constant 0 : index
    %15 = affine.load %1[%c0_9] : memref<?xf32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xi32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xi32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xi32>
    %10 = arith.cmpi sgt, %8, %9 : i32
    %c0_6 = arith.constant 0 : index
    %11 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %12 = affine.load %1[%c0_7] : memref<?xi32>
    %13 = arith.cmpi slt, %11, %12 : i32
    %c0_8 = arith.constant 0 : index
    %14 = affine.load %0[%c0_8] : memref<?xi32>
    %c0_9 = arith.constant 0 : index
    %15 = affine.load %1[%c0_9] : memref<?xi32>
    %16 = arith.cmpi sge, %14, %15 : i32
    %c0_10 = arith.constant 0 : index
    %17 = affine.load %0[%c0_10] : memref<?xi32>
    %c0_11 = arith.constant 0 : index
    %18 = affine.load %1[%c0_11] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xi32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xi32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xi32>
    %10 = arith.cmpi ugt, %8, %9 : i32
    %c0_6 = arith.constant 0 : index
    %11 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %12 = affine.load %1[%c0_7] : memref<?xi32>
    %13 = arith.cmpi ult, %11, %12 : i32
    %c0_8 = arith.constant 0 : index
    %14 = affine.load %0[%c0_8] : memref<?xi32>
    %c0_9 = arith.constant 0 : index
    %15 = affine.load %1[%c0_9] : memref<?xi32>
    %16 = arith.cmpi uge, %14, %15 : i32
    %c0_10 = arith.constant 0 : index
    %17 = affine.load %0[%c0_10] : memref<?xi32>
    %c0_11 = arith.constant 0 : index
    %18 = affine.load %1[%c0_11] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %0[%c0_2] : memref<?xi32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %1[%c0_3] : memref<?xi32>
    %7 = arith.cmpi eq, %5, %6 : i32
    %8 = arith.andi %4, %7 : i1
    %c0_4 = arith.constant 0 : index
    %9 = affine.load %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %10 = affine.load %1[%c0_5] : memref<?xi32>
    %11 = arith.cmpi eq, %9, %10 : i32
    %c0_6 = arith.constant 0 : index
    %12 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %13 = affine.load %1[%c0_7] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    %c0_0 = arith.constant 0 : index
    affine.store %2, %0[%c0_0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %0[%c0_1] : memref<?xi32>
    %c1_i32_2 = arith.constant 1 : i32
    %4 = arith.subi %3, %c1_i32_2 : i32
    %c0_3 = arith.constant 0 : index
    affine.store %4, %0[%c0_3] : memref<?xi32>
    %c0_4 = arith.constant 0 : index
    %5 = affine.load %0[%c0_4] : memref<?xi32>
    %c1_i32_5 = arith.constant 1 : i32
    %6 = arith.muli %5, %c1_i32_5 : i32
    %c0_6 = arith.constant 0 : index
    affine.store %6, %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %7 = affine.load %0[%c0_7] : memref<?xi32>
    %c1_i32_8 = arith.constant 1 : i32
    %8 = arith.divsi %7, %c1_i32_8 : i32
    %c0_9 = arith.constant 0 : index
    affine.store %8, %0[%c0_9] : memref<?xi32>
    %c0_10 = arith.constant 0 : index
    %9 = affine.load %0[%c0_10] : memref<?xi32>
    %c1_i32_11 = arith.constant 1 : i32
    %10 = arith.remsi %9, %c1_i32_11 : i32
    %c0_12 = arith.constant 0 : index
    affine.store %10, %0[%c0_12] : memref<?xi32>
    %c0_13 = arith.constant 0 : index
    %11 = affine.load %0[%c0_13] : memref<?xi32>
    %c1_i32_14 = arith.constant 1 : i32
    %12 = arith.andi %11, %c1_i32_14 : i32
    %c0_15 = arith.constant 0 : index
    affine.store %12, %0[%c0_15] : memref<?xi32>
    %c0_16 = arith.constant 0 : index
    %13 = affine.load %0[%c0_16] : memref<?xi32>
    %c1_i32_17 = arith.constant 1 : i32
    %14 = arith.ori %13, %c1_i32_17 : i32
    %c0_18 = arith.constant 0 : index
    affine.store %14, %0[%c0_18] : memref<?xi32>
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
  func @foo(%arg0: i32, %arg1: f32) attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
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
  func @bar(%arg0: i32) -> i32 attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
    return %arg0 : i32
  }
  func @foo(%arg0: i32, %arg1: f32) attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = call @bar(%arg0) : (i32) -> i32
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
  func @foo(%arg0: i32, %arg1: f32) attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = call @bar(%arg0) : (i32) -> i32
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
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
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (10)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = affine.apply #map0()
    %1 = affine.apply #map1()
    affine.for %arg0 = #map2(%0) to #map2(%1) step 2 {
      %2 = arith.index_cast %arg0 : index to i32
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %0[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %1 = affine.load %0[%c0_0] : memref<?xi32>
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
    
    it "can translate any for loop to scf while" $ do
      [r|
void foo() {
  for (int i=0; i>2; i+=1) {
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %0[%c0] : memref<?xi32>
    scf.while : () -> () {
      %c0_0 = arith.constant 0 : index
      %1 = affine.load %0[%c0_0] : memref<?xi32>
      %c2_i32 = arith.constant 2 : i32
      %2 = arith.cmpi sgt, %1, %c2_i32 : i32
      scf.condition(%2)
    } do {
      %c0_0 = arith.constant 0 : index
      %1 = affine.load %0[%c0_0] : memref<?xi32>
      %c1_i32 = arith.constant 1 : i32
      %2 = arith.addi %1, %c1_i32 : i32
      %c0_1 = arith.constant 0 : index
      affine.store %2, %0[%c0_1] : memref<?xi32>
      scf.yield
    }
    return
  }
}
      |]

    it "can translate while to scf while" $ do
      [r|
void foo() {
  int i =0;
  while (i< 10) {
    i += 1;
  }
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %0[%c0] : memref<?xi32>
    scf.while : () -> () {
      %c0_0 = arith.constant 0 : index
      %1 = affine.load %0[%c0_0] : memref<?xi32>
      %c10_i32 = arith.constant 10 : i32
      %2 = arith.cmpi slt, %1, %c10_i32 : i32
      scf.condition(%2)
    } do {
      %c0_0 = arith.constant 0 : index
      %1 = affine.load %0[%c0_0] : memref<?xi32>
      %c1_i32 = arith.constant 1 : i32
      %2 = arith.addi %1, %c1_i32 : i32
      %c0_1 = arith.constant 0 : index
      affine.store %2, %0[%c0_1] : memref<?xi32>
      scf.yield
    }
    return
  }
}
      |]

    it "can translate do-while to scf while" $ do
      [r|
void foo() {
  int i =0;
  do {
    i += 1;
  } while (i<10);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %0[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %1 = affine.load %0[%c0_0] : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    %c0_1 = arith.constant 0 : index
    affine.store %2, %0[%c0_1] : memref<?xi32>
    scf.while : () -> () {
      %c0_2 = arith.constant 0 : index
      %3 = affine.load %0[%c0_2] : memref<?xi32>
      %c10_i32 = arith.constant 10 : i32
      %4 = arith.cmpi slt, %3, %c10_i32 : i32
      scf.condition(%4)
    } do {
      %c0_2 = arith.constant 0 : index
      %3 = affine.load %0[%c0_2] : memref<?xi32>
      %c1_i32_3 = arith.constant 1 : i32
      %4 = arith.addi %3, %c1_i32_3 : i32
      %c0_4 = arith.constant 0 : index
      affine.store %4, %0[%c0_4] : memref<?xi32>
      scf.yield
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
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
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (10)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %2 = affine.apply #map0()
    %3 = affine.apply #map1()
    affine.for %arg0 = #map2(%2) to #map2(%3) {
      %4 = arith.index_cast %arg0 : index to i32
      %c0 = arith.constant 0 : index
      %5 = affine.load %1[%c0] : memref<?xi32>
      %6 = arith.cmpi sgt, %4, %5 : i32
      scf.if %6 {
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
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (10)>
#map2 = affine_map<(d0) -> (d0)>
#map3 = affine_map<(d0) -> (0)>
#map4 = affine_map<(d0) -> (10)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = affine.apply #map0()
    %1 = affine.apply #map1()
    affine.for %arg0 = #map2(%0) to #map2(%1) {
      %2 = arith.index_cast %arg0 : index to i32
      %3 = affine.apply #map3(%arg0)
      %4 = affine.apply #map4(%arg0)
      affine.for %arg1 = #map2(%3) to #map2(%4) step 2 {
        %5 = arith.index_cast %arg1 : index to i32
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
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (10)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xi32>
    %4 = arith.cmpi ne, %2, %3 : i32
    scf.if %4 {
      %5 = affine.apply #map0()
      %6 = affine.apply #map1()
      affine.for %arg0 = #map2(%5) to #map2(%6) step 2 {
        %7 = arith.index_cast %arg0 : index to i32
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
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi8>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi16>
    %c1_1 = arith.constant 1 : index
    %2 = memref.alloca(%c1_1) : memref<?xi32>
    %c1_2 = arith.constant 1 : index
    %3 = memref.alloca(%c1_2) : memref<?xi64>
    %c1_3 = arith.constant 1 : index
    %4 = memref.alloca(%c1_3) : memref<?xf32>
    %c1_4 = arith.constant 1 : index
    %5 = memref.alloca(%c1_4) : memref<?xf64>
    %c0 = arith.constant 0 : index
    %6 = affine.load %0[%c0] : memref<?xi8>
    %c0_5 = arith.constant 0 : index
    %7 = affine.load %1[%c0_5] : memref<?xi16>
    %8 = arith.trunci %7 : i16 to i8
    %c0_6 = arith.constant 0 : index
    %9 = affine.load %2[%c0_6] : memref<?xi32>
    %10 = arith.trunci %9 : i32 to i8
    %c0_7 = arith.constant 0 : index
    %11 = affine.load %3[%c0_7] : memref<?xi64>
    %12 = arith.trunci %11 : i64 to i8
    %c0_8 = arith.constant 0 : index
    %13 = affine.load %4[%c0_8] : memref<?xf32>
    %14 = arith.fptosi %13 : f32 to i32
    %15 = arith.trunci %14 : i32 to i8
    %c0_9 = arith.constant 0 : index
    %16 = affine.load %5[%c0_9] : memref<?xf64>
    %17 = arith.fptosi %16 : f64 to i64
    %18 = arith.trunci %17 : i64 to i8
    %c0_10 = arith.constant 0 : index
    %19 = affine.load %0[%c0_10] : memref<?xi8>
    %20 = arith.extsi %19 : i8 to i16
    %c0_11 = arith.constant 0 : index
    %21 = affine.load %1[%c0_11] : memref<?xi16>
    %c0_12 = arith.constant 0 : index
    %22 = affine.load %2[%c0_12] : memref<?xi32>
    %23 = arith.trunci %22 : i32 to i16
    %c0_13 = arith.constant 0 : index
    %24 = affine.load %3[%c0_13] : memref<?xi64>
    %25 = arith.trunci %24 : i64 to i16
    %c0_14 = arith.constant 0 : index
    %26 = affine.load %4[%c0_14] : memref<?xf32>
    %27 = arith.fptosi %26 : f32 to i32
    %28 = arith.trunci %27 : i32 to i16
    %c0_15 = arith.constant 0 : index
    %29 = affine.load %5[%c0_15] : memref<?xf64>
    %30 = arith.fptosi %29 : f64 to i64
    %31 = arith.trunci %30 : i64 to i16
    %c0_16 = arith.constant 0 : index
    %32 = affine.load %0[%c0_16] : memref<?xi8>
    %33 = arith.extsi %32 : i8 to i32
    %c0_17 = arith.constant 0 : index
    %34 = affine.load %1[%c0_17] : memref<?xi16>
    %35 = arith.extsi %34 : i16 to i32
    %c0_18 = arith.constant 0 : index
    %36 = affine.load %2[%c0_18] : memref<?xi32>
    %c0_19 = arith.constant 0 : index
    %37 = affine.load %3[%c0_19] : memref<?xi64>
    %38 = arith.trunci %37 : i64 to i32
    %c0_20 = arith.constant 0 : index
    %39 = affine.load %4[%c0_20] : memref<?xf32>
    %40 = arith.fptosi %39 : f32 to i32
    %c0_21 = arith.constant 0 : index
    %41 = affine.load %5[%c0_21] : memref<?xf64>
    %42 = arith.fptosi %41 : f64 to i64
    %43 = arith.trunci %42 : i64 to i32
    %c0_22 = arith.constant 0 : index
    %44 = affine.load %0[%c0_22] : memref<?xi8>
    %45 = arith.extsi %44 : i8 to i64
    %c0_23 = arith.constant 0 : index
    %46 = affine.load %1[%c0_23] : memref<?xi16>
    %47 = arith.extsi %46 : i16 to i64
    %c0_24 = arith.constant 0 : index
    %48 = affine.load %2[%c0_24] : memref<?xi32>
    %49 = arith.extsi %48 : i32 to i64
    %c0_25 = arith.constant 0 : index
    %50 = affine.load %3[%c0_25] : memref<?xi64>
    %c0_26 = arith.constant 0 : index
    %51 = affine.load %4[%c0_26] : memref<?xf32>
    %52 = arith.fptosi %51 : f32 to i32
    %53 = arith.extsi %52 : i32 to i64
    %c0_27 = arith.constant 0 : index
    %54 = affine.load %5[%c0_27] : memref<?xf64>
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
  (float)v0;
  (float)v1;
  (float)v2;
  (float)v3;
  (float)v4;
  (float)v5;
  (double)v0;
  (double)v1;
  (double)v2;
  (double)v3;
  (double)v4;
  (double)v5;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi8>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi16>
    %c1_1 = arith.constant 1 : index
    %2 = memref.alloca(%c1_1) : memref<?xi32>
    %c1_2 = arith.constant 1 : index
    %3 = memref.alloca(%c1_2) : memref<?xi64>
    %c1_3 = arith.constant 1 : index
    %4 = memref.alloca(%c1_3) : memref<?xf32>
    %c1_4 = arith.constant 1 : index
    %5 = memref.alloca(%c1_4) : memref<?xf64>
    %c0 = arith.constant 0 : index
    %6 = affine.load %0[%c0] : memref<?xi8>
    %7 = arith.extsi %6 : i8 to i32
    %8 = arith.sitofp %7 : i32 to f32
    %c0_5 = arith.constant 0 : index
    %9 = affine.load %1[%c0_5] : memref<?xi16>
    %10 = arith.extsi %9 : i16 to i32
    %11 = arith.sitofp %10 : i32 to f32
    %c0_6 = arith.constant 0 : index
    %12 = affine.load %2[%c0_6] : memref<?xi32>
    %13 = arith.sitofp %12 : i32 to f32
    %c0_7 = arith.constant 0 : index
    %14 = affine.load %3[%c0_7] : memref<?xi64>
    %15 = arith.trunci %14 : i64 to i32
    %16 = arith.sitofp %15 : i32 to f32
    %c0_8 = arith.constant 0 : index
    %17 = affine.load %4[%c0_8] : memref<?xf32>
    %c0_9 = arith.constant 0 : index
    %18 = affine.load %5[%c0_9] : memref<?xf64>
    %19 = arith.truncf %18 : f64 to f32
    %c0_10 = arith.constant 0 : index
    %20 = affine.load %0[%c0_10] : memref<?xi8>
    %21 = arith.extsi %20 : i8 to i64
    %22 = arith.sitofp %21 : i64 to f64
    %c0_11 = arith.constant 0 : index
    %23 = affine.load %1[%c0_11] : memref<?xi16>
    %24 = arith.extsi %23 : i16 to i64
    %25 = arith.sitofp %24 : i64 to f64
    %c0_12 = arith.constant 0 : index
    %26 = affine.load %2[%c0_12] : memref<?xi32>
    %27 = arith.extsi %26 : i32 to i64
    %28 = arith.sitofp %27 : i64 to f64
    %c0_13 = arith.constant 0 : index
    %29 = affine.load %3[%c0_13] : memref<?xi64>
    %30 = arith.sitofp %29 : i64 to f64
    %c0_14 = arith.constant 0 : index
    %31 = affine.load %4[%c0_14] : memref<?xf32>
    %32 = arith.extf %31 : f32 to f64
    %c0_15 = arith.constant 0 : index
    %33 = affine.load %5[%c0_15] : memref<?xf64>
    return
  }
}
      |]

    it "can translate post/pre/inc/dec" $ do
      [r|
void foo() {
  int v0;
  ++v0;
  --v0;
  v0++;
  v0--;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    %c0_0 = arith.constant 0 : index
    affine.store %2, %0[%c0_0] : memref<?xi32>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %0[%c0_1] : memref<?xi32>
    %c0_2 = arith.constant 0 : index
    %4 = affine.load %0[%c0_2] : memref<?xi32>
    %c1_i32_3 = arith.constant 1 : i32
    %5 = arith.subi %4, %c1_i32_3 : i32
    %c0_4 = arith.constant 0 : index
    affine.store %5, %0[%c0_4] : memref<?xi32>
    %c0_5 = arith.constant 0 : index
    %6 = affine.load %0[%c0_5] : memref<?xi32>
    %c0_6 = arith.constant 0 : index
    %7 = affine.load %0[%c0_6] : memref<?xi32>
    %c0_7 = arith.constant 0 : index
    %8 = affine.load %0[%c0_7] : memref<?xi32>
    %c1_i32_8 = arith.constant 1 : i32
    %9 = arith.addi %8, %c1_i32_8 : i32
    %c0_9 = arith.constant 0 : index
    affine.store %9, %0[%c0_9] : memref<?xi32>
    %c0_10 = arith.constant 0 : index
    %10 = affine.load %0[%c0_10] : memref<?xi32>
    %c0_11 = arith.constant 0 : index
    %11 = affine.load %0[%c0_11] : memref<?xi32>
    %c1_i32_12 = arith.constant 1 : i32
    %12 = arith.subi %11, %c1_i32_12 : i32
    %c0_13 = arith.constant 0 : index
    affine.store %12, %0[%c0_13] : memref<?xi32>
    return
  }
}
      |]
    
    it "can translate plus/minus" $ do
      [r|
void foo() {
  int v0;
  +v0;
  -v0;
  float v1;
  +v1;
  -v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %2 = affine.load %0[%c0_0] : memref<?xi32>
    %c0_i32 = arith.constant 0 : i32
    %3 = arith.subi %c0_i32, %2 : i32
    %c1_1 = arith.constant 1 : index
    %4 = memref.alloca(%c1_1) : memref<?xf32>
    %c0_2 = arith.constant 0 : index
    %5 = affine.load %4[%c0_2] : memref<?xf32>
    %c0_3 = arith.constant 0 : index
    %6 = affine.load %4[%c0_3] : memref<?xf32>
    %7 = arith.negf %6 : f32
    return
  }
}
      |]
    
    it "can translate not" $ do
      [r|
void foo() {
  int v0;
  ! (v0 == v0);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %2 = affine.load %0[%c0_0] : memref<?xi32>
    %3 = arith.cmpi eq, %1, %2 : i32
    %false = arith.constant false
    %true = arith.constant true
    %4 = arith.subi %false, %true : i1
    %5 = arith.xori %4, %3 : i1
    return
  }
}
      |]
    
    it "can translate pointer" $ do
      [r|
void foo() {
  int *v0;
  *v0 + 1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi32>>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xmemref<?xi32>>
    %c0_0 = arith.constant 0 : index
    %2 = affine.load %1[%c0_0] : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %3 = arith.addi %2, %c1_i32 : i32
    return
  }
}
      |]

    it "can translate pointer index access" $ do
      [r|
void foo() {
  int *v0;
  v0[0] = v0[1];
}
      |] `shouldBeTranslatedAs` [r|
#map0 = affine_map<() -> (1)>
#map1 = affine_map<() -> (0)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi32>>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xmemref<?xi32>>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.index_cast %c1_i32 : i32 to index
    %3 = affine.apply #map0()
    %4 = affine.load %1[%3] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %5 = affine.load %0[%c0_0] : memref<?xmemref<?xi32>>
    %c0_i32 = arith.constant 0 : i32
    %6 = arith.index_cast %c0_i32 : i32 to index
    %7 = affine.apply #map1()
    affine.store %4, %5[%7] : memref<?xi32>
    return
  }
}
      |]

    it "can translate pointer deref assign" $ do
      [r|
void main() {
  int *a;
  *a = 1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @main() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi32>>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xmemref<?xi32>>
    %c1_i32 = arith.constant 1 : i32
    %c0_0 = arith.constant 0 : index
    affine.store %c1_i32, %1[%c0_0] : memref<?xi32>
    return
  }
}
      |]

    it "can translate pointer assign" $ do
      [r|
void foo() {
  char *a;
  char *b;
  a = b;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi8>>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xmemref<?xi8>>
    %c0 = arith.constant 0 : index
    %2 = affine.load %1[%c0] : memref<?xmemref<?xi8>>
    %c0_1 = arith.constant 0 : index
    affine.store %2, %0[%c0_1] : memref<?xmemref<?xi8>>
    return
  }
}
      |]
  
    it "can translate pointer casting" $ do
      [r|
void foo() {
  int *v0;
  (int[3])v0;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi32>>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xmemref<?xi32>>
    %2 = memref.cast %1 : memref<?xi32> to memref<3xi32>
    return
  }
}
      |]
    
    it "can translate i8 pointer casting" $ do
      [r|
void foo() {
  char *b;
  (int [8])b;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xmemref<?xi8>>
    %c0 = arith.constant 0 : index
    %1 = affine.load %0[%c0] : memref<?xmemref<?xi8>>
    %c0_0 = arith.constant 0 : index
    %2 = memref.view %1[%c0_0][] : memref<?xi8> to memref<8xi32>
    return
  }
}
      |]
    
    it "can translate enum" $ do
      [r|
enum test {
  A,
  B
};

void foo(enum test a) {
  int v0 = A;
  v0 = B;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo(%arg0: i32) attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %1 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %1[%c0] : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %c0_0 = arith.constant 0 : index
    affine.store %c1_i32, %1[%c0_0] : memref<?xi32>
    return
  }
}
      |]
    
    it "can translate arg pointer access" $ do
      [r|
void foo(float* input) {
  float a[100];
  for (int i=0; i<100; i+=1) {
    input[i] = a[i];
    a[i] = input[i];
  }
}
      |] `shouldBeTranslatedAs` [r|
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (100)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo(%arg0: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<100xf32>
    %1 = affine.apply #map0()
    %2 = affine.apply #map1()
    affine.for %arg1 = #map2(%1) to #map2(%2) {
      %3 = arith.index_cast %arg1 : index to i32
      %4 = arith.index_cast %3 : i32 to index
      %5 = affine.apply #map2(%arg1)
      %6 = affine.load %0[%5] : memref<100xf32>
      %7 = arith.index_cast %3 : i32 to index
      %8 = affine.apply #map2(%arg1)
      affine.store %6, %arg0[%8] : memref<?xf32>
      %9 = arith.index_cast %3 : i32 to index
      %10 = affine.apply #map2(%arg1)
      %11 = affine.load %arg0[%10] : memref<?xf32>
      %12 = arith.index_cast %3 : i32 to index
      %13 = affine.apply #map2(%arg1)
      affine.store %11, %0[%13] : memref<100xf32>
    }
    return
  }
}
      |]

    it "can translate __kernel, __global and __local" $ do
      [r|
__kernel void foo(__global float* input, __local float *a) {
  for (int i=0; i<100; i+=1) {
    input[i] = a[i];
    a[i] = input[i];
  }
}
      |] `shouldBeTranslatedAs` [r|
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (100)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo(%arg0: memref<?xf32, 2>, %arg1: memref<?xf32, 1>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    %0 = affine.apply #map0()
    %1 = affine.apply #map1()
    affine.for %arg2 = #map2(%0) to #map2(%1) {
      %2 = arith.index_cast %arg2 : index to i32
      %3 = arith.index_cast %2 : i32 to index
      %4 = affine.apply #map2(%arg2)
      %5 = affine.load %arg1[%4] : memref<?xf32, 1>
      %6 = arith.index_cast %2 : i32 to index
      %7 = affine.apply #map2(%arg2)
      affine.store %5, %arg0[%7] : memref<?xf32, 2>
      %8 = arith.index_cast %2 : i32 to index
      %9 = affine.apply #map2(%arg2)
      %10 = affine.load %arg0[%9] : memref<?xf32, 2>
      %11 = arith.index_cast %2 : i32 to index
      %12 = affine.apply #map2(%arg2)
      affine.store %10, %arg1[%12] : memref<?xf32, 1>
    }
    return
  }
}
      |]
    
    it "can translate init list" $ do
      [r|
void foo() {
  int a[2] = {1,2};
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1_i32 = arith.constant 1 : i32
    %c2_i32 = arith.constant 2 : i32
    %0 = memref.alloca() : memref<2xi32>
    %c0 = arith.constant 0 : index
    affine.store %c1_i32, %0[%c0] : memref<2xi32>
    %c1 = arith.constant 1 : index
    affine.store %c2_i32, %0[%c1] : memref<2xi32>
    return
  }
}
      |]

    it "can translate malloc/free" $ do
      [r|
void foo() {
  char *v = malloc(10);
  free(v);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c10_i32 = arith.constant 10 : i32
    %0 = arith.index_cast %c10_i32 : i32 to index
    %1 = memref.alloc(%0) : memref<?xi8>
    %c1 = arith.constant 1 : index
    %2 = memref.alloca(%c1) : memref<?xmemref<?xi8>>
    %c0 = arith.constant 0 : index
    affine.store %1, %2[%c0] : memref<?xmemref<?xi8>>
    %c0_0 = arith.constant 0 : index
    %3 = affine.load %2[%c0_0] : memref<?xmemref<?xi8>>
    memref.dealloc %3 : memref<?xi8>
    return
  }
}
      |]
    
    it "can translate array to pointer casting" $ do
      [r|
void foo() {
  char b[10];
  char *c = (char *)b;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<10xi8>
    %c0 = arith.constant 0 : index
    %1 = memref.dim %0, %c0 : memref<10xi8>
    %c0_0 = arith.constant 0 : index
    %2 = memref.view %0[%c0_0][%1] : memref<10xi8> to memref<?xi8>
    %c1 = arith.constant 1 : index
    %3 = memref.alloca(%c1) : memref<?xmemref<?xi8>>
    %c0_1 = arith.constant 0 : index
    affine.store %2, %3[%c0_1] : memref<?xmemref<?xi8>>
    return
  }
}
      |]

    it "can translate for inductions with same names" $ do
      [r|
void foo(int i) {
  int a[10];
  a[i] = 1;
  for (int i=0; i<10; ++i) {
    a[i] = 2;
    for (int i=0; i<10; ++i) {
      a[i] = 3;
    }
  }
}
      |] `shouldBeTranslatedAs` [r|
#map0 = affine_map<()[s0] -> (s0)>
#map1 = affine_map<()[s0] -> (0)>
#map2 = affine_map<()[s0] -> (10)>
#map3 = affine_map<(d0) -> (d0)>
#map4 = affine_map<(d0)[s0] -> (d0)>
#map5 = affine_map<(d0)[s0] -> (0)>
#map6 = affine_map<(d0)[s0] -> (10)>
#map7 = affine_map<(d0)[s0] -> (d1)>
module  {
  func @foo(%arg0: i32) attributes {llvm.emit_c_interface} {
    %0 = arith.index_cast %arg0 : i32 to index
    %1 = memref.alloca() : memref<10xi32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.index_cast %arg0 : i32 to index
    %3 = affine.apply #map0()[%0]
    affine.store %c1_i32, %1[%3] : memref<10xi32>
    %4 = affine.apply #map1()[%0]
    %5 = affine.apply #map2()[%0]
    affine.for %arg1 = #map3(%4) to #map3(%5) {
      %6 = arith.index_cast %arg1 : index to i32
      %c2_i32 = arith.constant 2 : i32
      %7 = arith.index_cast %6 : i32 to index
      %8 = affine.apply #map4(%arg1)[%0]
      affine.store %c2_i32, %1[%8] : memref<10xi32>
      %9 = affine.apply #map5(%arg1)[%0]
      %10 = affine.apply #map6(%arg1)[%0]
      affine.for %arg2 = #map3(%9) to #map3(%10) {
        %11 = arith.index_cast %arg2 : index to i32
        %c3_i32 = arith.constant 3 : i32
        %12 = arith.index_cast %11 : i32 to index
        %13 = affine.apply #map7(%arg2)[%0]
        affine.store %c3_i32, %1[%13] : memref<10xi32>
      }
    }
    return
  }
}
      |]

    it "can translate nested initialization" $ do
      [r|
void foo() {
  float lhs[1][3] = {{1.0, 2.0, 3.0}};
  int rhs[2][3] = {{1,2,3},{4,5,6}};
  int output[2][3][2] = {{{1,2},{3,4},{5,6}},{{7,8},{9,10},{11,12}}};
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %cst = arith.constant 1.000000e+00 : f32
    %cst_0 = arith.constant 2.000000e+00 : f32
    %cst_1 = arith.constant 3.000000e+00 : f32
    %0 = memref.alloca() : memref<1x3xf32>
    %c0 = arith.constant 0 : index
    %c0_2 = arith.constant 0 : index
    affine.store %cst, %0[%c0, %c0_2] : memref<1x3xf32>
    %c0_3 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    affine.store %cst_0, %0[%c0_3, %c1] : memref<1x3xf32>
    %c0_4 = arith.constant 0 : index
    %c2 = arith.constant 2 : index
    affine.store %cst_1, %0[%c0_4, %c2] : memref<1x3xf32>
    %c1_i32 = arith.constant 1 : i32
    %c2_i32 = arith.constant 2 : i32
    %c3_i32 = arith.constant 3 : i32
    %c4_i32 = arith.constant 4 : i32
    %c5_i32 = arith.constant 5 : i32
    %c6_i32 = arith.constant 6 : i32
    %1 = memref.alloca() : memref<2x3xi32>
    %c0_5 = arith.constant 0 : index
    %c0_6 = arith.constant 0 : index
    affine.store %c1_i32, %1[%c0_5, %c0_6] : memref<2x3xi32>
    %c0_7 = arith.constant 0 : index
    %c1_8 = arith.constant 1 : index
    affine.store %c2_i32, %1[%c0_7, %c1_8] : memref<2x3xi32>
    %c0_9 = arith.constant 0 : index
    %c2_10 = arith.constant 2 : index
    affine.store %c3_i32, %1[%c0_9, %c2_10] : memref<2x3xi32>
    %c1_11 = arith.constant 1 : index
    %c0_12 = arith.constant 0 : index
    affine.store %c4_i32, %1[%c1_11, %c0_12] : memref<2x3xi32>
    %c1_13 = arith.constant 1 : index
    %c1_14 = arith.constant 1 : index
    affine.store %c5_i32, %1[%c1_13, %c1_14] : memref<2x3xi32>
    %c1_15 = arith.constant 1 : index
    %c2_16 = arith.constant 2 : index
    affine.store %c6_i32, %1[%c1_15, %c2_16] : memref<2x3xi32>
    %c1_i32_17 = arith.constant 1 : i32
    %c2_i32_18 = arith.constant 2 : i32
    %c3_i32_19 = arith.constant 3 : i32
    %c4_i32_20 = arith.constant 4 : i32
    %c5_i32_21 = arith.constant 5 : i32
    %c6_i32_22 = arith.constant 6 : i32
    %c7_i32 = arith.constant 7 : i32
    %c8_i32 = arith.constant 8 : i32
    %c9_i32 = arith.constant 9 : i32
    %c10_i32 = arith.constant 10 : i32
    %c11_i32 = arith.constant 11 : i32
    %c12_i32 = arith.constant 12 : i32
    %2 = memref.alloca() : memref<2x3x2xi32>
    %c0_23 = arith.constant 0 : index
    %c0_24 = arith.constant 0 : index
    %c0_25 = arith.constant 0 : index
    affine.store %c1_i32_17, %2[%c0_23, %c0_24, %c0_25] : memref<2x3x2xi32>
    %c0_26 = arith.constant 0 : index
    %c0_27 = arith.constant 0 : index
    %c1_28 = arith.constant 1 : index
    affine.store %c2_i32_18, %2[%c0_26, %c0_27, %c1_28] : memref<2x3x2xi32>
    %c0_29 = arith.constant 0 : index
    %c1_30 = arith.constant 1 : index
    %c0_31 = arith.constant 0 : index
    affine.store %c3_i32_19, %2[%c0_29, %c1_30, %c0_31] : memref<2x3x2xi32>
    %c0_32 = arith.constant 0 : index
    %c1_33 = arith.constant 1 : index
    %c1_34 = arith.constant 1 : index
    affine.store %c4_i32_20, %2[%c0_32, %c1_33, %c1_34] : memref<2x3x2xi32>
    %c0_35 = arith.constant 0 : index
    %c2_36 = arith.constant 2 : index
    %c0_37 = arith.constant 0 : index
    affine.store %c5_i32_21, %2[%c0_35, %c2_36, %c0_37] : memref<2x3x2xi32>
    %c0_38 = arith.constant 0 : index
    %c2_39 = arith.constant 2 : index
    %c1_40 = arith.constant 1 : index
    affine.store %c6_i32_22, %2[%c0_38, %c2_39, %c1_40] : memref<2x3x2xi32>
    %c1_41 = arith.constant 1 : index
    %c0_42 = arith.constant 0 : index
    %c0_43 = arith.constant 0 : index
    affine.store %c7_i32, %2[%c1_41, %c0_42, %c0_43] : memref<2x3x2xi32>
    %c1_44 = arith.constant 1 : index
    %c0_45 = arith.constant 0 : index
    %c1_46 = arith.constant 1 : index
    affine.store %c8_i32, %2[%c1_44, %c0_45, %c1_46] : memref<2x3x2xi32>
    %c1_47 = arith.constant 1 : index
    %c1_48 = arith.constant 1 : index
    %c0_49 = arith.constant 0 : index
    affine.store %c9_i32, %2[%c1_47, %c1_48, %c0_49] : memref<2x3x2xi32>
    %c1_50 = arith.constant 1 : index
    %c1_51 = arith.constant 1 : index
    %c1_52 = arith.constant 1 : index
    affine.store %c10_i32, %2[%c1_50, %c1_51, %c1_52] : memref<2x3x2xi32>
    %c1_53 = arith.constant 1 : index
    %c2_54 = arith.constant 2 : index
    %c0_55 = arith.constant 0 : index
    affine.store %c11_i32, %2[%c1_53, %c2_54, %c0_55] : memref<2x3x2xi32>
    %c1_56 = arith.constant 1 : index
    %c2_57 = arith.constant 2 : index
    %c1_58 = arith.constant 1 : index
    affine.store %c12_i32, %2[%c1_56, %c2_57, %c1_58] : memref<2x3x2xi32>
    return
  }
}
      |]

    it "can translate const variable decls" $ do
      [r|
void foo() {
  const int v0;
  const int v1 = 1;
  const int v2[2];
  const int v3[2] = {1,2};
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_i32 = arith.constant 1 : i32
    %1 = arith.index_cast %c1_i32 : i32 to index
    %2 = memref.alloca() : memref<2xi32>
    %c1_i32_0 = arith.constant 1 : i32
    %c2_i32 = arith.constant 2 : i32
    %3 = memref.alloca() : memref<2xi32>
    %c0 = arith.constant 0 : index
    affine.store %c1_i32_0, %3[%c0] : memref<2xi32>
    %c1_1 = arith.constant 1 : index
    affine.store %c2_i32, %3[%c1_1] : memref<2xi32>
    return
  }
}
      |]
    
    it "can translate memcpy" $ do
      [r|
void foo() {
  int src[2];
  int dst[2];
  memcpy(dst, src);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<2xi32>
    %1 = memref.alloca() : memref<2xi32>
    memref.copy %0, %1 : memref<2xi32> to memref<2xi32>
    return
  }
}
      |]
    
    it "can translate dma_start" $ do
      [r|
void foo() {
  int src[2];
  int dst[2];
  int tag[1];
  dma_start(src[1], dst[1], tag[1], 1);
}
      |] `shouldBeTranslatedAs` [r|
#map = affine_map<() -> (1)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<2xi32>
    %1 = memref.alloca() : memref<2xi32>
    %2 = memref.alloca() : memref<1xi32>
    %3 = affine.apply #map()
    %4 = affine.apply #map()
    %5 = affine.apply #map()
    %c1_i32 = arith.constant 1 : i32
    %6 = arith.index_cast %c1_i32 : i32 to index
    affine.dma_start %0[%3], %1[%4], %2[%5], %6 : memref<2xi32>, memref<2xi32>, memref<1xi32>
    return
  }
}
      |]
    
    it "can translate dma_wait" $ do
      [r|
void foo() {
  int tag[1];
  dma_wait(tag[1], 1);
}
      |] `shouldBeTranslatedAs` [r|
#map = affine_map<() -> (1)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<1xi32>
    %1 = affine.apply #map()
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.index_cast %c1_i32 : i32 to index
    affine.dma_wait %0[%1], %2 : memref<1xi32>
    return
  }
}
      |]

    it "can translate any dma_start" $ do
      [r|
void foo() {
  int src[2];
  int dst[2];
  int tag[1];
  int index = 1;
  dma_start(src[index], dst[1], tag[1], 1);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<2xi32>
    %1 = memref.alloca() : memref<2xi32>
    %2 = memref.alloca() : memref<1xi32>
    %c1_i32 = arith.constant 1 : i32
    %c1 = arith.constant 1 : index
    %3 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c1_i32, %3[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %4 = affine.load %3[%c0_0] : memref<?xi32>
    %5 = arith.index_cast %4 : i32 to index
    %c1_i32_1 = arith.constant 1 : i32
    %6 = arith.index_cast %c1_i32_1 : i32 to index
    %c1_i32_2 = arith.constant 1 : i32
    %7 = arith.index_cast %c1_i32_2 : i32 to index
    %c1_i32_3 = arith.constant 1 : i32
    %8 = arith.index_cast %c1_i32_3 : i32 to index
    memref.dma_start %0[%5], %1[%6], %8, %2[%7] : memref<2xi32>, memref<2xi32>, memref<1xi32>
    return
  }
}
      |]
    
    it "can translate any dma_wait" $ do
      [r|
void foo() {
  int tag[1];
  int index = 0;
  dma_wait(tag[index], 1);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<1xi32>
    %c0_i32 = arith.constant 0 : i32
    %c1 = arith.constant 1 : index
    %1 = memref.alloca(%c1) : memref<?xi32>
    %c0 = arith.constant 0 : index
    affine.store %c0_i32, %1[%c0] : memref<?xi32>
    %c0_0 = arith.constant 0 : index
    %2 = affine.load %1[%c0_0] : memref<?xi32>
    %3 = arith.index_cast %2 : i32 to index
    %c1_i32 = arith.constant 1 : i32
    %4 = arith.index_cast %c1_i32 : i32 to index
    memref.dma_wait %0[%3], %4 : memref<1xi32>
    return
  }
}
      |]
    
    it "can translate nested scope" $ do
      [r|
void foo() {
  int v0;
  int v1;
  {
    int v0;
    int v1;
    v1;
  }
  for (int i=0; i<10; ++i) {
    {
      int v1;
      v1;
    }
  }
}
      |] `shouldBeTranslatedAs` [r|
#map0 = affine_map<() -> (0)>
#map1 = affine_map<() -> (10)>
#map2 = affine_map<(d0) -> (d0)>
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xi32>
    memref.alloca_scope  {
      %c1_1 = arith.constant 1 : index
      %4 = memref.alloca(%c1_1) : memref<?xi32>
      %c1_2 = arith.constant 1 : index
      %5 = memref.alloca(%c1_2) : memref<?xi32>
      %c0 = arith.constant 0 : index
      %6 = affine.load %5[%c0] : memref<?xi32>
    }
    %2 = affine.apply #map0()
    %3 = affine.apply #map1()
    affine.for %arg0 = #map2(%2) to #map2(%3) {
      %4 = arith.index_cast %arg0 : index to i32
      memref.alloca_scope  {
        %c1_1 = arith.constant 1 : index
        %5 = memref.alloca(%c1_1) : memref<?xi32>
        %c0 = arith.constant 0 : index
        %6 = affine.load %5[%c0] : memref<?xi32>
      }
    }
    return
  }
}
      |]
    
    it "can translate address op" $ do
      [r|
void foo() {
  int v0;
  int *v1 = &v0;
  *v1;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xi32>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xmemref<?xi32>>
    %c0 = arith.constant 0 : index
    affine.store %0, %1[%c0] : memref<?xmemref<?xi32>>
    %c0_1 = arith.constant 0 : index
    %2 = affine.load %1[%c0_1] : memref<?xmemref<?xi32>>
    %c0_2 = arith.constant 0 : index
    %3 = affine.load %2[%c0_2] : memref<?xi32>
    return
  }
}
      |]

    it "can translate opencl vector type" $ do
      [r|
typedef char char2 __attribute__((__ext_vector_type__(2)));
typedef char char3 __attribute__((__ext_vector_type__(3)));
typedef char char4 __attribute__((__ext_vector_type__(4)));
typedef char char8 __attribute__((__ext_vector_type__(8)));
typedef char char16 __attribute__((__ext_vector_type__(16)));

typedef unsigned char uchar2 __attribute__((__ext_vector_type__(2)));
typedef unsigned char uchar3 __attribute__((__ext_vector_type__(3)));
typedef unsigned char uchar4 __attribute__((__ext_vector_type__(4)));
typedef unsigned char uchar8 __attribute__((__ext_vector_type__(8)));
typedef unsigned char uchar16 __attribute__((__ext_vector_type__(16)));

typedef short short2 __attribute__((__ext_vector_type__(2)));
typedef short short3 __attribute__((__ext_vector_type__(3)));
typedef short short4 __attribute__((__ext_vector_type__(4)));
typedef short short8 __attribute__((__ext_vector_type__(8)));
typedef short short16 __attribute__((__ext_vector_type__(16)));

typedef unsigned short ushort2 __attribute__((__ext_vector_type__(2)));
typedef unsigned short ushort3 __attribute__((__ext_vector_type__(3)));
typedef unsigned short ushort4 __attribute__((__ext_vector_type__(4)));
typedef unsigned short ushort8 __attribute__((__ext_vector_type__(8)));
typedef unsigned short ushort16 __attribute__((__ext_vector_type__(16)));

typedef int int2 __attribute__((__ext_vector_type__(2)));
typedef int int3 __attribute__((__ext_vector_type__(3)));
typedef int int4 __attribute__((__ext_vector_type__(4)));
typedef int int8 __attribute__((__ext_vector_type__(8)));
typedef int int16 __attribute__((__ext_vector_type__(16)));

typedef unsigned int uint2 __attribute__((__ext_vector_type__(2)));
typedef unsigned int uint3 __attribute__((__ext_vector_type__(3)));
typedef unsigned int uint4 __attribute__((__ext_vector_type__(4)));
typedef unsigned int uint8 __attribute__((__ext_vector_type__(8)));
typedef unsigned int uint16 __attribute__((__ext_vector_type__(16)));

typedef long long2 __attribute__((__ext_vector_type__(2)));
typedef long long3 __attribute__((__ext_vector_type__(3)));
typedef long long4 __attribute__((__ext_vector_type__(4)));
typedef long long8 __attribute__((__ext_vector_type__(8)));
typedef long long16 __attribute__((__ext_vector_type__(16)));

typedef unsigned long ulong2 __attribute__((__ext_vector_type__(2)));
typedef unsigned long ulong3 __attribute__((__ext_vector_type__(3)));
typedef unsigned long ulong4 __attribute__((__ext_vector_type__(4)));
typedef unsigned long ulong8 __attribute__((__ext_vector_type__(8)));
typedef unsigned long ulong16 __attribute__((__ext_vector_type__(16)));

typedef float float2 __attribute__((__ext_vector_type__(2)));
typedef float float3 __attribute__((__ext_vector_type__(3)));
typedef float float4 __attribute__((__ext_vector_type__(4)));
typedef float float8 __attribute__((__ext_vector_type__(8)));
typedef float float16 __attribute__((__ext_vector_type__(16)));

typedef double double2 __attribute__((__ext_vector_type__(2)));
typedef double double3 __attribute__((__ext_vector_type__(3)));
typedef double double4 __attribute__((__ext_vector_type__(4)));
typedef double double8 __attribute__((__ext_vector_type__(8)));
typedef double double16 __attribute__((__ext_vector_type__(16)));

void foo() {
  char2 v0;
  char4 v1;
  char8 v2;
  char16 v3;
  uchar2 v4;
  uchar4 v5;
  uchar8 v6;
  uchar16 v7;

  short2 v9;
  short4 v10;
  short8 v11;
  short16 v12;
  ushort2 v13;
  ushort4 v14;
  ushort8 v15;
  ushort16 v16;

  int2 v17;
  int4 v18;
  int8 v19;
  int16 v20;
  uint2 v21;
  uint4 v22;
  uint8 v23;
  uint16 v24;

  long2 v25;
  long4 v26;
  long8 v27;
  long16 v28;
  ulong2 v29;
  ulong4 v30;
  ulong8 v31;
  ulong16 v32;

  float2 v33;
  float4 v34;
  float8 v35;
  float16 v36;
  double2 v37;
  double4 v38;
  double8 v39;
  double16 v40;

  char3 v41;
  uchar3 v42;
  short3 v43;
  ushort3 v44;
  int3 v45;
  uint3 v46;
  long3 v47;
  ulong3 v48;
  float3 v49;
  double3 v50;

}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xvector<2xi8>>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xvector<4xi8>>
    %c1_1 = arith.constant 1 : index
    %2 = memref.alloca(%c1_1) : memref<?xvector<8xi8>>
    %c1_2 = arith.constant 1 : index
    %3 = memref.alloca(%c1_2) : memref<?xvector<16xi8>>
    %c1_3 = arith.constant 1 : index
    %4 = memref.alloca(%c1_3) : memref<?xvector<2xi8>>
    %c1_4 = arith.constant 1 : index
    %5 = memref.alloca(%c1_4) : memref<?xvector<4xi8>>
    %c1_5 = arith.constant 1 : index
    %6 = memref.alloca(%c1_5) : memref<?xvector<8xi8>>
    %c1_6 = arith.constant 1 : index
    %7 = memref.alloca(%c1_6) : memref<?xvector<16xi8>>
    %c1_7 = arith.constant 1 : index
    %8 = memref.alloca(%c1_7) : memref<?xvector<2xi16>>
    %c1_8 = arith.constant 1 : index
    %9 = memref.alloca(%c1_8) : memref<?xvector<4xi16>>
    %c1_9 = arith.constant 1 : index
    %10 = memref.alloca(%c1_9) : memref<?xvector<8xi16>>
    %c1_10 = arith.constant 1 : index
    %11 = memref.alloca(%c1_10) : memref<?xvector<16xi16>>
    %c1_11 = arith.constant 1 : index
    %12 = memref.alloca(%c1_11) : memref<?xvector<2xi16>>
    %c1_12 = arith.constant 1 : index
    %13 = memref.alloca(%c1_12) : memref<?xvector<4xi16>>
    %c1_13 = arith.constant 1 : index
    %14 = memref.alloca(%c1_13) : memref<?xvector<8xi16>>
    %c1_14 = arith.constant 1 : index
    %15 = memref.alloca(%c1_14) : memref<?xvector<16xi16>>
    %c1_15 = arith.constant 1 : index
    %16 = memref.alloca(%c1_15) : memref<?xvector<2xi32>>
    %c1_16 = arith.constant 1 : index
    %17 = memref.alloca(%c1_16) : memref<?xvector<4xi32>>
    %c1_17 = arith.constant 1 : index
    %18 = memref.alloca(%c1_17) : memref<?xvector<8xi32>>
    %c1_18 = arith.constant 1 : index
    %19 = memref.alloca(%c1_18) : memref<?xvector<16xi32>>
    %c1_19 = arith.constant 1 : index
    %20 = memref.alloca(%c1_19) : memref<?xvector<2xi32>>
    %c1_20 = arith.constant 1 : index
    %21 = memref.alloca(%c1_20) : memref<?xvector<4xi32>>
    %c1_21 = arith.constant 1 : index
    %22 = memref.alloca(%c1_21) : memref<?xvector<8xi32>>
    %c1_22 = arith.constant 1 : index
    %23 = memref.alloca(%c1_22) : memref<?xvector<16xi32>>
    %c1_23 = arith.constant 1 : index
    %24 = memref.alloca(%c1_23) : memref<?xvector<2xi64>>
    %c1_24 = arith.constant 1 : index
    %25 = memref.alloca(%c1_24) : memref<?xvector<4xi64>>
    %c1_25 = arith.constant 1 : index
    %26 = memref.alloca(%c1_25) : memref<?xvector<8xi64>>
    %c1_26 = arith.constant 1 : index
    %27 = memref.alloca(%c1_26) : memref<?xvector<16xi64>>
    %c1_27 = arith.constant 1 : index
    %28 = memref.alloca(%c1_27) : memref<?xvector<2xi64>>
    %c1_28 = arith.constant 1 : index
    %29 = memref.alloca(%c1_28) : memref<?xvector<4xi64>>
    %c1_29 = arith.constant 1 : index
    %30 = memref.alloca(%c1_29) : memref<?xvector<8xi64>>
    %c1_30 = arith.constant 1 : index
    %31 = memref.alloca(%c1_30) : memref<?xvector<16xi64>>
    %c1_31 = arith.constant 1 : index
    %32 = memref.alloca(%c1_31) : memref<?xvector<2xf32>>
    %c1_32 = arith.constant 1 : index
    %33 = memref.alloca(%c1_32) : memref<?xvector<4xf32>>
    %c1_33 = arith.constant 1 : index
    %34 = memref.alloca(%c1_33) : memref<?xvector<8xf32>>
    %c1_34 = arith.constant 1 : index
    %35 = memref.alloca(%c1_34) : memref<?xvector<16xf32>>
    %c1_35 = arith.constant 1 : index
    %36 = memref.alloca(%c1_35) : memref<?xvector<2xf64>>
    %c1_36 = arith.constant 1 : index
    %37 = memref.alloca(%c1_36) : memref<?xvector<4xf64>>
    %c1_37 = arith.constant 1 : index
    %38 = memref.alloca(%c1_37) : memref<?xvector<8xf64>>
    %c1_38 = arith.constant 1 : index
    %39 = memref.alloca(%c1_38) : memref<?xvector<16xf64>>
    %c1_39 = arith.constant 1 : index
    %40 = memref.alloca(%c1_39) : memref<?xvector<3xi8>>
    %c1_40 = arith.constant 1 : index
    %41 = memref.alloca(%c1_40) : memref<?xvector<3xi8>>
    %c1_41 = arith.constant 1 : index
    %42 = memref.alloca(%c1_41) : memref<?xvector<3xi16>>
    %c1_42 = arith.constant 1 : index
    %43 = memref.alloca(%c1_42) : memref<?xvector<3xi16>>
    %c1_43 = arith.constant 1 : index
    %44 = memref.alloca(%c1_43) : memref<?xvector<3xi32>>
    %c1_44 = arith.constant 1 : index
    %45 = memref.alloca(%c1_44) : memref<?xvector<3xi32>>
    %c1_45 = arith.constant 1 : index
    %46 = memref.alloca(%c1_45) : memref<?xvector<3xi64>>
    %c1_46 = arith.constant 1 : index
    %47 = memref.alloca(%c1_46) : memref<?xvector<3xi64>>
    %c1_47 = arith.constant 1 : index
    %48 = memref.alloca(%c1_47) : memref<?xvector<3xf32>>
    %c1_48 = arith.constant 1 : index
    %49 = memref.alloca(%c1_48) : memref<?xvector<3xf64>>
    return
  }
}
      |]
    
    it "can translate vector op" $ do
      [r|
typedef char char2 __attribute__((__ext_vector_type__(2)));
typedef float float2 __attribute__((__ext_vector_type__(2)));

void foo() {
  char2 v0, v1;
  v0 + v1;
  float2 v2, v3;
  v2 + v3;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xvector<2xi8>>
    %c1_0 = arith.constant 1 : index
    %1 = memref.alloca(%c1_0) : memref<?xvector<2xi8>>
    %c0 = arith.constant 0 : index
    %2 = affine.load %0[%c0] : memref<?xvector<2xi8>>
    %c0_1 = arith.constant 0 : index
    %3 = affine.load %1[%c0_1] : memref<?xvector<2xi8>>
    %4 = arith.addi %2, %3 : vector<2xi8>
    %c1_2 = arith.constant 1 : index
    %5 = memref.alloca(%c1_2) : memref<?xvector<2xf32>>
    %c1_3 = arith.constant 1 : index
    %6 = memref.alloca(%c1_3) : memref<?xvector<2xf32>>
    %c0_4 = arith.constant 0 : index
    %7 = affine.load %5[%c0_4] : memref<?xvector<2xf32>>
    %c0_5 = arith.constant 0 : index
    %8 = affine.load %6[%c0_5] : memref<?xvector<2xf32>>
    %9 = arith.addf %7, %8 : vector<2xf32>
    return
  }
}
      |]
    
    it "can translate vector load/store" $ do
      [r|
typedef int int2 __attribute__((__ext_vector_type__(2)));

void foo() {
  int2 v0;
  int v1[2];
  vload(v1[0], &v0);
  vstore(v0, v1[0]);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c1 = arith.constant 1 : index
    %0 = memref.alloca(%c1) : memref<?xvector<2xi32>>
    %1 = memref.alloca() : memref<2xi32>
    %c0_i32 = arith.constant 0 : i32
    %2 = arith.index_cast %c0_i32 : i32 to index
    %3 = vector.load %1[%2] : memref<2xi32>, vector<2xi32>
    %c0 = arith.constant 0 : index
    affine.store %3, %0[%c0] : memref<?xvector<2xi32>>
    %c0_i32_0 = arith.constant 0 : i32
    %4 = arith.index_cast %c0_i32_0 : i32 to index
    %c0_1 = arith.constant 0 : index
    %5 = affine.load %0[%c0_1] : memref<?xvector<2xi32>>
    vector.store %5, %1[%4] : memref<2xi32>, vector<2xi32>
    return
  }
}
      |]
    
    it "can translate string literal" $ do
      [r|
void foo() {
  char *v = "aaa";
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %cst = arith.constant dense<97> : vector<3xi8>
    %c3 = arith.constant 3 : index
    %c0 = arith.constant 0 : index
    %0 = memref.alloca(%c3) : memref<?xi8>
    vector.store %cst, %0[%c0] : memref<?xi8>, vector<3xi8>
    %c1 = arith.constant 1 : index
    %1 = memref.alloca(%c1) : memref<?xmemref<?xi8>>
    %c0_0 = arith.constant 0 : index
    affine.store %0, %1[%c0_0] : memref<?xmemref<?xi8>>
    return
  }
}
      |]
    
    it "can translate dynamic casting" $ do
      [r|
void foo() {
  float *a = (float *)malloc(10);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %c10_i32 = arith.constant 10 : i32
    %0 = arith.index_cast %c10_i32 : i32 to index
    %1 = memref.alloc(%0) : memref<?xi8>
    %c0 = arith.constant 0 : index
    %2 = memref.dim %1, %c0 : memref<?xi8>
    %c0_0 = arith.constant 0 : index
    %3 = memref.view %1[%c0_0][%2] : memref<?xi8> to memref<?xf32>
    %c1 = arith.constant 1 : index
    %4 = memref.alloca(%c1) : memref<?xmemref<?xf32>>
    %c0_1 = arith.constant 0 : index
    affine.store %3, %4[%c0_1] : memref<?xmemref<?xf32>>
    return
  }
}
      |]
    
    it "can translate more array to pointer casting" $ do
      [r|
void foo() {
  int b[10][10];
  int *c = (int *)b;
  (int [10][10])c;
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<10x10xi32>
    %c1 = arith.constant 1 : index
    %c0 = arith.constant 0 : index
    %1 = memref.dim %0, %c0 : memref<10x10xi32>
    %2 = arith.muli %1, %c1 : index
    %c1_0 = arith.constant 1 : index
    %3 = memref.dim %0, %c1_0 : memref<10x10xi32>
    %4 = arith.muli %3, %2 : index
    %5 = memref.alloca() : memref<1xindex>
    %c0_1 = arith.constant 0 : index
    affine.store %4, %5[%c0_1] : memref<1xindex>
    %6 = memref.reshape %0(%5) : (memref<10x10xi32>, memref<1xindex>) -> memref<?xi32>
    %c1_2 = arith.constant 1 : index
    %7 = memref.alloca(%c1_2) : memref<?xmemref<?xi32>>
    %c0_3 = arith.constant 0 : index
    affine.store %6, %7[%c0_3] : memref<?xmemref<?xi32>>
    %c0_4 = arith.constant 0 : index
    %8 = affine.load %7[%c0_4] : memref<?xmemref<?xi32>>
    %c10 = arith.constant 10 : index
    %c10_5 = arith.constant 10 : index
    %9 = memref.alloca() : memref<2xindex>
    %c0_6 = arith.constant 0 : index
    affine.store %c10, %9[%c0_6] : memref<2xindex>
    %c1_7 = arith.constant 1 : index
    affine.store %c10_5, %9[%c1_7] : memref<2xindex>
    %10 = memref.reshape %8(%9) : (memref<?xi32>, memref<2xindex>) -> memref<10x10xi32>
    return
  }
}
      |]

    it "can translate array of pointers" $ do
      [r|
__kernel void foo(__global float *input) {}

void launch(const char *, int, int, char **);

void main() {
    char *inputs[1];
    inputs[0] = malloc(10);
    launch("foo", 1, 1, (char **)inputs);
    free(inputs[0]);
}
      |] `shouldBeTranslatedAs` [r|
#map = affine_map<() -> (0)>
module  {
  func private @launch(memref<?xi8>, i32, i32, memref<?xmemref<?xi8>>)
  func @foo(%arg0: memref<?xf32, 2>) attributes {cl.kernel = true, llvm.emit_c_interface} {
    return
  }
  func @main() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<1xmemref<?xi8>>
    %c10_i32 = arith.constant 10 : i32
    %1 = arith.index_cast %c10_i32 : i32 to index
    %2 = memref.alloc(%1) : memref<?xi8>
    %c0_i32 = arith.constant 0 : i32
    %3 = arith.index_cast %c0_i32 : i32 to index
    %4 = affine.apply #map()
    affine.store %2, %0[%4] : memref<1xmemref<?xi8>>
    %cst = arith.constant dense<[102, 111, 111]> : vector<3xi8>
    %c3 = arith.constant 3 : index
    %c0 = arith.constant 0 : index
    %5 = memref.alloca(%c3) : memref<?xi8>
    vector.store %cst, %5[%c0] : memref<?xi8>, vector<3xi8>
    %c1_i32 = arith.constant 1 : i32
    %c1_i32_0 = arith.constant 1 : i32
    %6 = memref.cast %0 : memref<1xmemref<?xi8>> to memref<?xmemref<?xi8>>
    call @launch(%5, %c1_i32, %c1_i32_0, %6) : (memref<?xi8>, i32, i32, memref<?xmemref<?xi8>>) -> ()
    %c0_i32_1 = arith.constant 0 : i32
    %7 = arith.index_cast %c0_i32_1 : i32 to index
    %8 = affine.apply #map()
    %9 = affine.load %0[%8] : memref<1xmemref<?xi8>>
    memref.dealloc %9 : memref<?xi8>
    return
  }
}
      |]
    
    it "can translate conv_1d" $ do
      [r|
void foo() {
  float lhs[3];
  float rhs[1];
  float output[3];
  conv_1d(lhs, rhs, output);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<3xf32>
    %1 = memref.alloca() : memref<1xf32>
    %2 = memref.alloca() : memref<3xf32>
    linalg.conv_1d ins(%0, %1 : memref<3xf32>, memref<1xf32>) outs(%2 : memref<3xf32>)
    return
  }
}
      |]
    
    it "can translate conv_2d" $ do
      [r|
void foo() {
  float lhs[3][4];
  float rhs[1][1];
  float output[3][4];
  conv_2d(lhs, rhs, output);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %0 = memref.alloca() : memref<3x4xf32>
    %1 = memref.alloca() : memref<1x1xf32>
    %2 = memref.alloca() : memref<3x4xf32>
    linalg.conv_2d ins(%0, %1 : memref<3x4xf32>, memref<1x1xf32>) outs(%2 : memref<3x4xf32>)
    return
  }
}
      |]
    
    it "can translate math functions" $ do
      [r|
void foo() {
  abs(1.0);
  atan2(1.0, 2.0);
  atan(1.0);
  ceil(1.0);
  cos(1.0);
  erf(1.0);
  exp2(1.0);
  expm1(1.0);
  exp(1.0);
  floor(1.0);
  fma(1.0, 2.0, 3.0);
  log10(1.0);
  log1p(1.0);
  log2(1.0);
  log(1.0);
  powf(1.0,2.0);
  rsqrt(1.0);
  sin(1.0);
  sqrt(1.0);
  tanh(1.0);
}
      |] `shouldBeTranslatedAs` [r|
module  {
  func @foo() attributes {llvm.emit_c_interface} {
    %cst = arith.constant 1.000000e+00 : f32
    %0 = math.abs %cst : f32
    %cst_0 = arith.constant 1.000000e+00 : f32
    %cst_1 = arith.constant 2.000000e+00 : f32
    %1 = math.atan2 %cst_0, %cst_1 : f32
    %cst_2 = arith.constant 1.000000e+00 : f32
    %2 = math.atan %cst_2 : f32
    %cst_3 = arith.constant 1.000000e+00 : f32
    %3 = math.ceil %cst_3 : f32
    %cst_4 = arith.constant 1.000000e+00 : f32
    %4 = math.cos %cst_4 : f32
    %cst_5 = arith.constant 1.000000e+00 : f32
    %5 = math.erf %cst_5 : f32
    %cst_6 = arith.constant 1.000000e+00 : f32
    %6 = math.exp2 %cst_6 : f32
    %cst_7 = arith.constant 1.000000e+00 : f32
    %7 = math.expm1 %cst_7 : f32
    %cst_8 = arith.constant 1.000000e+00 : f32
    %8 = math.exp %cst_8 : f32
    %cst_9 = arith.constant 1.000000e+00 : f32
    %9 = math.floor %cst_9 : f32
    %cst_10 = arith.constant 1.000000e+00 : f32
    %cst_11 = arith.constant 2.000000e+00 : f32
    %cst_12 = arith.constant 3.000000e+00 : f32
    %10 = math.fma %cst_10, %cst_11, %cst_12 : f32
    %cst_13 = arith.constant 1.000000e+00 : f32
    %11 = math.log10 %cst_13 : f32
    %cst_14 = arith.constant 1.000000e+00 : f32
    %12 = math.log1p %cst_14 : f32
    %cst_15 = arith.constant 1.000000e+00 : f32
    %13 = math.log2 %cst_15 : f32
    %cst_16 = arith.constant 1.000000e+00 : f32
    %14 = math.log %cst_16 : f32
    %cst_17 = arith.constant 1.000000e+00 : f32
    %cst_18 = arith.constant 2.000000e+00 : f32
    %15 = math.powf %cst_17, %cst_18 : f32
    %cst_19 = arith.constant 1.000000e+00 : f32
    %16 = math.rsqrt %cst_19 : f32
    %cst_20 = arith.constant 1.000000e+00 : f32
    %17 = math.sin %cst_20 : f32
    %cst_21 = arith.constant 1.000000e+00 : f32
    %18 = math.sqrt %cst_21 : f32
    %cst_22 = arith.constant 1.000000e+00 : f32
    %19 = math.tanh %cst_22 : f32
    return
  }
}
      |]
    
