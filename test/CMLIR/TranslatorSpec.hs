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
    %0 = memref.alloca() : memref<i8>
    %1 = memref.alloca() : memref<i8>
    %2 = memref.alloca() : memref<i16>
    %3 = memref.alloca() : memref<i16>
    %4 = memref.alloca() : memref<i32>
    %5 = memref.alloca() : memref<i32>
    %6 = memref.alloca() : memref<i64>
    %7 = memref.alloca() : memref<i64>
    %8 = memref.alloca() : memref<f32>
    %9 = memref.alloca() : memref<f64>
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
    %0 = memref.alloca() : memref<i8>
    affine.store %c97_i8, %0[] : memref<i8>
    %c1_i32 = arith.constant 1 : i32
    %1 = memref.alloca() : memref<i32>
    affine.store %c1_i32, %1[] : memref<i32>
    %c1_i64 = arith.constant 1 : i64
    %2 = memref.alloca() : memref<i64>
    affine.store %c1_i64, %2[] : memref<i64>
    %cst = arith.constant 1.000000e-01 : f32
    %3 = memref.alloca() : memref<f32>
    affine.store %cst, %3[] : memref<f32>
    %cst_0 = arith.constant 1.000000e-01 : f64
    %4 = memref.alloca() : memref<f64>
    affine.store %cst_0, %4[] : memref<f64>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
    %4 = arith.addi %2, %3 : i32
    %5 = affine.load %0[] : memref<i32>
    %6 = affine.load %1[] : memref<i32>
    %7 = arith.subi %5, %6 : i32
    %8 = affine.load %0[] : memref<i32>
    %9 = affine.load %1[] : memref<i32>
    %10 = arith.muli %8, %9 : i32
    %11 = affine.load %0[] : memref<i32>
    %12 = affine.load %1[] : memref<i32>
    %13 = arith.divsi %11, %12 : i32
    %14 = affine.load %0[] : memref<i32>
    %15 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
    %4 = arith.addi %2, %3 : i32
    %5 = affine.load %0[] : memref<i32>
    %6 = affine.load %1[] : memref<i32>
    %7 = arith.subi %5, %6 : i32
    %8 = affine.load %0[] : memref<i32>
    %9 = affine.load %1[] : memref<i32>
    %10 = arith.muli %8, %9 : i32
    %11 = affine.load %0[] : memref<i32>
    %12 = affine.load %1[] : memref<i32>
    %13 = arith.divui %11, %12 : i32
    %14 = affine.load %0[] : memref<i32>
    %15 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<f32>
    %1 = memref.alloca() : memref<f32>
    %2 = affine.load %0[] : memref<f32>
    %3 = affine.load %1[] : memref<f32>
    %4 = arith.addf %2, %3 : f32
    %5 = affine.load %0[] : memref<f32>
    %6 = affine.load %1[] : memref<f32>
    %7 = arith.subf %5, %6 : f32
    %8 = affine.load %0[] : memref<f32>
    %9 = affine.load %1[] : memref<f32>
    %10 = arith.mulf %8, %9 : f32
    %11 = affine.load %0[] : memref<f32>
    %12 = affine.load %1[] : memref<f32>
    %13 = arith.divf %11, %12 : f32
    %14 = affine.load %0[] : memref<f32>
    %15 = affine.load %1[] : memref<f32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %5 = affine.load %0[] : memref<i32>
    %6 = affine.load %1[] : memref<i32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %8 = affine.load %0[] : memref<i32>
    %9 = affine.load %1[] : memref<i32>
    %10 = arith.cmpi sgt, %8, %9 : i32
    %11 = affine.load %0[] : memref<i32>
    %12 = affine.load %1[] : memref<i32>
    %13 = arith.cmpi slt, %11, %12 : i32
    %14 = affine.load %0[] : memref<i32>
    %15 = affine.load %1[] : memref<i32>
    %16 = arith.cmpi sge, %14, %15 : i32
    %17 = affine.load %0[] : memref<i32>
    %18 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %5 = affine.load %0[] : memref<i32>
    %6 = affine.load %1[] : memref<i32>
    %7 = arith.cmpi ne, %5, %6 : i32
    %8 = affine.load %0[] : memref<i32>
    %9 = affine.load %1[] : memref<i32>
    %10 = arith.cmpi ugt, %8, %9 : i32
    %11 = affine.load %0[] : memref<i32>
    %12 = affine.load %1[] : memref<i32>
    %13 = arith.cmpi ult, %11, %12 : i32
    %14 = affine.load %0[] : memref<i32>
    %15 = affine.load %1[] : memref<i32>
    %16 = arith.cmpi uge, %14, %15 : i32
    %17 = affine.load %0[] : memref<i32>
    %18 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
    %4 = arith.cmpi eq, %2, %3 : i32
    %5 = affine.load %0[] : memref<i32>
    %6 = affine.load %1[] : memref<i32>
    %7 = arith.cmpi eq, %5, %6 : i32
    %8 = arith.andi %4, %7 : i1
    %9 = affine.load %0[] : memref<i32>
    %10 = affine.load %1[] : memref<i32>
    %11 = arith.cmpi eq, %9, %10 : i32
    %12 = affine.load %0[] : memref<i32>
    %13 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = affine.load %0[] : memref<i32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    affine.store %2, %0[] : memref<i32>
    %3 = affine.load %0[] : memref<i32>
    %c1_i32_0 = arith.constant 1 : i32
    %4 = arith.subi %3, %c1_i32_0 : i32
    affine.store %4, %0[] : memref<i32>
    %5 = affine.load %0[] : memref<i32>
    %c1_i32_1 = arith.constant 1 : i32
    %6 = arith.muli %5, %c1_i32_1 : i32
    affine.store %6, %0[] : memref<i32>
    %7 = affine.load %0[] : memref<i32>
    %c1_i32_2 = arith.constant 1 : i32
    %8 = arith.divsi %7, %c1_i32_2 : i32
    affine.store %8, %0[] : memref<i32>
    %9 = affine.load %0[] : memref<i32>
    %c1_i32_3 = arith.constant 1 : i32
    %10 = arith.remsi %9, %c1_i32_3 : i32
    affine.store %10, %0[] : memref<i32>
    %11 = affine.load %0[] : memref<i32>
    %c1_i32_4 = arith.constant 1 : i32
    %12 = arith.andi %11, %c1_i32_4 : i32
    affine.store %12, %0[] : memref<i32>
    %13 = affine.load %0[] : memref<i32>
    %c1_i32_5 = arith.constant 1 : i32
    %14 = arith.ori %13, %c1_i32_5 : i32
    affine.store %14, %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %0[] : memref<i32>
    %1 = affine.load %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %0[] : memref<i32>
    scf.while : () -> () {
      %1 = affine.load %0[] : memref<i32>
      %c2_i32 = arith.constant 2 : i32
      %2 = arith.cmpi sgt, %1, %c2_i32 : i32
      scf.condition(%2)
    } do {
      %1 = affine.load %0[] : memref<i32>
      %c1_i32 = arith.constant 1 : i32
      %2 = arith.addi %1, %c1_i32 : i32
      affine.store %2, %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %0[] : memref<i32>
    scf.while : () -> () {
      %1 = affine.load %0[] : memref<i32>
      %c10_i32 = arith.constant 10 : i32
      %2 = arith.cmpi slt, %1, %c10_i32 : i32
      scf.condition(%2)
    } do {
      %1 = affine.load %0[] : memref<i32>
      %c1_i32 = arith.constant 1 : i32
      %2 = arith.addi %1, %c1_i32 : i32
      affine.store %2, %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %0[] : memref<i32>
    %1 = affine.load %0[] : memref<i32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    affine.store %2, %0[] : memref<i32>
    scf.while : () -> () {
      %3 = affine.load %0[] : memref<i32>
      %c10_i32 = arith.constant 10 : i32
      %4 = arith.cmpi slt, %3, %c10_i32 : i32
      scf.condition(%4)
    } do {
      %3 = affine.load %0[] : memref<i32>
      %c1_i32_0 = arith.constant 1 : i32
      %4 = arith.addi %3, %c1_i32_0 : i32
      affine.store %4, %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.apply #map0()
    %3 = affine.apply #map1()
    affine.for %arg0 = #map2(%2) to #map2(%3) {
      %4 = arith.index_cast %arg0 : index to i32
      %5 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %3 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i8>
    %1 = memref.alloca() : memref<i16>
    %2 = memref.alloca() : memref<i32>
    %3 = memref.alloca() : memref<i64>
    %4 = memref.alloca() : memref<f32>
    %5 = memref.alloca() : memref<f64>
    %6 = affine.load %0[] : memref<i8>
    %7 = affine.load %1[] : memref<i16>
    %8 = arith.trunci %7 : i16 to i8
    %9 = affine.load %2[] : memref<i32>
    %10 = arith.trunci %9 : i32 to i8
    %11 = affine.load %3[] : memref<i64>
    %12 = arith.trunci %11 : i64 to i8
    %13 = affine.load %4[] : memref<f32>
    %14 = arith.fptosi %13 : f32 to i32
    %15 = arith.trunci %14 : i32 to i8
    %16 = affine.load %5[] : memref<f64>
    %17 = arith.fptosi %16 : f64 to i64
    %18 = arith.trunci %17 : i64 to i8
    %19 = affine.load %0[] : memref<i8>
    %20 = arith.extsi %19 : i8 to i16
    %21 = affine.load %1[] : memref<i16>
    %22 = affine.load %2[] : memref<i32>
    %23 = arith.trunci %22 : i32 to i16
    %24 = affine.load %3[] : memref<i64>
    %25 = arith.trunci %24 : i64 to i16
    %26 = affine.load %4[] : memref<f32>
    %27 = arith.fptosi %26 : f32 to i32
    %28 = arith.trunci %27 : i32 to i16
    %29 = affine.load %5[] : memref<f64>
    %30 = arith.fptosi %29 : f64 to i64
    %31 = arith.trunci %30 : i64 to i16
    %32 = affine.load %0[] : memref<i8>
    %33 = arith.extsi %32 : i8 to i32
    %34 = affine.load %1[] : memref<i16>
    %35 = arith.extsi %34 : i16 to i32
    %36 = affine.load %2[] : memref<i32>
    %37 = affine.load %3[] : memref<i64>
    %38 = arith.trunci %37 : i64 to i32
    %39 = affine.load %4[] : memref<f32>
    %40 = arith.fptosi %39 : f32 to i32
    %41 = affine.load %5[] : memref<f64>
    %42 = arith.fptosi %41 : f64 to i64
    %43 = arith.trunci %42 : i64 to i32
    %44 = affine.load %0[] : memref<i8>
    %45 = arith.extsi %44 : i8 to i64
    %46 = affine.load %1[] : memref<i16>
    %47 = arith.extsi %46 : i16 to i64
    %48 = affine.load %2[] : memref<i32>
    %49 = arith.extsi %48 : i32 to i64
    %50 = affine.load %3[] : memref<i64>
    %51 = affine.load %4[] : memref<f32>
    %52 = arith.fptosi %51 : f32 to i32
    %53 = arith.extsi %52 : i32 to i64
    %54 = affine.load %5[] : memref<f64>
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
    %0 = memref.alloca() : memref<i8>
    %1 = memref.alloca() : memref<i16>
    %2 = memref.alloca() : memref<i32>
    %3 = memref.alloca() : memref<i64>
    %4 = memref.alloca() : memref<f32>
    %5 = memref.alloca() : memref<f64>
    %6 = affine.load %0[] : memref<i8>
    %7 = arith.extsi %6 : i8 to i32
    %8 = arith.sitofp %7 : i32 to f32
    %9 = affine.load %1[] : memref<i16>
    %10 = arith.extsi %9 : i16 to i32
    %11 = arith.sitofp %10 : i32 to f32
    %12 = affine.load %2[] : memref<i32>
    %13 = arith.sitofp %12 : i32 to f32
    %14 = affine.load %3[] : memref<i64>
    %15 = arith.trunci %14 : i64 to i32
    %16 = arith.sitofp %15 : i32 to f32
    %17 = affine.load %4[] : memref<f32>
    %18 = affine.load %5[] : memref<f64>
    %19 = arith.truncf %18 : f64 to f32
    %20 = affine.load %0[] : memref<i8>
    %21 = arith.extsi %20 : i8 to i64
    %22 = arith.sitofp %21 : i64 to f64
    %23 = affine.load %1[] : memref<i16>
    %24 = arith.extsi %23 : i16 to i64
    %25 = arith.sitofp %24 : i64 to f64
    %26 = affine.load %2[] : memref<i32>
    %27 = arith.extsi %26 : i32 to i64
    %28 = arith.sitofp %27 : i64 to f64
    %29 = affine.load %3[] : memref<i64>
    %30 = arith.sitofp %29 : i64 to f64
    %31 = affine.load %4[] : memref<f32>
    %32 = arith.extf %31 : f32 to f64
    %33 = affine.load %5[] : memref<f64>
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
    %0 = memref.alloca() : memref<i32>
    %1 = affine.load %0[] : memref<i32>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.addi %1, %c1_i32 : i32
    affine.store %2, %0[] : memref<i32>
    %3 = affine.load %0[] : memref<i32>
    %4 = affine.load %0[] : memref<i32>
    %c1_i32_0 = arith.constant 1 : i32
    %5 = arith.subi %4, %c1_i32_0 : i32
    affine.store %5, %0[] : memref<i32>
    %6 = affine.load %0[] : memref<i32>
    %7 = affine.load %0[] : memref<i32>
    %8 = affine.load %0[] : memref<i32>
    %c1_i32_1 = arith.constant 1 : i32
    %9 = arith.addi %8, %c1_i32_1 : i32
    affine.store %9, %0[] : memref<i32>
    %10 = affine.load %0[] : memref<i32>
    %11 = affine.load %0[] : memref<i32>
    %c1_i32_2 = arith.constant 1 : i32
    %12 = arith.subi %11, %c1_i32_2 : i32
    affine.store %12, %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = affine.load %0[] : memref<i32>
    %2 = affine.load %0[] : memref<i32>
    %c0_i32 = arith.constant 0 : i32
    %3 = arith.subi %c0_i32, %2 : i32
    %4 = memref.alloca() : memref<f32>
    %5 = affine.load %4[] : memref<f32>
    %6 = affine.load %4[] : memref<f32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = affine.load %0[] : memref<i32>
    %2 = affine.load %0[] : memref<i32>
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
    %0 = memref.alloca() : memref<memref<?xi32>>
    %1 = affine.load %0[] : memref<memref<?xi32>>
    %c0 = arith.constant 0 : index
    %2 = affine.load %1[%c0] : memref<?xi32>
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
    %0 = memref.alloca() : memref<memref<?xi32>>
    %1 = affine.load %0[] : memref<memref<?xi32>>
    %c1_i32 = arith.constant 1 : i32
    %2 = arith.index_cast %c1_i32 : i32 to index
    %3 = affine.apply #map0()
    %4 = affine.load %1[%3] : memref<?xi32>
    %5 = affine.load %0[] : memref<memref<?xi32>>
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
    %0 = memref.alloca() : memref<memref<?xi32>>
    %1 = affine.load %0[] : memref<memref<?xi32>>
    %c1_i32 = arith.constant 1 : i32
    %c0 = arith.constant 0 : index
    affine.store %c1_i32, %1[%c0] : memref<?xi32>
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
    %0 = memref.alloca() : memref<memref<?xi8>>
    %1 = memref.alloca() : memref<memref<?xi8>>
    %2 = affine.load %1[] : memref<memref<?xi8>>
    affine.store %2, %0[] : memref<memref<?xi8>>
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
    %0 = memref.alloca() : memref<memref<?xi32>>
    %1 = affine.load %0[] : memref<memref<?xi32>>
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
    %0 = memref.alloca() : memref<memref<?xi8>>
    %1 = affine.load %0[] : memref<memref<?xi8>>
    %c0 = arith.constant 0 : index
    %2 = memref.view %1[%c0][] : memref<?xi8> to memref<8xi32>
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
    %1 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %1[] : memref<i32>
    %c1_i32 = arith.constant 1 : i32
    affine.store %c1_i32, %1[] : memref<i32>
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
    %2 = memref.alloca() : memref<memref<?xi8>>
    affine.store %1, %2[] : memref<memref<?xi8>>
    %3 = affine.load %2[] : memref<memref<?xi8>>
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
    %1 = memref.cast %0 : memref<10xi8> to memref<?xi8>
    %2 = memref.alloca() : memref<memref<?xi8>>
    affine.store %1, %2[] : memref<memref<?xi8>>
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
    %0 = memref.alloca() : memref<i32>
    %c1_i32 = arith.constant 1 : i32
    %1 = arith.index_cast %c1_i32 : i32 to index
    %2 = memref.alloca() : memref<2xi32>
    %c1_i32_0 = arith.constant 1 : i32
    %c2_i32 = arith.constant 2 : i32
    %3 = memref.alloca() : memref<2xi32>
    %c0 = arith.constant 0 : index
    affine.store %c1_i32_0, %3[%c0] : memref<2xi32>
    %c1 = arith.constant 1 : index
    affine.store %c2_i32, %3[%c1] : memref<2xi32>
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
    %3 = memref.alloca() : memref<i32>
    affine.store %c1_i32, %3[] : memref<i32>
    %4 = affine.load %3[] : memref<i32>
    %5 = arith.index_cast %4 : i32 to index
    %c1_i32_0 = arith.constant 1 : i32
    %6 = arith.index_cast %c1_i32_0 : i32 to index
    %c1_i32_1 = arith.constant 1 : i32
    %7 = arith.index_cast %c1_i32_1 : i32 to index
    %c1_i32_2 = arith.constant 1 : i32
    %8 = arith.index_cast %c1_i32_2 : i32 to index
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
    %1 = memref.alloca() : memref<i32>
    affine.store %c0_i32, %1[] : memref<i32>
    %2 = affine.load %1[] : memref<i32>
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
    %0 = memref.alloca() : memref<i32>
    %1 = memref.alloca() : memref<i32>
    memref.alloca_scope  {
      %4 = memref.alloca() : memref<i32>
      %5 = memref.alloca() : memref<i32>
      %6 = affine.load %5[] : memref<i32>
    }
    %2 = affine.apply #map0()
    %3 = affine.apply #map1()
    affine.for %arg0 = #map2(%2) to #map2(%3) {
      %4 = arith.index_cast %arg0 : index to i32
      memref.alloca_scope  {
        %5 = memref.alloca() : memref<i32>
        %6 = affine.load %5[] : memref<i32>
      }
    }
    return
  }
}
      |]

    it "can translate opencl vector type" $ do
      [r|
typedef struct char2   char2;
typedef struct char3   char3;
typedef struct char4   char4;
typedef struct char8   char8;
typedef struct char16  char16;
typedef struct uchar2  uchar2;
typedef struct uchar3  uchar3;
typedef struct uchar4  uchar4;
typedef struct uchar8  uchar8;
typedef struct uchar16 uchar16;

typedef struct short2   short2;
typedef struct short3   short3;
typedef struct short4   short4;
typedef struct short8   short8;
typedef struct short16  short16;
typedef struct ushort2  ushort2;
typedef struct ushort3  ushort3;
typedef struct ushort4  ushort4;
typedef struct ushort8  ushort8;
typedef struct ushort16 ushort16;

typedef struct int2   int2;
typedef struct int3   int3;
typedef struct int4   int4;
typedef struct int8   int8;
typedef struct int16  int16;
typedef struct uint2  uint2;
typedef struct uint3  uint3;
typedef struct uint4  uint4;
typedef struct uint8  uint8;
typedef struct uint16 uint16;

typedef struct long2   long2;
typedef struct long3   long3;
typedef struct long4   long4;
typedef struct long8   long8;
typedef struct long16  long16;
typedef struct ulong2  ulong2;
typedef struct ulong3  ulong3;
typedef struct ulong4  ulong4;
typedef struct ulong8  ulong8;
typedef struct ulong16 ulong16;

typedef struct float2   float2;
typedef struct float3   float3;
typedef struct float4   float4;
typedef struct float8   float8;
typedef struct float16  float16;

typedef struct double2   double2;
typedef struct double3   double3;
typedef struct double4   double4;
typedef struct double8   double8;
typedef struct double16  double16;

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
    %0 = memref.alloca() : memref<vector<2xi8>>
    %1 = memref.alloca() : memref<vector<4xi8>>
    %2 = memref.alloca() : memref<vector<8xi8>>
    %3 = memref.alloca() : memref<vector<16xi8>>
    %4 = memref.alloca() : memref<vector<2xi8>>
    %5 = memref.alloca() : memref<vector<4xi8>>
    %6 = memref.alloca() : memref<vector<8xi8>>
    %7 = memref.alloca() : memref<vector<16xi8>>
    %8 = memref.alloca() : memref<vector<2xi16>>
    %9 = memref.alloca() : memref<vector<4xi16>>
    %10 = memref.alloca() : memref<vector<8xi16>>
    %11 = memref.alloca() : memref<vector<16xi16>>
    %12 = memref.alloca() : memref<vector<2xi16>>
    %13 = memref.alloca() : memref<vector<4xi16>>
    %14 = memref.alloca() : memref<vector<8xi16>>
    %15 = memref.alloca() : memref<vector<16xi16>>
    %16 = memref.alloca() : memref<vector<2xi32>>
    %17 = memref.alloca() : memref<vector<4xi32>>
    %18 = memref.alloca() : memref<vector<8xi32>>
    %19 = memref.alloca() : memref<vector<16xi32>>
    %20 = memref.alloca() : memref<vector<2xi32>>
    %21 = memref.alloca() : memref<vector<4xi32>>
    %22 = memref.alloca() : memref<vector<8xi32>>
    %23 = memref.alloca() : memref<vector<16xi32>>
    %24 = memref.alloca() : memref<vector<2xi64>>
    %25 = memref.alloca() : memref<vector<4xi64>>
    %26 = memref.alloca() : memref<vector<8xi64>>
    %27 = memref.alloca() : memref<vector<16xi64>>
    %28 = memref.alloca() : memref<vector<2xi64>>
    %29 = memref.alloca() : memref<vector<4xi64>>
    %30 = memref.alloca() : memref<vector<8xi64>>
    %31 = memref.alloca() : memref<vector<16xi64>>
    %32 = memref.alloca() : memref<vector<2xf32>>
    %33 = memref.alloca() : memref<vector<4xf32>>
    %34 = memref.alloca() : memref<vector<8xf32>>
    %35 = memref.alloca() : memref<vector<16xf32>>
    %36 = memref.alloca() : memref<vector<2xf64>>
    %37 = memref.alloca() : memref<vector<4xf64>>
    %38 = memref.alloca() : memref<vector<8xf64>>
    %39 = memref.alloca() : memref<vector<16xf64>>
    %40 = memref.alloca() : memref<vector<3xi8>>
    %41 = memref.alloca() : memref<vector<3xi8>>
    %42 = memref.alloca() : memref<vector<3xi16>>
    %43 = memref.alloca() : memref<vector<3xi16>>
    %44 = memref.alloca() : memref<vector<3xi32>>
    %45 = memref.alloca() : memref<vector<3xi32>>
    %46 = memref.alloca() : memref<vector<3xi64>>
    %47 = memref.alloca() : memref<vector<3xi64>>
    %48 = memref.alloca() : memref<vector<3xf32>>
    %49 = memref.alloca() : memref<vector<3xf32>>
    return
  }
}
      |]