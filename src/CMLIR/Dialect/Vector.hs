{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Vector where

import MLIR.AST

vload :: Location -> Type -> Name -> [Name] -> Operation 
vload loc ty src indices = Operation
  { opName = "vector.load"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = src:indices
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

vstore :: Location -> Name -> Name -> [Name] -> Operation 
vstore loc v dst indices = Operation
  { opName = "vector.store"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = v:dst:indices
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

vtranspose :: Location -> Type -> Name -> [Int] -> Operation 
vtranspose loc ty v transp = Operation
  { opName = "vector.transpose"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = [v]
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "transp" 
                      (ArrayAttr $ IntegerAttr (IntegerType Signless 64) . fromIntegral <$> transp)
  }


