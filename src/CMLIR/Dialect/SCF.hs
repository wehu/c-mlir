{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.SCF where

import MLIR.AST

for :: Location -> [Type] -> Name -> Name -> Name -> [Name] -> Region -> Operation
for loc types lb ub step args region = Operation
  { opName = "scf.for"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = [lb, ub, step] ++ args
  , opRegions = [region]
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

if_ :: Location -> [Type] -> Name -> Region -> Region -> Operation
if_ loc types cond t f = Operation
  { opName = "scf.if"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = [cond]
  , opRegions = [t, f]
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

yield :: Location -> [Type] -> [Name] -> Operation
yield loc types args = Operation
  { opName = "scf.yield"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs 
  }