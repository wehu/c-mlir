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

ifelse :: Location -> [Type] -> Name -> Region -> Region -> Operation
ifelse loc types cond t f = Operation
  { opName = "scf.if"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = [cond]
  , opRegions = [t, f]
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

while :: Location -> [Type] -> [Name] -> Region -> Region -> Operation
while loc types args cond body = Operation
  { opName = "scf.while"
  , opLocation = loc
  , opResultTypes = Explicit types
  , opOperands = args
  , opRegions = [cond, body]
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

condition :: Location -> Name -> [Name] -> Operation
condition loc cond args = Operation
  { opName = "scf.condition"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = cond:args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }


yield :: Location -> [Name] -> Operation
yield loc args = Operation
  { opName = "scf.yield"
  , opLocation = loc
  , opResultTypes = Explicit []
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs 
  }