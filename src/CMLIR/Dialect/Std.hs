{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Std where

import MLIR.AST
import Data.ByteString.UTF8

call :: Location -> Type -> ByteString -> [Name] -> Operation
call loc ty callee args = Operation
  { opName = "std.call"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = args
  , opRegions = []
  , opSuccessors = []
  , opAttributes = namedAttribute "callee" (FlatSymbolRefAttr callee)
  }