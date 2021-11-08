{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CMLIR.Dialect.Math where

import MLIR.AST

abs :: Location -> Type -> [Name] -> Operation
abs loc ty operands = Operation
  { opName = "math.abs"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

atan2 :: Location -> Type -> [Name] -> Operation
atan2 loc ty operands = Operation
  { opName = "math.atan2"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

atan :: Location -> Type -> [Name] -> Operation
atan loc ty operands = Operation
  { opName = "math.atan"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

ceil :: Location -> Type -> [Name] -> Operation
ceil loc ty operands = Operation
  { opName = "math.ceil"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

cos :: Location -> Type -> [Name] -> Operation
cos loc ty operands = Operation
  { opName = "math.cos"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

erf :: Location -> Type -> [Name] -> Operation
erf loc ty operands = Operation
  { opName = "math.erf"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

exp2 :: Location -> Type -> [Name] -> Operation
exp2 loc ty operands = Operation
  { opName = "math.exp2"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

expm1 :: Location -> Type -> [Name] -> Operation
expm1 loc ty operands = Operation
  { opName = "math.expm1"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

exp :: Location -> Type -> [Name] -> Operation
exp loc ty operands = Operation
  { opName = "math.exp"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

floor :: Location -> Type -> [Name] -> Operation
floor loc ty operands = Operation
  { opName = "math.floor"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

fma :: Location -> Type -> [Name] -> Operation
fma loc ty operands = Operation
  { opName = "math.fma"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

log10 :: Location -> Type -> [Name] -> Operation
log10 loc ty operands = Operation
  { opName = "math.log10"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

log1p :: Location -> Type -> [Name] -> Operation
log1p loc ty operands = Operation
  { opName = "math.log1p"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

log2 :: Location -> Type -> [Name] -> Operation
log2 loc ty operands = Operation
  { opName = "math.log2"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

log :: Location -> Type -> [Name] -> Operation
log loc ty operands = Operation
  { opName = "math.log"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

powf :: Location -> Type -> [Name] -> Operation
powf loc ty operands = Operation
  { opName = "math.powf"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

rsqrt :: Location -> Type -> [Name] -> Operation
rsqrt loc ty operands = Operation
  { opName = "math.rsqrt"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

sin :: Location -> Type -> [Name] -> Operation
sin loc ty operands = Operation
  { opName = "math.sin"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

sqrt :: Location -> Type -> [Name] -> Operation
sqrt loc ty operands = Operation
  { opName = "math.sqrt"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }

tanh :: Location -> Type -> [Name] -> Operation
tanh loc ty operands = Operation
  { opName = "math.tanh"
  , opLocation = loc
  , opResultTypes = Explicit [ty]
  , opOperands = operands
  , opRegions = []
  , opSuccessors = []
  , opAttributes = NoAttrs
  }