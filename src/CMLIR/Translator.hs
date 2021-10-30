{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module CMLIR.Translator where

import qualified MLIR.AST.Builder as AST
import qualified MLIR.AST as AST
import MLIR.AST.Serialize
import qualified Data.ByteString.UTF8 as BU
import qualified MLIR.AST.Dialect.Arith as Arith
import qualified MLIR.AST.Dialect.Std as Std
import qualified MLIR.Native as MLIR

import Language.C.Syntax.AST
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Language.C.Data.Node ( NodeInfo )
import Language.C.Pretty
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.Maybe
import qualified Data.Map as M
import System.Exit
import Debug.Trace
import MLIR.AST.Builder (MonadBlockDecl)
import qualified Language.C.Analysis.TypeUtils as AST

data Env = Env {funcDefs :: [FunDef], typeDefs :: [Type]}

unsupportedM :: (Pretty a, Monad m) => a -> m b
unsupportedM a = return $ unsupported a

unsupported :: (Pretty a) => a -> b
unsupported a = error $ "unsupported:\n" ++ show (pretty a)

translateToMLIR :: CTranslUnit -> IO String
translateToMLIR tu =
   MLIR.withContext (\ctx -> do
     MLIR.registerAllDialects ctx
     nativeOp <- fromAST ctx (mempty, mempty) $ do
                   let res = runTrav [] $ AST.buildModule $ do
                              -- setDefaultLocation (FileLocation "" 0 0)
                              lift $ withExtDeclHandler (analyseAST tu) handlers
                              fs <- lift getUserState
                              forM_ fs (\(name, ty) -> AST.buildSimpleFunction (BU.fromString name) [] AST.NoAttrs (do Std.return []))
                   case res of
                     Left errs -> error $ show errs
                     Right (res, _) -> res 
     --MLIR.dump nativeOp
     check <- MLIR.verifyOperation nativeOp
     unless check $ exitWith (ExitFailure 1)
     BU.toString <$> MLIR.showOperationWithLocation nativeOp)

handlers :: DeclEvent -> TravT [(String, AST.Type)] Identity ()
handlers (TagEvent tagDef) = handleTag tagDef
handlers (DeclEvent identDecl) = handleIdent identDecl
handlers (ParamEvent paramDecl) = handleParam paramDecl
handlers (LocalEvent identDecl) = handleIdent identDecl
handlers (TypeDefEvent typeDef) = handleType typeDef
handlers (AsmEvent asmBlock) = handleAsm asmBlock

handleTag (CompDef compT) = return ()
handleTag (EnumDef enumT) = return ()

handleIdent (Declaration decl) = return ()
handleIdent (ObjectDef objDef) = return ()
handleIdent (FunctionDef funDef) = handleFDef funDef
handleIdent (EnumeratorDef enumerator) = return ()

handleParam (ParamDecl varDecl _) = return ()
handleParam (AbstractParamDecl varDecl _) = return ()

handleType (TypeDef ident ty attrs _) = return ()

handleAsm (CStrLit c n) = return ()

varName (VarName (Ident ident _ _) _) = ident
varName NoName = ""

type_ (FunctionType ty attrs) = f ty 
  where f (FunType resType argTypes _) = 
            AST.FunctionType (map (\t -> paramDecl t ^. _2) argTypes) (map type_ [resType])
        f (FunTypeIncomplete ty) = type_ ty
type_ t@(DirectType name quals attrs) =
  case name of
    TyVoid -> AST.NoneType
    TyIntegral (id -> TyBool) -> AST.IntegerType AST.Signless 1
    TyIntegral (id -> TyChar) -> AST.IntegerType AST.Signed 8
    TyIntegral (id -> TySChar) -> AST.IntegerType AST.Signed 8
    TyIntegral (id -> TyUChar) -> AST.IntegerType AST.Signless 8
    TyIntegral (id -> TyShort) -> AST.IntegerType AST.Signed 16
    TyIntegral (id -> TyUShort) -> AST.IntegerType AST.Signless 16
    TyIntegral (id -> TyInt) -> AST.IntegerType AST.Signed 32
    TyIntegral (id -> TyUInt) -> AST.IntegerType AST.Signless 32
    TyIntegral (id -> TyInt128) -> AST.IntegerType AST.Signed 128
    TyIntegral (id -> TyUInt128) -> AST.IntegerType AST.Signless 128
    TyIntegral (id -> TyLong) -> AST.IntegerType AST.Signed 64
    TyIntegral (id -> TyULong) -> AST.IntegerType AST.Signless 64
    TyIntegral (id -> TyLLong) -> AST.IntegerType AST.Signed 64
    TyIntegral (id -> TyULLong) -> AST.IntegerType AST.Signless 64
    TyFloating (id -> TyFloat) -> AST.Float32Type 
    TyFloating (id -> TyDouble) -> AST.Float64Type
    TyFloating (id -> TyLDouble) -> AST.Float64Type
    ty -> unsupported t
type_ ty = unsupported ty

paramDecl (ParamDecl var _) = varDecl var
paramDecl (AbstractParamDecl var _) = varDecl var

varDecl (VarDecl name attrs ty) = (varName name, type_ ty)

handleFDef (FunDef var stmt _) = do
  let (name, ty) = varDecl var
  modifyUserState ((name, ty):)
  return ()
