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

data Env = Env {decls :: [Decl],
                objDefs :: [ObjDef],
                funDefs :: [FunDef],
                enumerators :: [Enumerator],
                typeDefs :: [TypeDef]}

initEnv = Env{decls = [],
              objDefs = [],
              funDefs = [],
              enumerators = [],
              typeDefs = []}

unsupportedM :: (Pretty a, Monad m) => a -> m b
unsupportedM a = return $ unsupported a

unsupported :: (Pretty a) => a -> b
unsupported a = error $ "unsupported:\n" ++ show (pretty a)

translateToMLIR :: CTranslUnit -> IO String
translateToMLIR tu =
   MLIR.withContext (\ctx -> do
     MLIR.registerAllDialects ctx
     nativeOp <- fromAST ctx (mempty, mempty) $ do
                   let res = runTrav initEnv $ AST.buildModule $ do
                              -- setDefaultLocation (FileLocation "" 0 0)
                              lift $ withExtDeclHandler (analyseAST tu) handlers
                              lift (funDefs <$> getUserState) >>= mapM_ addFunction
                   case res of
                     Left errs -> error $ show errs
                     Right (res, _) -> res
     --MLIR.dump nativeOp
     check <- MLIR.verifyOperation nativeOp
     unless check $ exitWith (ExitFailure 1)
     BU.toString <$> MLIR.showOperationWithLocation nativeOp)

addFunction :: MonadBlockDecl m => FunDef -> m ()
addFunction (FunDef var stmt _) = do
  let (name, ty) = varDecl var
  AST.buildSimpleFunction (BU.fromString name) [] AST.NoAttrs $ do
    Std.return []

handlers :: DeclEvent -> TravT Env Identity ()
handlers (TagEvent tagDef) = handleTag tagDef
handlers (DeclEvent identDecl) = handleIdentDecl identDecl
handlers (ParamEvent paramDecl) = handleParam paramDecl
handlers (LocalEvent identDecl) = handleIdentDecl identDecl
handlers (TypeDefEvent typeDef) = handleTypeDecl typeDef
handlers (AsmEvent asmBlock) = handleAsm asmBlock

handleTag :: Monad m => TagDef -> m ()
handleTag (CompDef compT) = return ()
handleTag (EnumDef enumT) = return ()

handleIdentDecl :: IdentDecl -> Trav Env ()
handleIdentDecl (Declaration decl) = modifyUserState (\s -> s{decls=decl : decls s})
handleIdentDecl (ObjectDef objDef) = modifyUserState (\s -> s{objDefs=objDef : objDefs s})
handleIdentDecl (FunctionDef funDef) = modifyUserState (\s -> s{funDefs=funDef : funDefs s})
handleIdentDecl (EnumeratorDef enumerator) = modifyUserState (\s -> s{enumerators=enumerator : enumerators s})

handleParam :: Monad m => ParamDecl -> m ()
handleParam (ParamDecl varDecl _) = return ()
handleParam (AbstractParamDecl varDecl _) = return ()

handleTypeDecl :: TypeDef -> Trav Env ()
handleTypeDecl typeDef = modifyUserState (\s -> s{typeDefs=typeDef : typeDefs s})

handleAsm :: Monad m => CStringLiteral a -> m ()
handleAsm (CStrLit c n) = return ()

varName :: VarName -> String
varName (VarName (Ident ident _ _) _) = ident
varName NoName = ""

type_ :: Type -> AST.Type
type_ (FunctionType ty attrs) = f ty
  where f (FunType resType argTypes _) =
            AST.FunctionType (map (\t -> paramDecl t ^. _2) argTypes) (map type_ [resType])
        f (FunTypeIncomplete ty) = type_ ty
type_ ty@(DirectType name quals attrs) =
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
    _ -> unsupported ty
type_ ty = unsupported ty

paramDecl :: ParamDecl -> (String, AST.Type)
paramDecl (ParamDecl var _) = varDecl var
paramDecl (AbstractParamDecl var _) = varDecl var

varDecl :: VarDecl -> (String, AST.Type)
varDecl (VarDecl name attrs ty) = (varName name, type_ ty)
