{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Pretty
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Data.Maybe
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Map as M
import System.Exit
import Debug.Trace

data Env = Env {decls :: [Decl],
                objDefs :: [ObjDef],
                funDefs :: [FunDef],
                enumerators :: [Enumerator],
                typeDefs :: [TypeDef],
                labels :: M.Map String AST.BlockName,
                idCounter :: Int}

type EnvM = TravT Env Identity

initEnv = Env{decls = [],
              objDefs = [],
              funDefs = [],
              enumerators = [],
              typeDefs = [],
              labels = M.empty,
              idCounter = 0}

underScope action = do
  env <- getUserState
  result <- action
  modifyUserState (const env)
  return result

addLabel name label =
   modifyUserState (\s -> s{labels=M.insert name label (labels s)})

lookupLabel name = do
  l <- M.lookup name <$> getUserState
  case l of
    Just l -> return l
    Nothing -> error $ "cannot find label " ++ name

freshName :: EnvM BU.ByteString
freshName = do
  id <- idCounter <$> getUserState
  modifyUserState (\s -> s{idCounter = idCounter s + 1})
  return $ BU.fromString $ show id

unsupported :: (Pretty a) => a -> b
unsupported a = error $ "unsupported:\n" ++ show (pretty a)

translateToMLIR :: CTranslUnit -> IO String
translateToMLIR tu =
   MLIR.withContext (\ctx -> do
     MLIR.registerAllDialects ctx
     nativeOp <- fromAST ctx (mempty, mempty) $ do
                   let res = runTrav initEnv $ do
                              withExtDeclHandler (analyseAST tu) handlers
                              fs <- getUserState >>= mapM transFunction . funDefs
                              id <- freshName
                              return $ AST.ModuleOp $ AST.Block id [] fs
                   case res of
                     Left errs -> error $ show errs
                     Right (res, _) -> res
     --MLIR.dump nativeOp
     check <- MLIR.verifyOperation nativeOp
     unless check $ exitWith (ExitFailure 1)
     BU.toString <$> MLIR.showOperationWithLocation nativeOp)

transFunction :: FunDef -> EnvM AST.Binding
transFunction (FunDef var stmt node) = underScope $ do
  let (name, ty) = varDecl var
      args = over (traverse . _1) BU.fromString $ params var
  b <- transBlock args stmt
  return $ AST.Do $ AST.FuncOp (getPos node) (BU.fromString name) ty $ AST.Region [b]

transBlock :: [(AST.Name, AST.Type)] -> CStatement NodeInfo -> EnvM AST.Block
transBlock args (CCompound labels items _) = do
  id <- freshName
  let lnames = map identName labels
  ops <- join <$> mapM transBlockItem items
  forM_ lnames (`addLabel` id)
  return $ AST.Block id args ops
transBlock args s = unsupported s

transBlockItem :: CCompoundBlockItem NodeInfo -> EnvM [AST.Binding]
transBlockItem (CBlockStmt s) = transStmt s
transBlockItem s = unsupported s

transStmt :: CStatement NodeInfo -> EnvM [AST.Binding]
transStmt (CReturn Nothing node) =
  return [AST.Do $ Std.Return (getPos node) []]
transStmt (CReturn (Just e) node) = do
  results <- transExpr e
  let (n AST.:= v) = last results
  return $ results ++ [AST.Do $ Std.Return (getPos node) [n]]
transStmt e = unsupported e

transExpr :: CExpression NodeInfo -> EnvM [AST.Binding]
transExpr (CConst c) = transConst c
transExpr e = unsupported e

transConst :: CConstant NodeInfo -> EnvM [AST.Binding]
transConst (CIntConst i node) = transInt i (getPos node)
transConst (CCharConst c node) = transChar c (getPos node)
transConst (CFloatConst f node) = transFloat f (getPos node)
transConst (CStrConst s node) = transStr s (getPos node)

transInt :: CInteger -> AST.Location -> EnvM [AST.Binding]
transInt (CInteger i _ _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 32
  return [id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral i))]

transChar :: CChar -> AST.Location -> EnvM [AST.Binding]
transChar (CChar c _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 8
  return [id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral $ ord c))]
transChar c loc = error "unsupported chars"

transFloat :: CFloat -> AST.Location -> EnvM [AST.Binding]
transFloat (CFloat str) loc = do
  id <- freshName
  let ty = AST.Float32Type
  return [id AST.:= Arith.Constant loc ty (AST.FloatAttr ty $ read str)]

transStr :: CString -> AST.Location -> EnvM [AST.Binding]
transStr s@(CString str _) loc = error "xxx"

handlers :: DeclEvent -> EnvM ()
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

identName :: Ident -> String
identName (Ident ident _ _) = ident

varName :: VarName -> String
varName (VarName ident _) = identName ident
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
    TyFloating (id -> TyFloatN n _) -> unsupported ty
    TyComplex t -> type_ (DirectType (TyFloating t) quals attrs)
    TyComp ref -> unsupported ty
    TyEnum ref -> unsupported ty
    TyBuiltin _ -> unsupported ty
    _ -> unsupported ty
type_ (PtrType t quals attrs) = AST.MemRefType [Nothing] (type_ t) Nothing Nothing
type_ (ArrayType t size quals attrs) = AST.MemRefType [arraySize size] (type_ t) Nothing Nothing
type_ (TypeDefType (TypeDefRef ident t _) quals attrs) = type_ t

arraySize :: ArraySize -> Maybe a
arraySize (UnknownArraySize static) = Nothing
arraySize (ArraySize static expr) = Nothing

paramDecl :: ParamDecl -> (String, AST.Type)
paramDecl (ParamDecl var _) = varDecl var
paramDecl (AbstractParamDecl var _) = varDecl var

varDecl :: VarDecl -> (String, AST.Type)
varDecl (VarDecl name attrs ty) = (varName name, type_ ty)

params :: VarDecl -> [(String, AST.Type)]
params (VarDecl name attr ty) = ps ty
  where ps (FunctionType ty attrs) = f ty
        ps _ = unsupported ty
        f (FunType resType argTypes _) = map paramDecl argTypes
        f (FunTypeIncomplete ty) = unsupported ty

getPos :: NodeInfo -> AST.Location
getPos n = 
  let pos = posOfNode n
    in AST.FileLocation 
        (BU.fromString $ posFile pos)
        (fromIntegral $ posRow pos)
        (fromIntegral $ posColumn pos)