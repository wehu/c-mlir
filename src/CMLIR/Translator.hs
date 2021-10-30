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
import qualified MLIR.AST.Dialect.MemRef as MemRef
import qualified CMLIR.Dialect.MemRef as MemRef

import Language.C.Syntax.AST
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Analysis.ConstEval
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
                labels :: M.Map String BU.ByteString,
                vars :: M.Map String (BU.ByteString, AST.Type, Bool),
                idCounter :: Int}

type EnvM = TravT Env Identity

initEnv = Env{decls = [],
              objDefs = [],
              funDefs = [],
              enumerators = [],
              typeDefs = [],
              labels = M.empty,
              vars = M.empty,
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

addVar name v =
  modifyUserState (\s -> s{vars=M.insert name v (vars s)})

lookupVar name = do
  v <- M.lookup name . vars <$> getUserState
  case v of
    Just v -> return v
    Nothing -> error $ "cannot find variable " ++ name

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
                              modifyUserState (\s -> s{funDefs=[]})
                              withExtDeclHandler (analyseAST tu) handleFDef
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
transFunction f@(FunDef var stmt node) = underScope $ do
  let (name, ty) = varDecl var
      args = over (traverse . _1) BU.fromString $ params var
  mapM_ (uncurry addVar) [(BU.toString $ a ^._1, (a ^._1, a^._2, False)) | a <- args]
  b <- transBlock args stmt
  return $ AST.Do $ AST.FuncOp (getPos node) (BU.fromString name) ty $ AST.Region [b]

transBlock :: [(AST.Name, AST.Type)] -> CStatement NodeInfo -> EnvM AST.Block
transBlock args (CCompound labels items _) = do
  id <- freshName
  -- let lnames = map identName labels
  ops <- join <$> mapM transBlockItem items
  -- forM_ lnames (`addLabel` id)
  return $ AST.Block id args (ops ^..traverse._Left)
transBlock args s = unsupported s

transBlockItem :: CCompoundBlockItem NodeInfo -> EnvM [Either AST.Binding BU.ByteString]
transBlockItem (CBlockStmt s) = transStmt s
transBlockItem (CBlockDecl decl) = do
  modifyUserState (\s -> s{objDefs=[]})
  withExtDeclHandler (analyseDecl True decl) handleVDecl
  getUserState >>= (\s -> join <$> mapM transLocalDecl (objDefs s))
transBlockItem s = unsupported s

transLocalDecl :: ObjDef -> EnvM [Either AST.Binding BU.ByteString]
transLocalDecl (ObjDef var init node) = do
  id <- freshName
  let (n, t) = varDecl var
      mt = case t of
             AST.MemRefType{} -> t
             t -> AST.MemRefType [Just 1] t Nothing Nothing
      alloc = MemRef.alloca (getPos node) mt [] []
      b = Left $ id AST.:= alloc
  addVar n (id, t, True)
  return [b]

transStmt :: CStatement NodeInfo -> EnvM [Either AST.Binding BU.ByteString]
transStmt (CReturn Nothing node) =
  return [Left $ AST.Do $ Std.Return (getPos node) []]
transStmt (CReturn (Just e) node) = do
  results <- transExpr e
  let id = case last results of
             Left (n AST.:= v) -> n
             Right n -> n
             _ -> unsupported e
  return $ results ++ [Left $ AST.Do $ Std.Return (getPos node) [id]]
transStmt (CExpr (Just e) node) = transExpr e
transStmt e = unsupported e

transExpr :: CExpression NodeInfo -> EnvM [Either AST.Binding BU.ByteString]
transExpr (CConst c) = transConst c
transExpr (CVar ident node) = do
  (id, ty, isLocal) <- lookupVar (identName ident)
  if isLocal then do
    id0 <- freshName
    let c = Arith.Constant (getPos node) AST.IndexType (AST.IntegerAttr AST.IndexType 0)
        i0 = id0 AST.:= c
    id1 <- freshName
    let ld = MemRef.Load ty id [id0]
        i1 = id1 AST.:= ld 
    return [Left i0, Left i1, Right id1]
  else return [Right id]
transExpr e = unsupported e

transConst :: CConstant NodeInfo -> EnvM [Either AST.Binding BU.ByteString]
transConst (CIntConst i node) = transInt i (getPos node)
transConst (CCharConst c node) = transChar c (getPos node)
transConst (CFloatConst f node) = transFloat f (getPos node)
transConst (CStrConst s node) = transStr s (getPos node)

transInt :: CInteger -> AST.Location -> EnvM [Either AST.Binding BU.ByteString]
transInt (CInteger i _ _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 32
  return [Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral i))]

transChar :: CChar -> AST.Location -> EnvM [Either AST.Binding BU.ByteString]
transChar (CChar c _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 8
  return [Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral $ ord c))]
transChar c loc = error "unsupported chars"

transFloat :: CFloat -> AST.Location -> EnvM [Either AST.Binding BU.ByteString]
transFloat (CFloat str) loc = do
  id <- freshName
  let ty = AST.Float32Type
  return [Left $ id AST.:= Arith.Constant loc ty (AST.FloatAttr ty $ read str)]

transStr :: CString -> AST.Location -> EnvM [Either AST.Binding BU.ByteString]
transStr s@(CString str _) loc = error "xxx"

-- handlers :: DeclEvent -> EnvM ()
-- handlers e@(TagEvent tagDef) = handleTag e
-- handlers (DeclEvent identDecl) = handleIdentDecl identDecl
-- handlers e@(ParamEvent paramDecl) = handleParam e
-- handlers (LocalEvent identDecl) = handleIdentDecl identDecl
-- handlers (TypeDefEvent typeDef) = handleTypeDecl typeDef
-- handlers (AsmEvent asmBlock) = handleAsm asmBlock

handleTag :: DeclEvent -> EnvM ()
handleTag (TagEvent (CompDef compT)) = return ()
handleTag (TagEvent (EnumDef enumT)) = return ()
handleTag _ = return ()

-- handleIdentDecl :: IdentDecl -> EnvM ()
-- handleIdentDecl (Declaration decl) = modifyUserState (\s -> s{decls=decls s ++ [decl]})
-- handleIdentDecl (ObjectDef objDef) = modifyUserState (\s -> s{objDefs=objDefs s ++ [objDef]})
-- handleIdentDecl (FunctionDef funDef) = modifyUserState (\s -> s{funDefs=funDefs s ++ [funDef]})
-- handleIdentDecl (EnumeratorDef enumerator) = modifyUserState (\s -> s{enumerators=enumerators s ++ [enumerator]})

handleFDef :: DeclEvent -> EnvM ()
handleFDef (DeclEvent (FunctionDef funDef)) = modifyUserState (\s -> s{funDefs=funDefs s ++ [funDef]})
handleFDef _ = return ()

handleVDecl :: DeclEvent -> EnvM ()
handleVDecl (LocalEvent (ObjectDef objDef)) = modifyUserState (\s -> s{objDefs=objDefs s ++ [objDef]})
handleVDecl _ = return ()

handleParam :: DeclEvent -> EnvM ()
handleParam (ParamEvent p) = return ()
handleParam _ = return ()

handleTypeDecl :: DeclEvent -> EnvM ()
handleTypeDecl (TypeDefEvent typeDef) = modifyUserState (\s -> s{typeDefs=typeDefs s ++ [typeDef]})
handleTypeDecl _ = return ()

handleAsm :: DeclEvent -> EnvM ()
handleAsm (AsmEvent (CStrLit c n)) = return ()
handleAsm _ = return ()

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
type_ ty@(PtrType t quals attrs) = unsupported ty --AST.MemRefType [Nothing] (type_ t) Nothing Nothing
type_ (ArrayType t size quals attrs) =
  let s = arraySize size
   in case type_ t of
        AST.MemRefType sizes t Nothing Nothing -> AST.MemRefType (s:sizes) t Nothing Nothing
        t -> AST.MemRefType [s] t Nothing Nothing
type_ (TypeDefType (TypeDefRef ident t _) quals attrs) = type_ t

arraySize :: ArraySize -> Maybe Int
arraySize (UnknownArraySize static) = 
  error "unsupported dynamic array size"
arraySize (ArraySize static expr) = 
  case intValue expr of
    Just e -> Just $ fromIntegral e
    Nothing -> error "unsupported dynamic array size"

paramDecl :: ParamDecl -> (String, AST.Type)
paramDecl (ParamDecl var _) = varDecl var
paramDecl (AbstractParamDecl var _) = varDecl var

varDecl :: VarDecl -> (String, AST.Type)
varDecl (VarDecl name attrs ty) = (varName name, type_ ty)

params :: VarDecl -> [(String, AST.Type)]
params (VarDecl name attr ty) = f ty
  where f (FunctionType ty attrs) = ps ty
        f _ = unsupported ty
        ps (FunType resType argTypes _) = map paramDecl argTypes
        ps (FunTypeIncomplete ty) = unsupported ty

getPos :: NodeInfo -> AST.Location
getPos n =
  let pos = posOfNode n
    in AST.FileLocation
        (BU.fromString $ posFile pos)
        (fromIntegral $ posRow pos)
        (fromIntegral $ posColumn pos)