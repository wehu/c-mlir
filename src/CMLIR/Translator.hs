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
import qualified CMLIR.Dialect.Arith as Arith
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

type SType = (AST.Type, Bool)

data Env = Env {decls :: [Decl],
                objDefs :: [ObjDef],
                funDefs :: [FunDef],
                enumerators :: [Enumerator],
                typeDefs :: [TypeDef],
                labels :: M.Map String BU.ByteString,
                vars :: M.Map String (BU.ByteString, SType, Bool),
                idCounter :: Int}

type EnvM = TravT Env Identity

type BindingOrName = Either AST.Binding BU.ByteString

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
  let (name, (ty, sign)) = varDecl var
      args = over (traverse . _1) BU.fromString $
               over (traverse . _2) fst $ params var
  mapM_ (uncurry addVar) [(a ^._1, (BU.fromString $ a ^._1, a^._2, False)) | a <- params var]
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

transBlockItem :: CCompoundBlockItem NodeInfo -> EnvM [BindingOrName]
transBlockItem (CBlockStmt s) = transStmt s
transBlockItem (CBlockDecl decl) = do
  modifyUserState (\s -> s{objDefs=[]})
  withExtDeclHandler (analyseDecl True decl) handleLDecl
  getUserState >>= (\s -> join <$> mapM transLocalDecl (objDefs s))
transBlockItem s = unsupported s

transLocalDecl :: ObjDef -> EnvM [BindingOrName]
transLocalDecl (ObjDef var init node) = do
  id <- freshName
  let (n, t) = varDecl var
      mt = case t of
             (t@AST.MemRefType{}, _) -> t
             (t, _) -> AST.MemRefType [Just 1] t Nothing Nothing
      alloc = MemRef.alloca (getPos node) mt [] []
      b = Left $ id AST.:= alloc
  addVar n (id, t, True)
  return [b]

lastId :: [BindingOrName] -> BU.ByteString
lastId [] = error "no intruction"
lastId bs =
  case last bs of
    Left (n AST.:= v) -> n
    Right n -> n
    Left e -> error "unsupported"

transStmt :: CStatement NodeInfo -> EnvM [BindingOrName]
transStmt (CReturn Nothing node) =
  return [Left $ AST.Do $ Std.Return (getPos node) []]
transStmt (CReturn (Just e) node) = do
  (bs, ty) <- transExpr e
  let id = lastId bs
  return $ bs ++ [Left $ AST.Do $ Std.Return (getPos node) [id]]
transStmt (CExpr (Just e) node) = do
  (bs, ty) <- transExpr e
  return bs
transStmt e = unsupported e

const0 loc = Arith.Constant loc AST.IndexType (AST.IntegerAttr AST.IndexType 0)

transExpr :: CExpression NodeInfo -> EnvM ([BindingOrName], SType)
transExpr (CConst c) = transConst c
transExpr (CVar ident node) = do
  (id, (ty, sign), isLocal) <- lookupVar (identName ident)
  if isLocal then do
    id0 <- freshName
    let c = const0 (getPos node)
        op0 = id0 AST.:= c
    id1 <- freshName
    let ld = MemRef.Load ty id [id0]
        op1 = id1 AST.:= ld
    return ([Left op0, Left op1, Right id1], (ty, sign))
  else return ([Right id], (ty, sign))
transExpr (CAssign op lhs@(CVar ident _) rhs node) = do
  (id, ty, isLocal) <- lookupVar (identName ident)
  (rhsBs, rhsTy) <- transExpr (case op of
                      CAssignOp -> rhs
                      CMulAssOp -> CBinary CAddOp lhs rhs node
                      CDivAssOp -> CBinary CDivOp lhs rhs node
                      CRmdAssOp -> CBinary CRmdOp lhs rhs node
                      CAddAssOp -> CBinary CAddOp lhs rhs node
                      CSubAssOp -> CBinary CSubOp lhs rhs node
                      CShlAssOp -> CBinary CShlOp lhs rhs node
                      CShrAssOp -> CBinary CShrOp lhs rhs node
                      CAndAssOp -> CBinary CAndOp lhs rhs node
                      CXorAssOp -> CBinary CXorOp lhs rhs node
                      COrAssOp -> CBinary COrOp lhs rhs node)
  let rhsId = lastId rhsBs
  id0 <- freshName
  let c = const0 (getPos node)
      op0 = id0 AST.:= c
  id1 <- freshName
  let st = MemRef.Store rhsId id [id0]
      op1 = id1 AST.:= st
  return (rhsBs ++ [Left op0, Left op1, Right id1], ty)
transExpr (CBinary bop lhs rhs node) = do
  (lhsBs, (lhsTy, lhsSign)) <- transExpr lhs
  (rhsBs, (rhsTy, rhsSign)) <- transExpr rhs
  let lhsId = lastId lhsBs
      rhsId = lastId rhsBs
      loc = getPos node
  id <- freshName
  let isF = case lhsTy of
              AST.IntegerType _ _ -> False
              AST.IndexType -> False
              _ -> True
      op = id AST.:= (case bop of
                        CAddOp -> if isF then Arith.AddF else Arith.AddI
                        CSubOp -> if isF then Arith.SubF else Arith.SubI
                        CMulOp -> if isF then Arith.MulF else Arith.MulI
                        CDivOp -> if isF then Arith.DivF else (if lhsSign then Arith.DivSI else Arith.DivUI)
                        CRmdOp -> if isF then Arith.RemF else (if lhsSign then Arith.RemSI else Arith.RemUI)
                        CShlOp -> Arith.ShLI
                        CShrOp -> if lhsSign then Arith.ShRSI else Arith.ShRUI 
                        CAndOp -> Arith.AndI
                        COrOp -> Arith.OrI
                        CLndOp -> Arith.AndI
                        CLorOp -> Arith.OrI
                        CXorOp -> Arith.XOrI
                        CEqOp -> if isF then Arith.cmpf 1 else Arith.cmpi 0
                        CNeqOp -> if isF then Arith.cmpf 6 else Arith.cmpi 1
                        CLeOp -> if isF then Arith.cmpf 4 else (if lhsSign then Arith.cmpi 2 else Arith.cmpi 6)
                        CGrOp -> if isF then Arith.cmpf 2 else (if lhsSign then Arith.cmpi 4 else Arith.cmpi 8)
                        CLeqOp -> if isF then Arith.cmpf 5 else (if lhsSign then Arith.cmpi 3 else Arith.cmpi 7)
                        CGeqOp -> if isF then Arith.cmpf 3 else (if lhsSign then Arith.cmpi 5 else Arith.cmpi 9)
                        ) loc lhsTy lhsId rhsId
  return (lhsBs ++ rhsBs ++ [Left op], (lhsTy, lhsSign))
transExpr (CComma es _) = do
  bs <- mapM transExpr es
  let ty = last bs ^._2
  return (join (bs ^..traverse._1), ty)
transExpr (CCond cond (Just lhs) rhs node) = do
  (condBs, (condTy, condSign)) <- transExpr cond
  (lhsBs, (lhsTy, lhsSign)) <- transExpr lhs
  (rhsBs, (rhsTy, rhsSign)) <- transExpr rhs
  id <- freshName
  let sel = id AST.:= Std.Select (getPos node) lhsTy (lastId condBs) (lastId lhsBs) (lastId rhsBs)
  return (condBs ++ lhsBs ++ rhsBs ++ 
          [Left sel, Right id], (lhsTy, lhsSign))
transExpr (CIndex e index node) = do
  let (src, indices) = collectIndices e [index]
  let name = case src of
               CVar ident _ -> identName ident
               _ -> unsupported src
  (srcId, (srcTy, sign), isLocal) <- lookupVar name
  indexBs <- mapM transExpr indices
  let indexIds = map lastId (indexBs ^.. traverse . _1)
  toIndices <- mapM (toIndex (getPos node)) indexIds
  let ty = case srcTy of
             AST.MemRefType _ ty _ _ -> ty
             _ -> unsupported src
  id <- freshName
  let ld = id AST.:= MemRef.Load ty srcId (toIndices ^.. traverse . _2)
  return (join (indexBs ^.. traverse . _1) ++ 
          toIndices ^.. traverse . _1 ++ [Left ld, Right id], (ty, sign))
  where
    collectIndices src indices =
      case src of
        (CIndex src' index' _) -> collectIndices src' (index':indices)
        _ -> (src, indices)
transExpr e = unsupported e

toIndex loc i = (\id -> (Left $ id AST.:= Arith.IndexCast loc AST.IndexType i, id)) <$> freshName

transConst :: CConstant NodeInfo -> EnvM ([BindingOrName], SType)
transConst (CIntConst i node) = transInt i (getPos node)
transConst (CCharConst c node) = transChar c (getPos node)
transConst (CFloatConst f node) = transFloat f (getPos node)
transConst (CStrConst s node) = transStr s (getPos node)

transInt :: CInteger -> AST.Location -> EnvM ([BindingOrName], SType)
transInt (CInteger i _ _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 32
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral i))], (ty, True))

transChar :: CChar -> AST.Location -> EnvM ([BindingOrName], SType)
transChar (CChar c _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 8
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral $ ord c))], (ty, True))
transChar c loc = error "unsupported chars"

transFloat :: CFloat -> AST.Location -> EnvM ([BindingOrName], SType)
transFloat (CFloat str) loc = do
  id <- freshName
  let ty = AST.Float32Type
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.FloatAttr ty $ read str)], (ty, True))

transStr :: CString -> AST.Location -> EnvM ([BindingOrName], SType)
transStr s@(CString str _) loc = error $ "unsupported for string " ++ str

handleTag :: DeclEvent -> EnvM ()
handleTag (TagEvent (CompDef compT)) = return ()
handleTag (TagEvent (EnumDef enumT)) = return ()
handleTag _ = return ()

handleGDecl :: DeclEvent -> EnvM ()
handleGDecl (DeclEvent (Declaration objDef)) = return ()
handleGDecl _ = return ()

handleFDef :: DeclEvent -> EnvM ()
handleFDef (DeclEvent (FunctionDef funDef)) = modifyUserState (\s -> s{funDefs=funDefs s ++ [funDef]})
handleFDef _ = return ()

handleLDecl :: DeclEvent -> EnvM ()
handleLDecl (LocalEvent (ObjectDef objDef)) = modifyUserState (\s -> s{objDefs=objDefs s ++ [objDef]})
handleLDecl _ = return ()

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

type_ :: Type -> SType
type_ (FunctionType ty attrs) = f ty
  where f (FunType resType argTypes _) =
            (AST.FunctionType (map (\t -> paramDecl t ^. _2 . _1) argTypes)
                              (map (\t -> type_ t ^._1) [resType]), False)
        f (FunTypeIncomplete ty) = type_ ty
type_ ty@(DirectType name quals attrs) =
  case name of
    TyVoid -> (AST.NoneType, False)
    TyIntegral (id -> TyBool) -> (AST.IntegerType AST.Signless 1, False)
    TyIntegral (id -> TyChar) -> (AST.IntegerType AST.Signless 8, True)
    TyIntegral (id -> TySChar) -> (AST.IntegerType AST.Signless 8, True)
    TyIntegral (id -> TyUChar) -> (AST.IntegerType AST.Signless 8, False)
    TyIntegral (id -> TyShort) -> (AST.IntegerType AST.Signless 16, True)
    TyIntegral (id -> TyUShort) -> (AST.IntegerType AST.Signless 16, False)
    TyIntegral (id -> TyInt) -> (AST.IntegerType AST.Signless 32, True)
    TyIntegral (id -> TyUInt) -> (AST.IntegerType AST.Signless 32, False)
    TyIntegral (id -> TyInt128) -> (AST.IntegerType AST.Signless 128, True)
    TyIntegral (id -> TyUInt128) -> (AST.IntegerType AST.Signless 128, False)
    TyIntegral (id -> TyLong) -> (AST.IntegerType AST.Signless 64, True)
    TyIntegral (id -> TyULong) -> (AST.IntegerType AST.Signless 64, False)
    TyIntegral (id -> TyLLong) -> (AST.IntegerType AST.Signless 64, True)
    TyIntegral (id -> TyULLong) -> (AST.IntegerType AST.Signless 64, False)
    TyFloating (id -> TyFloat) -> (AST.Float32Type, True)
    TyFloating (id -> TyDouble) -> (AST.Float64Type, True)
    TyFloating (id -> TyLDouble) -> (AST.Float64Type, True)
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
        (AST.MemRefType sizes t Nothing Nothing, sign) -> (AST.MemRefType (s:sizes) t Nothing Nothing, sign)
        (t, sign) -> (AST.MemRefType [s] t Nothing Nothing, sign)
type_ (TypeDefType (TypeDefRef ident t _) quals attrs) = type_ t

arraySize :: ArraySize -> Maybe Int
arraySize (UnknownArraySize static) =
  error "unsupported dynamic array size"
arraySize (ArraySize static expr) =
  case intValue expr of
    Just e -> Just $ fromIntegral e
    Nothing -> error "unsupported dynamic array size"

paramDecl :: ParamDecl -> (String, SType)
paramDecl (ParamDecl var _) = varDecl var
paramDecl (AbstractParamDecl var _) = varDecl var

varDecl :: VarDecl -> (String, SType)
varDecl (VarDecl name attrs ty) = (varName name, type_ ty)

params :: VarDecl -> [(String, SType)]
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