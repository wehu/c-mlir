{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
module CMLIR.Translator where

import qualified MLIR.AST.Builder as AST
import qualified MLIR.AST as AST
import qualified MLIR.Native.Pass as MLIR
import qualified MLIR.Native.ExecutionEngine as MLIR
import MLIR.AST.Serialize
import qualified Data.ByteString.UTF8 as BU
import qualified MLIR.AST.Dialect.Arith as Arith
import qualified CMLIR.Dialect.Arith as Arith
import qualified MLIR.AST.Dialect.Std as Std
import qualified CMLIR.Dialect.Std as Std
import qualified MLIR.Native as MLIR
import qualified MLIR.AST.Dialect.MemRef as MemRef
import qualified CMLIR.Dialect.MemRef as MemRef
import qualified MLIR.AST.Dialect.Affine as Affine
import qualified CMLIR.Dialect.Affine as Affine
import qualified CMLIR.Dialect.SCF as SCF

import Language.C.Syntax.AST
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.DeclAnalysis
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
                funsWithBody :: M.Map String Bool,
                enumerators :: [Enumerator],
                typeDefs :: [TypeDef],
                labels :: M.Map String BU.ByteString,
                enums :: M.Map String Integer,
                vars :: M.Map String (BU.ByteString, SType, Bool),
                kernels :: M.Map String Bool,
                idCounter :: Int}

type EnvM = TravT Env Identity

type BindingOrName = Either AST.Binding BU.ByteString

initEnv = Env{decls = [],
              objDefs = [],
              funDefs = [],
              funsWithBody = M.empty,
              enumerators = [],
              typeDefs = [],
              labels = M.empty,
              enums = M.empty,
              vars = M.empty,
              kernels = M.empty,
              idCounter = 0}

--------------------------------------------------------------------
-- Env helpers

underScope action = do
  env <- getUserState
  result <- action
  id <- idCounter <$> getUserState
  modifyUserState (const env{idCounter=id})
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

unsupported :: (Pretty a) => Position -> a -> b
unsupported pos a = error $ "unsupported:\n" ++ show (pretty a) ++ "@" ++ show pos

--------------------------------------------------------------------------
-- AST translators

-- | Helper to get the binding id
lastId :: [BindingOrName] -> BU.ByteString
lastId [] = error "no intruction"
lastId bs =
  case last bs of
    Left (n AST.:= v) -> n
    Right n -> n
    Left e -> error "unsupported"

-- | Helper to create constant zero
constIndex0 loc = Arith.Constant loc AST.IndexType (AST.IntegerAttr AST.IndexType 0)

constInt loc ty val = Arith.Constant loc ty (AST.IntegerAttr ty val)

-- | Helper to collect an array access to memref access by indices
collectIndices src indices =
      case src of
        (CIndex src' index' _) -> collectIndices src' (index':indices)
        _ -> (src, indices)

-- | Helper to convert an integer to index type if neccessary
toIndex loc i srcTy =
  if srcTy == AST.IndexType then return (Right i, i)
  else (\id -> (Left $ id AST.:= Arith.IndexCast loc AST.IndexType i, id)) <$> freshName

fromIndex loc i dstTy =
  if dstTy == AST.IndexType then return (Right i, i)
  else (\id -> (Left $ id AST.:= Arith.IndexCast loc dstTy i, id)) <$> freshName

data Options = Options {toLLVM :: Bool, dumpLoc :: Bool, jit :: Bool}

defaultOptions = Options {toLLVM = False, dumpLoc = False, jit = False}

-- | Translate c AST to MLIR
translateToMLIR :: Options -> CTranslUnit -> IO String
translateToMLIR opts tu =
   MLIR.withContext (\ctx -> do
     MLIR.registerAllDialects ctx
     nativeOp <- fromAST ctx (mempty, mempty) $ do
                   let res = runTrav initEnv $ do
                              -- record all kernels
                              recordKernelFunctions tu
                              -- analyze globals
                              withExtDeclHandler (analyseAST tu) handlers
                              -- add all enums
                              getUserState  >>= mapM_ addEnum . enumerators
                              -- add all global function declarations
                              getUserState  >>= mapM_ registerFunction . decls
                              -- translate all functions with definition body
                              fs <- getUserState >>= mapM transFunction . funDefs
                              -- add declarations for all functions without body
                              ds <- getUserState >>= mapM transGDecl . decls
                              -- generate a module
                              id <- freshName
                              return $ AST.ModuleOp $ AST.Block id [] (join ds ++ fs)
                   case res of
                     Left errs -> error $ show errs
                     Right (res, _) -> res
     check <- if toLLVM opts then do
                Just m <- MLIR.moduleFromOperation nativeOp
                MLIR.withPassManager ctx $ \pm -> do
                  MLIR.addConvertAffineToStandardPass pm
                  MLIR.addConvertSCFToStandardPass  pm
                  MLIR.addConvertMemRefToLLVMPass   pm
                  MLIR.addConvertVectorToLLVMPass   pm
                  MLIR.addConvertStandardToLLVMPass pm
                  MLIR.addConvertReconcileUnrealizedCastsPass pm
                  (== MLIR.Success) <$> MLIR.runPasses pm m
              else MLIR.verifyOperation nativeOp
     --MLIR.dump nativeOp
     unless check $ exitWith (ExitFailure 1)
     if jit opts then do
       Just m <- MLIR.moduleFromOperation nativeOp
       MLIR.withExecutionEngine m $ \(Just eng) -> do
         MLIR.withStringRef "main" $ \name -> do
           MLIR.executionEngineInvoke @() eng name []
       return ""
     else  
       BU.toString <$> (if (dumpLoc opts) then MLIR.showOperationWithLocation
                        else MLIR.showOperation) nativeOp)

emitted :: AST.Operation -> AST.Operation
emitted op = op { AST.opAttributes = AST.opAttributes op <> AST.namedAttribute "llvm.emit_c_interface" AST.UnitAttr }

-- | Add enums
addEnum :: Enumerator -> EnvM ()
addEnum (Enumerator ident e _ node) = do
  let name = identName ident
      v = fromJust $ intValue e
  modifyUserState (\s -> s{enums=M.insert name v (enums s)})

-- | Translate global declaration
transGDecl :: Decl -> EnvM [AST.Binding]
transGDecl decl@(Decl var node) = do
  let (name, (ty, sign)) = varDecl (posOf node) var
  funcs <- funsWithBody <$> getUserState
  let found = M.lookup name funcs
  if isn't _Nothing found then return []
  else
    case ty of
      AST.FunctionType argType resultTypes -> do
        let f = AST.FuncOp (getPos node) (BU.fromString name) ty $ AST.Region []
        isKernel <- M.lookup name . kernels <$> getUserState
        let f' = if isKernel ^. non False then
                   f{AST.opAttributes = AST.opAttributes f <> AST.namedAttribute "cl.kernel" (AST.BoolAttr True)}
                 else f
        return [AST.Do f'{AST.opAttributes=AST.opAttributes f' <> AST.namedAttribute "sym_visibility" (AST.StringAttr "private")}]
      _ -> unsupported (posOfNode node) decl

-- | Register all function types into env
registerFunction :: Decl -> EnvM ()
registerFunction f@(Decl var node) = do
  let (name, (ty, sign)) = varDecl (posOf node) var
  addVar name (BU.fromString name, (ty, sign), False)

-- | Translate a function to mlir AST
transFunction :: FunDef -> EnvM AST.Binding
transFunction f@(FunDef var stmt node) = do
  let (name, (ty, sign)) = varDecl (posOf node) var
      args = over (traverse . _1) BU.fromString $
               over (traverse . _2) fst $ params (posOf node) var
  modifyUserState (\s -> s{funsWithBody=M.insert name True (funsWithBody s)})
  underScope $ do
    mapM_ (uncurry addVar) [(a ^._1, (BU.fromString $ a ^._1, a^._2, False)) | a <- params (posOf node) var]
    b <- transBlock args [] stmt []
    let f = emitted $ AST.FuncOp (getPos node) (BU.fromString name) ty $ AST.Region [b]
    isKernel <- M.lookup name . kernels <$> getUserState
    let f' = if isKernel ^.non False then
               f{AST.opAttributes = AST.opAttributes f <> AST.namedAttribute "cl.kernel" (AST.BoolAttr True)}
             else f
    return $ AST.Do f'

-- | Translate a function block
transBlock :: [(AST.Name, AST.Type)] -> [AST.Binding] -> CStatement NodeInfo -> [AST.Binding] -> EnvM AST.Block
transBlock args pre (CCompound labels items node) post = do
  id <- freshName
  -- let lnames = map identName labels
  ops <- join <$> mapM transBlockItem items
  -- forM_ lnames (`addLabel` id)
  let defaultReturnOp = [AST.Do $ Std.Return (getPos node) []]
      lastOp =
       if null ops then defaultReturnOp
       else case last ops of
           Left (AST.Do (Std.Return _ _)) -> []
           _ -> defaultReturnOp
  return $ AST.Block id args (pre ++ ops ^..traverse._Left ++ (if null post then lastOp else post))
transBlock args _ s _ = unsupported (posOf s) s

-- | Translate a statement in block
transBlockItem :: CCompoundBlockItem NodeInfo -> EnvM [BindingOrName]
transBlockItem (CBlockStmt s) = transStmt s
transBlockItem (CBlockDecl decl) = do
  modifyUserState (\s -> s{objDefs=[]})
  withExtDeclHandler (analyseDecl True decl) handleLDecl
  getUserState >>= (\s -> join <$> mapM transLocalDecl (objDefs s))
transBlockItem s = unsupported (posOf s) s

-- | Translate a local variable declaration
transLocalDecl :: ObjDef -> EnvM [BindingOrName]
transLocalDecl (ObjDef var init node) = do
  let storage = declStorage var
  case storage of
    Static{} -> error $ "static is not supported" ++ show (posOf node)
    _ -> return ()
  id <- freshName
  id0 <- freshName
  initBs <- mapM transInit init
  let (n, t) = varDecl (posOf node) var
      mt = case t of
             (t@AST.MemRefType{}, _) -> t
             (t, _) -> AST.MemRefType [Just 1] t Nothing Nothing
      alloc = MemRef.alloca (getPos node) mt [] []
      b = Left $ id AST.:= alloc
      st = if isn't _Nothing initBs then
             [Left $ id0 AST.:= constIndex0 (getPos node)
             ,Left $ AST.Do $ MemRef.Store (lastId $ fromJust initBs) id [id0]]
           else []
  addVar n (id, t, True)
  return $ fromMaybe [] initBs ++ [b] ++ st

-- | Translate an initalization expression
transInit :: CInitializer NodeInfo -> EnvM [BindingOrName]
transInit (CInitExpr e node) = do
  bs <- transExpr e
  return $ bs ^._1
transInit init = unsupported (posOf init) init

-- | Translate a statement
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
transStmt (CFor (Right (CDecl [CTypeSpec (CIntType _)]
                              [(Just (CDeclr (Just ident0) [] Nothing [] _),
                                Just (CInitExpr lb@(CConst _) _),
                                Nothing)] _))
                (Just (CBinary CLeOp (CVar ident1 _) ub@(CConst _) _))
                (Just (CAssign CAddAssOp (CVar ident2 _) step@(CConst _) _))
                body node)
  -- try to translate for to affine.for
  | ident0 == ident1 && ident1 == ident2 = do
  let name = identName ident0
      loc = getPos node
  b <- underScope $ do
    let varName = BU.fromString name
        ty = AST.IntegerType AST.Signless 32
    (index, id) <- fromIndex loc varName ty
    addVar name (id, (ty, True), False)
    transBlock [(varName, AST.IndexType)]
      [b | isn't _Right index, (Left b) <- [index]]
      body
      [AST.Do $ Affine.yield (getPos node) []]
  let for = AST.Do $ Affine.for
                  (getPos node)
                  (fromIntegral $ fromJust $ intValue lb)
                  (fromIntegral $ fromJust $ intValue ub)
                  (fromIntegral $ fromJust $ intValue step)
                  $ AST.Region [b]
  return [Left for]
transStmt (CFor (Right (CDecl [CTypeSpec (CIntType _)]
                              [(Just (CDeclr (Just ident0) [] Nothing [] _),
                                Just (CInitExpr lb _),
                                Nothing)] _))
                (Just (CBinary CLeOp (CVar ident1 _) ub _))
                (Just (CAssign CAddAssOp (CVar ident2 _) step _))
                body node)
  -- try to translate for to scf.for
  | ident0 == ident1 && ident1 == ident2 = do
  let name = identName ident0
      loc = getPos node
  b <- underScope $ do
    let varName = BU.fromString name
        ty = AST.IntegerType AST.Signless 32
    (index, id) <- fromIndex loc varName ty
    addVar name (id, (ty, True), False)
    transBlock [(varName, AST.IndexType)]
      [b | isn't _Right index, (Left b) <- [index]]
      body
      [AST.Do $ SCF.yield loc []]
  (lbBs, (lbTy, _)) <- transExpr lb
  (ubBs, (ubTy, _)) <- transExpr ub
  (stepBs, (stepTy, _)) <- transExpr step
  (lbB, lbId) <- toIndex loc (lastId lbBs) lbTy
  (ubB, ubId) <- toIndex loc (lastId ubBs) ubTy
  (stepB, stepId) <- toIndex loc (lastId stepBs) stepTy
  let for = AST.Do $ SCF.for loc [] lbId ubId stepId [] $ AST.Region [b]
  return $ lbBs ++ ubBs ++ stepBs ++ [lbB, ubB, stepB, Left for]
transStmt (CFor init cond post body node) = underScope $ do
  -- try to translate for to scf.while
  let loc = getPos node
  initBs <- case init of
             Left (Just e) -> (^._1) <$> transExpr e
             Left Nothing -> return []
             Right decl -> transBlockItem (CBlockDecl decl)
  condBs <- case cond of
             Just e -> (^._1) <$> transExpr e
             Nothing -> return []
  postBs <- case post of
             Just e -> (^._1) <$> transExpr e
             Nothing -> return []
  bodyBs <- transBlock [] [] body (postBs ^..traverse._Left ++ [AST.Do $ SCF.yield loc []])
  condId <- freshName
  let while = AST.Do $ SCF.while loc [] []
              (AST.Region [AST.Block condId [] (condBs ^..traverse ._Left ++ [AST.Do $ SCF.condition loc (lastId condBs) []])])
              (AST.Region [bodyBs])
  return $ initBs ++ [Left while]
transStmt (CWhile cond body isDoWhile node) = do
  -- translate while to scf.while
  bodyBs <- if isDoWhile then do
              let (CCompound _ bs _) = body
              join <$> mapM transBlockItem bs
            else return []
  forBs <- transStmt (CFor (Left Nothing) (Just cond) Nothing body node)
  return $ bodyBs ++ forBs
transStmt (CIf cond t (Just f) node) = do
  -- translate ifelse to scf.if
  let loc = getPos node
  (condBs, _) <- transExpr cond
  tb <- underScope $ transBlock [] [] t [AST.Do $ SCF.yield loc []]
  fb <- underScope $ transBlock [] [] f [AST.Do $ SCF.yield loc []]
  let if_ = AST.Do $ SCF.ifelse loc [] (lastId condBs) (AST.Region [tb]) (AST.Region [fb])
  return $ condBs ++ [Left if_]
transStmt (CIf cond t Nothing node) = do
  -- translate if to scf.if
  let loc = getPos node
  (condBs, _) <- transExpr cond
  tb <- underScope $ transBlock [] [] t [AST.Do $ SCF.yield loc []]
  let if_ = AST.Do $ SCF.ifelse loc [] (lastId condBs) (AST.Region [tb]) (AST.Region [])
  return $ condBs ++ [Left if_]
transStmt e = unsupported (posOf e) e

-- | Translate an expression
transExpr :: CExpression NodeInfo -> EnvM ([BindingOrName], SType)
transExpr (CConst c) = transConst c
transExpr (CVar ident node) = do
  let name = identName ident
  enum <- M.lookup name . enums <$> getUserState
  case enum of
    Just enum -> do
      id <- freshName
      let ty = AST.IntegerType AST.Signless 32
      return ([Left $ id AST.:= constInt (getPos node) ty (fromInteger enum), Right id], (ty, True))
    Nothing -> do
      (id, (ty, sign), isLocal) <- lookupVar name
      if isLocal then do
        id0 <- freshName
        let c = constIndex0 (getPos node)
            op0 = id0 AST.:= c
        id1 <- freshName
        let ld = MemRef.Load ty id [id0]
            op1 = id1 AST.:= ld
        return ([Left op0, Left op1, Right id1], (ty, sign))
      else return ([Right id], (ty, sign))
transExpr (CAssign op lhs rhs node) = do
  let (src, indices) = collectIndices lhs []
  (id, ty, srcBs, isLocal) <- case src of
                       CVar ident _ -> (\(a, b, c) -> (a, b, [], c)) <$> lookupVar (identName ident)
                       _ -> (\(a, b) -> (lastId a, b, a, False)) <$> transExpr src
  (rhsBs, rhsTy) <- transExpr (case op of
                      CAssignOp -> rhs
                      CMulAssOp -> CBinary CMulOp lhs rhs node
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
  if null indices then do
    id0 <- freshName
    let c = constIndex0 (getPos node)
        op0 = id0 AST.:= c
    let st = MemRef.Store rhsId id [id0]
        op1 = AST.Do st
    return (srcBs ++ rhsBs ++ [Left op0, Left op1], ty)
  else do
    indexBs <- mapM transExpr indices
    let indexIds = map lastId (indexBs ^.. traverse . _1)
    toIndices <- mapM (uncurry (toIndex (getPos node))) [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
    let (dstTy, sign) = case ty of
                  (AST.MemRefType _ ty _ _, sign) -> (ty, sign)
                  (AST.UnrankedMemRefType ty _, sign) -> (ty, sign)
                  _ -> unsupported (posOf src) src

    id0 <- freshName
    id1 <- freshName
    id2 <- freshName
    let st = case ty of
               (ty@(AST.UnrankedMemRefType _ ms), _) ->
                 if isLocal
                 then [Left $ id0 AST.:= constIndex0 (getPos node)
                      ,Left $ id1 AST.:= MemRef.Load ty id [id0]
                      ,Left $ id2 AST.:= MemRef.cast (getPos node) (AST.MemRefType [Nothing] dstTy Nothing (Just ms)) id1
                      ,Left $ AST.Do $ MemRef.Store rhsId id2 (toIndices ^.. traverse . _2)]
                 else [Left $ id2 AST.:= MemRef.cast (getPos node) (AST.MemRefType [Nothing] dstTy Nothing (Just ms)) id
                      ,Left $ AST.Do $ MemRef.Store rhsId id2 (toIndices ^.. traverse . _2)]
               _ -> [Left $ AST.Do $ MemRef.Store rhsId id (toIndices ^.. traverse . _2)]
    return (srcBs ++ rhsBs ++ join (indexBs ^.. traverse . _1) ++
            toIndices ^.. traverse . _1 ++ st, (dstTy, sign))
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
      boolTy = AST.IntegerType AST.Signless 1
      (resTy, resSign) | bop == CEqOp ||
                         bop == CNeqOp ||
                         bop == CLeOp ||
                         bop == CGrOp ||
                         bop == CLeqOp ||
                         bop == CGeqOp = (boolTy, False)
                       | otherwise = (lhsTy, lhsSign)
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
                        ) loc resTy lhsId rhsId
  return (lhsBs ++ rhsBs ++ [Left op], (resTy, resSign))
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
  (srcId, (srcTy, sign), srcBs, isLocal) <-
     case src of
       CVar ident _ -> (\(a, b, c) -> (a, b, [],c)) <$> lookupVar (identName ident)
       _ -> (\(a, b) -> (lastId a, b, a, False)) <$> transExpr src
  indexBs <- mapM transExpr indices
  let indexIds = map lastId (indexBs ^.. traverse . _1)
  toIndices <- mapM (uncurry (toIndex (getPos node))) [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
  let ty = case srcTy of
             AST.MemRefType _ ty _ _ -> ty
             AST.UnrankedMemRefType ty _ -> ty
             _ -> unsupported (posOf src) src
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  id2 <- freshName
  let ld = case srcTy of
             AST.UnrankedMemRefType ty ms ->
               if isLocal
               then [Left $ id0 AST.:= constIndex0 (getPos node)
                    ,Left $ id1 AST.:= MemRef.Load srcTy srcId [id0]
                    ,Left $ id2 AST.:= MemRef.cast (getPos node) (AST.MemRefType [Nothing] ty Nothing (Just ms)) id1
                    ,Left $ id AST.:= MemRef.Load ty id2 (toIndices ^.. traverse . _2)]
               else [Left $ id2 AST.:= MemRef.cast (getPos node) (AST.MemRefType [Nothing] ty Nothing (Just ms)) srcId
                    ,Left $ id AST.:= MemRef.Load ty id2 (toIndices ^.. traverse . _2)]
             _ -> [Left $ id AST.:= MemRef.Load ty srcId (toIndices ^.. traverse . _2)]
  return (srcBs ++ join (indexBs ^.. traverse . _1) ++
          toIndices ^.. traverse . _1 ++ ld ++ [Right id], (ty, sign))
transExpr c@(CCast t e node) = do
  (srcBs, (srcTy, srcSign)) <- transExpr e
  (dstTy, dstSign) <- type_ (posOf node) 0 <$> analyseTypeDecl t
  if srcTy == dstTy then return (srcBs, (srcTy, srcSign))
  else do
    dstId <- freshName
    id <- freshName
    let loc = getPos node
        srcId = lastId srcBs
        casts
          | isFloat srcTy && isFloat dstTy =
            [Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncF else Arith.ExtF) loc dstTy srcId]
          | isInt srcTy && isInt dstTy =
            [Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncI else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc dstTy srcId]
          | isFloat srcTy && isInt dstTy && bits srcTy == bits dstTy =
            [Left $ dstId AST.:= (if srcSign then Arith.FPToSI else Arith.FPToUI) loc (AST.IntegerType AST.Signless (bits srcTy)) srcId]
          | isFloat srcTy && isInt dstTy =
            [Left $ id AST.:= (if srcSign then Arith.FPToSI else Arith.FPToUI) loc (AST.IntegerType AST.Signless (bits srcTy)) srcId
            ,Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncI else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc dstTy id]
          | isInt srcTy && isFloat dstTy && bits srcTy == bits dstTy =
            [Left $ dstId AST.:= (if srcSign then Arith.SIToFP else Arith.UIToFP) loc (floatTy $ bits srcTy) srcId]
          | isInt srcTy && isFloat dstTy =
            [Left $ id AST.:= (if bits srcTy > bits dstTy then Arith.TruncI else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc (AST.IntegerType AST.Signless (bits dstTy)) srcId
            ,Left $ dstId AST.:= (if srcSign then Arith.SIToFP else Arith.UIToFP) loc dstTy id]
          | isMemref srcTy && isMemref dstTy =
            [Left $ dstId AST.:= MemRef.cast loc dstTy srcId]
          | otherwise = unsupported (posOf c) c
    return (srcBs ++ casts ++ [Right dstId], (dstTy, dstSign))
  where isFloat ty = case ty of
                       AST.Float16Type -> True
                       AST.Float32Type -> True
                       AST.Float64Type -> True
                       _ -> False
        floatTy bits = case bits of
                         16 -> AST.Float16Type
                         32 -> AST.Float32Type
                         64 -> AST.Float64Type
                         _ -> unsupported (posOf c) c
        isInt ty = case ty of
                     AST.IntegerType _ _ -> True
                     _ -> False
        isMemref ty = case ty of
                        AST.MemRefType{} -> True
                        AST.UnrankedMemRefType{} -> True
                        _ -> False
        bits ty = case ty of
                    AST.Float16Type -> 16
                    AST.Float32Type -> 32
                    AST.Float64Type -> 64
                    AST.IntegerType _ bs -> bs
                    _ -> unsupported (posOf c) c
transExpr (CCall (CVar ident _) args node) = do
  let name = identName ident
  (_, (ty, sign), _) <- lookupVar name
  let resTy = case ty of
                AST.FunctionType _ [resTy] -> resTy
                _ -> error "expected a function type"
  id <- freshName
  argsBs <- mapM transExpr args
  let call = id AST.:= Std.call (getPos node) resTy (BU.fromString name) (map lastId $ argsBs ^..traverse._1)
  return (join (argsBs ^..traverse._1) ++ [Left call, Right id], (resTy, sign))
transExpr (CUnary CPreIncOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (incBs, _) <- transExpr (CAssign CAddAssOp e const1 node)
  (bs, sty) <- transExpr e
  return (incBs ++ bs, sty)
transExpr (CUnary CPreDecOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (incBs, _) <- transExpr (CAssign CSubAssOp e const1 node)
  (bs, sty) <- transExpr e
  return (incBs ++ bs, sty)
transExpr (CUnary CPostIncOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (bs, sty) <- transExpr e
  (incBs, _) <- transExpr (CAssign CAddAssOp e const1 node)
  return (bs ++ incBs ++ [Right $ lastId bs], sty)
transExpr (CUnary CPostDecOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (bs, sty) <- transExpr e
  (incBs, _) <- transExpr (CAssign CSubAssOp e const1 node)
  return (bs ++ incBs ++ [Right $ lastId bs], sty)
transExpr (CUnary CPlusOp e node) = transExpr e
transExpr (CUnary CMinOp e node) = do
  let loc = getPos node
  (eBs, (eTy, eSign)) <- transExpr e
  id <- freshName
  id1 <- freshName
  let minus =
         case eTy of
          AST.IntegerType _ _ ->
            [Left $ id1 AST.:= constInt loc eTy 0
            ,Left $ id AST.:= Arith.SubI loc eTy id1 (lastId eBs)]
          _ -> [Left $ id AST.:= Arith.NegF (getPos node) eTy (lastId eBs)]
  return (eBs ++ minus ++ [Right id], (eTy, eSign))
transExpr (CUnary CNegOp e node) = do
  let loc = getPos node
  (eBs, (eTy, eSign)) <- transExpr e
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  id2 <- freshName
  let bs = [Left $ id0 AST.:= constInt loc eTy 0
            ,Left $ id1 AST.:= constInt loc eTy 1
            ,Left $ id2 AST.:= Arith.SubI loc eTy id0 id1
            ,Left $ id AST.:= Arith.XOrI loc eTy id2 (lastId eBs)]
  return (eBs ++ bs ++ [Right id], (eTy, eSign))
transExpr (CUnary CIndOp e node) = do
  (eBs, (eTy, eSign)) <- transExpr e
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  let loc = getPos node
      (resTy, ms) = case eTy of
                AST.UnrankedMemRefType t ms -> (t, ms)
                _ -> unsupported (posOf node) e
      bs = [Left $ id0 AST.:= constIndex0 loc
           ,Left $ id1 AST.:= MemRef.cast loc (AST.MemRefType [Nothing] resTy Nothing (Just ms)) (lastId eBs)
           ,Left $ id AST.:= MemRef.Load resTy id1 [id0]]
  return (eBs ++ bs ++ [Right id], (resTy, eSign))
transExpr e = unsupported (posOf e) e

-- | Translate a constant expression
transConst :: CConstant NodeInfo -> EnvM ([BindingOrName], SType)
transConst (CIntConst i node) = transInt i (getPos node)
transConst (CCharConst c node) = transChar c (getPos node)
transConst (CFloatConst f node) = transFloat f (getPos node)
transConst (CStrConst s node) = transStr s (getPos node)

-- | Translate an integer literal
transInt :: CInteger -> AST.Location -> EnvM ([BindingOrName], SType)
transInt (CInteger i _ flag) loc = do
  id <- freshName
  let bits | testFlag FlagUnsigned flag = 32
           | testFlag FlagLong flag = 64
           | testFlag FlagLongLong flag = 64
           | testFlag FlagImag flag = 32
           | otherwise = 32
      sign | testFlag FlagUnsigned flag = False
           | testFlag FlagLong flag = True
           | testFlag FlagLongLong flag = True
           | testFlag FlagImag flag = False
           | otherwise = True
      ty = AST.IntegerType AST.Signless bits
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral i))], (ty, sign))

-- | Translate a char literal
transChar :: CChar -> AST.Location -> EnvM ([BindingOrName], SType)
transChar (CChar c _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 8
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral $ ord c))], (ty, True))
transChar c loc = error "unsupported chars"

-- | Translate float literal
transFloat :: CFloat -> AST.Location -> EnvM ([BindingOrName], SType)
transFloat (CFloat str) loc = do
  id <- freshName
  let lastC = last str
      ty = case lastC of
            c | c == 'l' || c == 'L' -> AST.Float64Type
            _ -> AST.Float32Type
      str' = if lastC == 'l' || lastC == 'L' || lastC == 'f' || lastC == 'F' then init str else str
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.FloatAttr ty $ read str')], (ty, True))

-- | Translate a string literal
transStr :: CString -> AST.Location -> EnvM ([BindingOrName], SType)
transStr s@(CString str _) loc = error $ "unsupported for string " ++ str
--  id <- freshName
--  let ty = AST.VectorType [length str] (AST.IntegerType AST.Signless 8)
--      cs = AST.DenseElementsAttr ty $
--             AST.DenseUInt8 $ listArray (0 :: Int, (length str)-1) $ fromIntegral . ord <$> str
--  return ([Left $ id AST.:= Arith.Constant loc ty cs], (ty, True))

------------------------------------------------------------------------------
-- AST Handlers

recordKernelFunctions :: CTranslUnit -> EnvM ()
recordKernelFunctions (CTranslUnit decls _) = do
  forM_ decls $ \decl -> do
    case decl of
      CFDefExt (CFunDef declspecs declr oldstyle _ node) -> do
        declInfo <- analyseVarDecl' True declspecs declr oldstyle Nothing
        let (VarDeclInfo vname _ storage _ _ _) = declInfo
            name = identName $ identOfVarName vname
            kernel = case storage of
                       ClKernelSpec -> True
                       _ -> False
        modifyUserState (\s -> s{kernels=M.insert name kernel (kernels s)})
      _ -> return ()

handlers :: DeclEvent -> EnvM ()
handlers (TagEvent (CompDef compT)) = return ()
handlers (TagEvent (EnumDef (EnumType _ es _ node))) = modifyUserState (\s -> s{enumerators=enumerators s ++ es})
handlers (DeclEvent (Declaration decl)) = modifyUserState (\s -> s{decls=decls s ++ [decl]})
handlers (DeclEvent (FunctionDef funDef)) = modifyUserState (\s -> s{funDefs=funDefs s ++ [funDef]})
handlers (TypeDefEvent typeDef) = modifyUserState (\s -> s{typeDefs=typeDefs s ++ [typeDef]})
handlers (AsmEvent (CStrLit c n)) = return ()
handlers _ = return ()

handleLDecl :: DeclEvent -> EnvM ()
handleLDecl (LocalEvent (ObjectDef objDef)) = modifyUserState (\s -> s{objDefs=objDefs s ++ [objDef]})
handleLDecl _ = return ()

identName :: Ident -> String
identName (Ident ident _ _) = ident

varName :: VarName -> String
varName (VarName ident _) = identName ident
varName NoName = ""

type_ :: Position -> Int -> Type -> SType
type_ pos ms (FunctionType ty attrs) = f ty
  where f (FunType resType argTypes _) =
            (AST.FunctionType (map (\t -> paramDecl pos t ^. _2 . _1) argTypes)
                              ([t | t <- map (\t -> type_ pos ms t ^._1) [resType], t /= AST.NoneType]), type_ pos ms resType ^. _2)
        f (FunTypeIncomplete ty) = type_ pos ms ty
type_ pos ms ty@(DirectType name quals attrs) =
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
    TyFloating (id -> TyFloatN n _) -> unsupported pos ty
    TyComplex t -> let (ct, sign) = type_ pos ms (DirectType (TyFloating t) quals attrs)
                    in (AST.ComplexType ct, sign)
    TyComp ref -> unsupported pos ty
    TyEnum ref -> (AST.IntegerType AST.Signless 32, True)
    TyBuiltin _ -> unsupported pos ty
    _ -> unsupported pos ty
type_ pos ms ty@(PtrType t quals attrs) =
  let (tt, sign) = type_ pos ms t
   in (AST.UnrankedMemRefType tt (AST.IntegerAttr (AST.IntegerType AST.Signless 64) ms), sign)
type_ pos ms (ArrayType t size quals attrs) =
  let s = arraySize size
      msAttr = AST.IntegerAttr (AST.IntegerType AST.Signless 64) ms
   in case type_ pos ms t of
        (AST.MemRefType sizes t Nothing ms, sign) -> (AST.MemRefType (s:sizes) t Nothing ms, sign)
        (t, sign) -> (AST.MemRefType [s] t Nothing (Just msAttr), sign)
type_ pos ms (TypeDefType (TypeDefRef ident t _) quals attrs) = type_ pos ms t

arraySize :: ArraySize -> Maybe Int
arraySize (UnknownArraySize static) =
  error "unsupported dynamic array size"
arraySize (ArraySize static expr) =
  case intValue expr of
    Just e -> Just $ fromIntegral e
    Nothing -> error "unsupported dynamic array size"

paramDecl :: Position -> ParamDecl -> (String, SType)
paramDecl pos (ParamDecl var _) = varDecl pos var
paramDecl pos (AbstractParamDecl var _) = varDecl pos var

varDecl :: Position -> VarDecl -> (String, SType)
varDecl pos v@(VarDecl name attrs ty) = (varName name, type_ pos (memorySpace v) ty)

params :: Position -> VarDecl -> [(String, SType)]
params pos (VarDecl name attr ty) = f ty
  where f (FunctionType ty attrs) = ps ty
        f _ = unsupported pos ty
        ps (FunType resType argTypes _) = map (paramDecl pos) argTypes
        ps (FunTypeIncomplete ty) = unsupported pos ty

memorySpace :: VarDecl -> Int
memorySpace var = 
  let storage = declStorage var
   in case storage of
        (Static NoLinkage True) -> 1
        (Static NoLinkage False) -> 2
        _ -> 0

getPos :: NodeInfo -> AST.Location
getPos n =
  let pos = posOfNode n
    in AST.FileLocation
        (BU.fromString $ posFile pos)
        (fromIntegral $ posRow pos)
        (fromIntegral $ posColumn pos)