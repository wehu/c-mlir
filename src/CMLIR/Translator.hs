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
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Int
import qualified Data.Vector.Storable as V
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Map as M
import System.Exit
import Debug.Trace
import Foreign (withForeignPtr)
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

type SType = (AST.Type, Bool)

data Env = Env {decls :: [Decl],
                objDefs :: M.Map Position ObjDef,
                funDefs :: [FunDef],
                funsWithBody :: M.Map String AST.Type,
                enumerators :: [Enumerator],
                typeDefs :: [TypeDef],
                labels :: M.Map String BU.ByteString,
                enums :: M.Map String Integer,
                vars :: M.Map String (BU.ByteString, SType, Bool),
                kernels :: M.Map String Bool,
                affineDimensions :: M.Map String (Int, BU.ByteString),
                affineSymbols :: M.Map String (Int, BU.ByteString),
                isAffineScope :: Bool,
                idCounter :: Int}

type EnvM = TravT Env Identity

type BindingOrName = Either AST.Binding BU.ByteString

initEnv = Env{decls = [],
              objDefs = M.empty,
              funDefs = [],
              funsWithBody = M.empty,
              enumerators = [],
              typeDefs = [],
              labels = M.empty,
              enums = M.empty,
              vars = M.empty,
              kernels = M.empty,
              affineDimensions = M.empty,
              affineSymbols = M.empty,
              isAffineScope = False,
              idCounter = 0}

--------------------------------------------------------------------
-- Env helpers

underScope action = do
  env <- getUserState
  modifyUserState (\s -> s{isAffineScope=False})
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

addAffineDimension name id =
  modifyUserState (\s -> s{affineDimensions=M.insert name (M.size (affineDimensions s), id) (affineDimensions s)})

addAffineSymbol name id =
  modifyUserState (\s -> s{affineSymbols=M.insert name (M.size (affineSymbols s), id) (affineSymbols s)})

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

isStaticShapeMemref ty =
  case ty of
    (AST.MemRefType ds ty _ _) | all (isn't _Nothing) ds -> True
    _ -> False

applyAffineExpr :: AST.Location -> M.Map String (Int, BU.ByteString) -> M.Map String (Int, BU.ByteString)
                    -> Affine.Expr -> EnvM [BindingOrName]
applyAffineExpr loc dimensions symbols e = do
  id <- freshName
  let ds = L.sortBy (\a b -> compare (a^._2._1) (b^._2._1)) (M.toList dimensions) ^..traverse._2._2
      syms = L.sortBy (\a b -> compare (a^._2._1) (b^._2._1)) (M.toList symbols) ^..traverse._2._2
  return [Left $ id AST.:= Affine.apply loc (Affine.Map (M.size dimensions) (M.size symbols) [e]) (ds++syms), Right id]

data Options = Options {toLLVM :: Bool, dumpLoc :: Bool, jits :: [String], simplize :: Bool}

defaultOptions = Options {toLLVM = False, dumpLoc = False, jits = [], simplize = True}

-- | Translate c AST to MLIR
translateToMLIR :: Options -> CTranslUnit -> IO String
translateToMLIR opts tu =
   MLIR.withContext (\ctx -> do
     MLIR.registerAllDialects ctx
     let (ast, fs) = let res = runTrav initEnv $ do
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
                              fds <- funsWithBody <$> getUserState
                              return (AST.ModuleOp $ AST.Block id [] (join ds ++ fs), fds)
                   in case res of
                       Left errs -> error $ show errs
                       Right (res, _) -> res
     nativeOp <- fromAST ctx (mempty, mempty) ast
     check <- do
                -- run passes to llvm ir
                Just m <- MLIR.moduleFromOperation nativeOp
                MLIR.withPassManager ctx $ \pm -> do
                  when (toLLVM opts) $ do
                    MLIR.addConvertAffineToStandardPass pm
                    MLIR.addConvertSCFToStandardPass  pm
                    MLIR.addConvertMemRefToLLVMPass   pm
                    MLIR.addConvertVectorToLLVMPass   pm
                    MLIR.addConvertStandardToLLVMPass pm
                    MLIR.addConvertReconcileUnrealizedCastsPass pm
                  when (simplize opts) $ do
                    MLIR.addTransformsCanonicalizerPass pm
                  (== MLIR.Success) <$> MLIR.runPasses pm m
     --MLIR.dump nativeOp
     check <- if not (toLLVM opts) && not (simplize opts)
              then MLIR.verifyOperation nativeOp
              else return check
     unless check $ exitWith (ExitFailure 1)
     if not . null $ jits opts then do
       -- run jit
       join <$> forM (jits opts) (\fn -> do
         Just m <- MLIR.moduleFromOperation nativeOp
         evalContT $ do
           let ft = fs ^. at fn
               argSizes =
                  case ft of
                    Just ft ->
                      case ft of
                        (AST.FunctionType args results) ->
                          map sizeOfType args ++ map sizeOfType results
                        _ -> []
                    Nothing -> []
               buffer (t, size, n) = do
                 case t of
                   AST.MemRefType {} -> do
                     vec@(V.MVector _ fptr) <- V.unsafeThaw $ V.iterateN size (+1) (1 :: Int8)
                     ptr <- ContT $ withForeignPtr fptr
                     structPtr <- ContT $ MLIR.packStruct64 $
                       [MLIR.SomeStorable ptr, MLIR.SomeStorable ptr] ++ replicate (2*n+1) (MLIR.SomeStorable (0::Int64))
                     return (MLIR.SomeStorable structPtr, vec)
                   _ -> do
                     vec@(V.MVector _ fptr) <- V.unsafeThaw $ V.iterateN 0 (+1) (1 :: Int8)
                     return (MLIR.SomeStorable (0::Int64), vec) -- error "only support memref type in argument for jit"
           inputs <- mapM buffer argSizes
           (Just eng) <- ContT $ MLIR.withExecutionEngine m
           name <- ContT $ MLIR.withStringRef (BU.fromString fn)
           (Just ()) <- liftIO $ MLIR.executionEngineInvoke @() eng name (inputs ^..traverse._1)
           liftIO $ join <$> mapM (fmap show . V.unsafeFreeze) (inputs ^..traverse._2))
     else
       BU.toString <$> (if dumpLoc opts then MLIR.showOperationWithLocation
                        else MLIR.showOperation) nativeOp)

sizeOfType :: AST.Type -> (AST.Type, Int, Int)
sizeOfType ty@(AST.IntegerType _ s) = (ty, ceiling (fromIntegral s/8), 1)
sizeOfType ty@AST.IndexType = (ty, 8, 1)
sizeOfType ty@AST.Float16Type = (ty, 2, 1)
sizeOfType ty@AST.Float32Type = (ty, 4, 1)
sizeOfType ty@AST.Float64Type = (ty, 8, 1)
sizeOfType ty@(AST.MemRefType ds t _ _) =
  let size = product (ds ^..traverse._Just)
   in (ty, sizeOfType t ^._2 * size, L.length ds)
sizeOfType t = error "unsupported"

-- | Add a jit wrapper for function
emitted :: AST.Operation -> AST.Operation
emitted op = op { AST.opAttributes = AST.opAttributes op <>
                  AST.namedAttribute "llvm.emit_c_interface" AST.UnitAttr }

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
  modifyUserState (\s -> s{funsWithBody=M.insert name ty (funsWithBody s)})
  underScope $ do
    modifyUserState (\s -> s{isAffineScope = True})
    argIds <- mapM (\(n, t) -> do
                      id <- freshName
                      addVar n (id, t, False)
                      return (id, t^._1)) [(a ^._1, a ^._2) | a <- params (posOf node) var]
    indBs <- mapM (\(n, t, id) ->
                      if t == AST.IntegerType AST.Signless 32 then do
                        (indBs, indId) <- toIndex (getPos node) id t
                        addAffineSymbol n indId
                        case indBs of
                          Left indBs -> return [indBs]
                          _ -> return []
                      else return [])
                    [ (p ^._1, p ^._2._1, id ^._1) | p <- params (posOf node) var | id <- argIds]
    b <- transBlock argIds (join indBs) stmt []
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
transBlockItem (CBlockDecl (CDecl _ ds node)) = do
  join <$> mapM (\d -> do
    case d of
      (Just decl, _, _) -> do
        objDef <- M.lookup (posOf decl) . objDefs <$> getUserState
        case objDef of
          Just objDef -> transLocalDecl objDef
          Nothing -> error $ "cannot find " ++ show (posOf decl)
      _ -> error $ "unsupported " ++ show (posOf node)) ds
transBlockItem s = unsupported (posOf s) s

-- | Translate a local variable declaration
transLocalDecl :: ObjDef -> EnvM [BindingOrName]
transLocalDecl d@(ObjDef var@(VarDecl name attrs orgTy) init node) = do
  let storage = declStorage var
  case storage of
    Static{} -> error $ "static is not supported" ++ show (posOf node)
    _ -> return ()
  id <- freshName
  initBs <- mapM transInit init
  let (n, t) = varDecl (posOf node) var
      (isPtr, isConst) = case orgTy of
                (PtrType t quals _) -> (True, constant quals)
                (DirectType _ quals _) -> (False, constant quals)
                (ArrayType _ _ quals _) -> (False, constant quals)
                (TypeDefType _ quals _) -> (False, constant quals)
                _ -> (False, False)
      (mt, isAssignable) =
        if isPtr then (if isConst then t ^._1 else AST.MemRefType [] (t ^._1) Nothing Nothing, not isConst)
        else case t of
               (t@AST.MemRefType{}, _) -> (t, False)
               (t, _) -> (if isConst && isn't _Nothing initBs
                          then t else AST.MemRefType [] t Nothing Nothing, not isConst)
      (b, resId) = if isConst && isn't _Nothing initBs then
                     let id = lastId (join $ fromJust initBs)
                      in (Right id, id)
                   else (Left $ id AST.:= MemRef.alloca (getPos node) mt [] [], id)
  st <- if isn't _Nothing initBs && not isConst then do
          (^._1) <$> foldM (\(s, index) initBs -> do
                       let ds = case mt of
                                  (AST.MemRefType ds _ _ _) -> ds
                                  _ -> unsupported (posOf node) d
                       when (any (isn't _Just) ds) $ error $ "unsupported for dynamic sizes array initialization " ++ show (posOf node)
                       let shape = ds ^..traverse._Just
                           strides = tail $ L.foldl' (\s i -> (i*head s):s) [1] (reverse shape)
                       ids <- mapM (const freshName) ds
                       let consts = L.foldl' (\(s, d) id -> (s++[Left $ id AST.:= constInt (getPos node) AST.IndexType
                                           (mod (div index (strides !! d)) (shape !! d))], d+1)) ([], 0) ids
                       return (s++consts ^._1++[Left $ AST.Do $ Affine.store (getPos node) (lastId initBs) id ids], index+1))
                       ([], 0::Int)
                      (fromJust initBs)
        else return []
  addVar n (resId, t, isAssignable)
  isAffineS <- isAffineScope <$> getUserState
  indBs <- if isAffineS && isConst && isn't _Nothing initBs && t^._1 == AST.IntegerType AST.Signless 32 then do
             (indBs, indId) <- toIndex (getPos node) resId (t^._1)
             addAffineSymbol n indId
             return [indBs]
           else return []
  return $ join (fromMaybe [[]] initBs) ++ [b] ++ st ++ indBs

-- | Translate an initalization expression
transInit :: CInitializer NodeInfo -> EnvM [[BindingOrName]]
transInit (CInitExpr e node) = do
  bs <- transExpr e
  return [bs ^._1]
transInit (CInitList [] _) = return []
transInit l@(CInitList (([], init):res) node) = do
  i <- transInit init
  r <- transInit (CInitList res node)
  return $ i ++ r
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
                                Just (CInitExpr lb _),
                                Nothing)] _))
                (Just (CBinary CLeOp (CVar ident1 _) ub _))
                (Just stepE)
                body node)
  -- try to translate for to affine.for
  | ident0 == ident1 &&
    (case stepE of
      (CAssign CAddAssOp (CVar ident2 _) step _) -> ident1 == ident2
      (CUnary op (CVar ident2 _) _) | op == CPostIncOp || op == CPreIncOp -> ident1 == ident2
      _ -> False) = do
  ds <- affineDimensions <$> getUserState
  syms <- affineSymbols <$> getUserState
  let name = identName ident0
      loc = getPos node
      step = case stepE of
              (CAssign CAddAssOp (CVar ident2 _) step _) -> step
              (CUnary op (CVar ident2 _) _) | op == CPostIncOp || op == CPreIncOp ->
                CConst (CIntConst (cInteger 1) node)
              _ -> unsupported (posOf stepE) stepE
      lbAE = exprToAffineExpr ds syms lb
      ubAE = exprToAffineExpr ds syms ub
  if isn't _Nothing lbAE && isn't _Nothing ubAE then do
    lbInd <- applyAffineExpr loc ds syms $ fromJust lbAE ^._1
    ubInd <- applyAffineExpr loc ds syms $ fromJust ubAE ^._1
    b <- underScope $ do
      varName <- freshName
      let ty = AST.IntegerType AST.Signless 32
      (index, id) <- fromIndex loc varName ty
      addVar name (id, (ty, True), False)
      addAffineDimension name varName
      transBlock [(varName, AST.IndexType)]
        [b | isn't _Right index, (Left b) <- [index]]
        body
        [AST.Do $ Affine.yield (getPos node) []]
    let for = AST.Do $ Affine.for
                    (getPos node)
                    (lastId lbInd)
                    (lastId ubInd)
                    (fromIntegral $ fromJust $ intValue step)
                    $ AST.Region [b]
    return $ lbInd ++ ubInd ++ [Left for]
  else do
    b <- underScope $ do
      varName <- freshName
      let ty = AST.IntegerType AST.Signless 32
      (index, id) <- fromIndex loc varName ty
      addVar name (id, (ty, True), False)
      -- addAffineSymbol name varName
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
      (id, (ty, sign), isAssignable) <- lookupVar name
      if isAssignable then do
        id1 <- freshName
        let ld = Affine.load (getPos node) ty id []
            op1 = id1 AST.:= ld
        return ([Left op1, Right id1], (ty, sign))
      else return ([Right id], (ty, sign))
transExpr (CAssign op lhs rhs node) = do
  let (src, indices) = collectIndices lhs []
  (id, ty, srcBs, isAssignable, isDeref) <- case src of
                       CVar ident _ -> (\(a, b, c) -> (a, b, [], c, False)) <$> lookupVar (identName ident)
                       (CUnary CIndOp e node) | null indices -> (\(a, b) -> (lastId a, b, a, False, True)) <$> transExpr e
                       _ -> (\(a, b) -> (lastId a, b, a, False, True)) <$> transExpr src
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
    let c0 = [Left $ id0 AST.:= constIndex0 (getPos node) | isDeref]
        st = Affine.store (getPos node) rhsId id [id0 | isDeref]
        op1 = AST.Do st{AST.opLocation = getPos node}
    return (srcBs ++ rhsBs ++ c0 ++ [Left op1], ty)
  else do
    let (dstTy, sign) = case ty of
                  (AST.MemRefType _ ty _ _, sign) -> (ty, sign)
                  _ -> unsupported (posOf src) src
    id1 <- freshName
    st <- if isAssignable
          then ([Left $ id1 AST.:= Affine.load (getPos node) (ty^._1) id []] ++) <$> tryStore (getPos node) rhsId id1 indices
          else tryStore (getPos node) rhsId id indices
    return (srcBs ++ rhsBs ++ st, (dstTy, sign))
  where tryStore loc vId dstId indices = do
          indexBs <- mapM transExpr indices
          let indexIds = map lastId (indexBs ^.. traverse . _1)
          toIndices <- mapM (uncurry (toIndex (getPos node))) [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
          ds <- affineDimensions <$> getUserState
          syms <- affineSymbols <$> getUserState
          let affineExprs = map (exprToAffineExpr ds syms) indices
              isAffineLoad = all (isn't _Nothing) affineExprs
          if isAffineLoad then do
            let es = map fromJust affineExprs ^..traverse._1
            indBs <- mapM (applyAffineExpr loc ds syms) es
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ join indBs ++ [Left $ AST.Do $ Affine.store loc vId dstId (map lastId indBs)]
          else
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ [Left $ AST.Do (MemRef.Store vId dstId (toIndices ^.. traverse . _2)){AST.opLocation=getPos node}]
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
  (srcId, (srcTy, sign), srcBs, isAssignable) <-
     case src of
       CVar ident _ -> (\(a, b, c) -> (a, b, [],c)) <$> lookupVar (identName ident)
       _ -> (\(a, b) -> (lastId a, b, a, False)) <$> transExpr src

  let ty = case srcTy of
             AST.MemRefType _ ty _ _ -> ty
             _ -> unsupported (posOf src) src
  id <- freshName
  id1 <- freshName
  ld <- if isAssignable
        then ([Left $ id1 AST.:= Affine.load (getPos node) srcTy srcId []] ++) <$> tryLoad (getPos node) ty srcTy id id1 indices
        else tryLoad (getPos node) ty srcTy id srcId indices
  return (srcBs ++ ld ++ [Right id], (ty, sign))
  where tryLoad loc ty srcTy id srcId indices = do
          indexBs <- mapM transExpr indices
          let indexIds = map lastId (indexBs ^.. traverse . _1)
          toIndices <- mapM (uncurry (toIndex (getPos node))) [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
          ds <- affineDimensions <$> getUserState
          syms <- affineSymbols <$> getUserState
          let affineExprs = map (exprToAffineExpr ds syms) indices
              isAffineLoad = all (isn't _Nothing) affineExprs
          if isAffineLoad then do
            let es = map fromJust affineExprs ^..traverse._1
            indBs <- mapM (applyAffineExpr loc ds syms) es
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ join indBs ++ [Left $ id AST.:= Affine.load loc ty srcId (map lastId indBs)]
          else
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ [Left $ id AST.:= (MemRef.Load ty srcId (toIndices ^.. traverse . _2)){AST.opLocation=getPos node}]
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
          | isI8Memref srcTy && isStaticShapeMemref dstTy =
            [Left $ id AST.:= constIndex0 loc
            ,Left $ dstId AST.:= MemRef.view loc dstTy srcId id []]
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
                        _ -> False
        isI8Memref ty = case ty of
                          (AST.MemRefType [_] (AST.IntegerType AST.Signless 8) _ _) -> True
                          _ -> False
        bits ty = case ty of
                    AST.Float16Type -> 16
                    AST.Float32Type -> 32
                    AST.Float64Type -> 64
                    AST.IntegerType _ bs -> bs
                    _ -> unsupported (posOf c) c
transExpr c@(CCall (CVar ident _) args node) = do
  let name = identName ident
      loc = getPos node
  argsBs <- mapM transExpr args
  id <- freshName
  case name of
    "malloc" -> do
      let resTy = AST.MemRefType [Nothing] (AST.IntegerType AST.Signless 8) Nothing Nothing
          (sizeB, (sizeTy, sizeSign)) = head argsBs
      (toB, toId) <- toIndex loc (lastId sizeB) sizeTy
      let malloc = id AST.:= MemRef.alloc loc resTy [toId] []
      return (join (argsBs ^..traverse._1) ++ [toB] ++ [Left malloc, Right id], (resTy, sizeSign))
    "free" -> do
      when (L.length argsBs /= 1) $ error $ "free expected 1 arguments" ++ show (posOf node)
      let (mB, (mTy, mSign)) = head argsBs
          free = AST.Do $ MemRef.dealloc loc (lastId mB)
      return (join (argsBs ^..traverse._1) ++ [Left free], (mTy, mSign))
    "memcpy" -> do
      when (L.length argsBs /= 2) $ error $ "memcpy expected 2 arguments" ++ show (posOf node)
      let (dstB, (dstTy, dstSign)) = head argsBs
          (srcB, (srcTy, srcSign)) = argsBs !! 1
          copy = AST.Do $ MemRef.copy loc (lastId srcB) (lastId dstB)
      return (join (argsBs ^..traverse._1) ++ [Left copy], (dstTy, dstSign))
    _ -> do
      (_, (ty, sign), _) <- lookupVar name
      let resTy = case ty of
                    AST.FunctionType _ resTy -> resTy
                    _ -> error "expected a function type"
      let call = id AST.:= Std.call loc resTy (BU.fromString name) (map lastId $ argsBs ^..traverse._1)
      return (join (argsBs ^..traverse._1) ++ [Left call, Right id], (if null resTy then AST.NoneType else head resTy, sign))
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
  let loc = getPos node
      (resTy, ms) = case eTy of
                AST.MemRefType [Nothing] t _ ms -> (t, ms)
                _ -> unsupported (posOf node) e
      bs = [Left $ id0 AST.:= constIndex0 loc
           ,Left $ id AST.:= Affine.load loc resTy (lastId eBs) [id0]]
  return (eBs ++ bs ++ [Right id], (resTy, eSign))
transExpr e = unsupported (posOf e) e

data DimensionOrSymbolOrConst =
  ADimension | ASymbol | AConst
  deriving (Eq)

inferDsc :: DimensionOrSymbolOrConst -> DimensionOrSymbolOrConst -> DimensionOrSymbolOrConst
inferDsc lDsc rDsc
  | lDsc == ADimension || rDsc == ADimension = ADimension
  | lDsc == ASymbol || rDsc == ASymbol = ASymbol
  | otherwise = AConst

-- | Translate to affine Expr
exprToAffineExpr :: M.Map String (Int, BU.ByteString) -> M.Map String (Int, BU.ByteString) -> CExpr
                   -> Maybe (Affine.Expr, DimensionOrSymbolOrConst)
exprToAffineExpr ds syms (CVar ident node) =
  let name = identName ident
      d = M.lookup name ds
   in case d of
        Just (d, _) -> Just (Affine.Dimension d, ADimension)
        Nothing -> fmap (\v -> (Affine.Symbol $ v ^._1, ASymbol)) (M.lookup name syms)
exprToAffineExpr ds syms c@(CConst (CIntConst _ _)) =
  fmap (\c -> (Affine.Constant $ fromIntegral c, AConst)) (intValue c)
exprToAffineExpr ds syms (CBinary CAddOp lhs rhs node) = do
  (l, lDsc) <- exprToAffineExpr ds syms lhs
  (r, rDsc) <- exprToAffineExpr ds syms rhs
  return (Affine.Add l r, inferDsc lDsc rDsc)
exprToAffineExpr ds syms (CBinary CMulOp lhs rhs node) = do
  (l, lDsc) <- exprToAffineExpr ds syms lhs
  (r, rDsc) <- exprToAffineExpr ds syms rhs
  if lDsc == ADimension && rDsc == ADimension then Nothing
  else return (Affine.Mul l r, inferDsc lDsc rDsc)
exprToAffineExpr ds syms (CBinary op lhs rhs node)
  | op == CDivOp ||
    op == CRmdOp = do
  (l, lDsc) <- exprToAffineExpr ds syms lhs
  (r, rDsc) <- exprToAffineExpr ds syms rhs
  if rDsc == ADimension then Nothing
  else return ((case op of
              CRmdOp -> Affine.Mod
              CDivOp -> Affine.FloorDiv
              _ -> unsupported (posOf node) op) l r, inferDsc lDsc rDsc)
exprToAffineExpr _ _ _ = Nothing

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
handlers (LocalEvent (ObjectDef objDef)) = modifyUserState (\s -> s{objDefs=M.insert (posOf objDef) objDef (objDefs s)})
handlers (AsmEvent (CStrLit c n)) = return ()
handlers _ = return ()

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
    TyIntegral (id -> TyInt) ->  (AST.IntegerType AST.Signless 32, True)
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
   in (AST.MemRefType [Nothing] tt Nothing (Just $ AST.IntegerAttr (AST.IntegerType AST.Signless 64) ms), sign)
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