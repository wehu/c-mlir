{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module CMLIR.Translator where

import qualified MLIR.AST.Builder as AST
import qualified MLIR.AST as AST
import qualified MLIR.AST.Serialize as AST
import qualified MLIR.Native.Pass as MLIR
import qualified MLIR.Native.ExecutionEngine as MLIR
import qualified MLIR.Native as MLIR

import qualified MLIR.AST.Dialect.Arith as Arith
import qualified MLIR.AST.Dialect.Std as Std
import qualified MLIR.AST.Dialect.Affine as Affine
import qualified MLIR.AST.Dialect.MemRef as MemRef
import qualified MLIR.AST.Dialect.LLVM as LLVM
import qualified MLIR.AST.Dialect.Vector as Vector
import qualified CMLIR.Dialect.Std as Std
import qualified CMLIR.Dialect.MemRef as MemRef
import qualified CMLIR.Dialect.Arith as Arith
import qualified CMLIR.Dialect.Affine as Affine
import qualified CMLIR.Dialect.SCF as SCF
import qualified CMLIR.Dialect.Vector as Vector
import qualified CMLIR.Dialect.Linalg as Linalg
import qualified CMLIR.Dialect.Math as Math

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
import Language.C.Data.Error
import Language.C.Pretty
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe
import Data.Int
import Data.Bifunctor
import Data.Array.IArray
import qualified Data.Vector.Storable as V
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import Foreign (withForeignPtr)

type SType = (AST.Type, Bool, Maybe SUERef)

data Env = Env {decls :: [Decl],
                objDefs :: M.Map Position ObjDef,
                funDefs :: [FunDef],
                funsWithBody :: M.Map String AST.Type,
                enumerators :: [Enumerator],
                typeDefs :: M.Map String TypeDef,
                compTypeDefs :: M.Map SUERef CompType,
                labels :: M.Map String BU.ByteString,
                enums :: M.Map String Integer,
                vars :: M.Map String (BU.ByteString, SType, Bool),
                kernels :: M.Map String Bool,
                affineDimensions :: M.Map String (Int, BU.ByteString),
                affineSymbols :: M.Map String (Int, BU.ByteString),
                affineExprs :: M.Map String CExpr,
                isAffineScope :: Bool,
                machine :: MachineDesc,
                idCounter :: Int}

type EnvM = TravT Env Identity

type BindingOrName = Either AST.Binding BU.ByteString

initEnv :: Env
initEnv = Env{decls = [],
              objDefs = M.empty,
              funDefs = [],
              funsWithBody = M.empty,
              enumerators = [],
              typeDefs = M.empty,
              compTypeDefs = M.empty,
              labels = M.empty,
              enums = M.empty,
              vars = M.empty,
              kernels = M.empty,
              affineDimensions = M.empty,
              affineSymbols = M.empty,
              affineExprs = M.empty,
              isAffineScope = False,
              machine = defaultMD{ptrSize=8*2+8+2*8}, -- memref size
              idCounter = 0}

--------------------------------------------------------------------
-- Env helpers

underScope :: EnvM b -> EnvM b
underScope action = do
  env <- getUserState
  modifyUserState (\s -> s{isAffineScope=False})
  result <- action
  id <- idCounter <$> getUserState
  modifyUserState (const env{idCounter=id})
  return result

addLabel name label =
   modifyUserState (\s -> s{labels=M.insert name label (labels s)})

addVar name v =
  modifyUserState (\s -> s{vars=M.insert name v (vars s)})

lookupVar pos name = do
  v <- M.lookup name . vars <$> getUserState
  case v of
    Just v -> return v
    Nothing -> errMsg pos $ "cannot find variable " ++ name

addAffineDimension name id =
  modifyUserState (\s -> s{affineDimensions=M.insert name (M.size (affineDimensions s), id) (affineDimensions s)})

addAffineSymbol name id =
  modifyUserState (\s -> s{affineSymbols=M.insert name (M.size (affineSymbols s), id) (affineSymbols s)})

addAffineExpr name e =
  modifyUserState (\s -> s{affineExprs=M.insert name e (affineExprs s)})

freshName :: EnvM BU.ByteString
freshName = do
  id <- idCounter <$> getUserState
  modifyUserState (\s -> s{idCounter = idCounter s + 1})
  return $ BU.fromString $ show id

unsupported :: (Pretty a, Pos a) => a -> EnvM b
unsupported a = throwTravError $ unsupportedFeature (show (pretty a)) a

errMsg :: NodeInfo -> String -> EnvM b
errMsg pos s = throwTravError $ userErr $ "error:\n" ++ s ++ "@" ++ show (posOf pos)

--------------------------------------------------------------------------
-- AST translators

-- | Helper to get the binding id
lastId :: NodeInfo -> [BindingOrName] -> BU.ByteString
lastId pos [] = error $ "no intruction@" ++ show (posOf pos)
lastId pos bs =
  case last bs of
    Left (n AST.:= v) -> n
    Right n -> n
    Left e -> error $ "unsupported@" ++ show (posOf pos)

-- | Helper to create constant zero
constIndex0 loc = Arith.Constant loc AST.IndexType (AST.IntegerAttr AST.IndexType 0)

constIndex1 loc = Arith.Constant loc AST.IndexType (AST.IntegerAttr AST.IndexType 1)

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

isMemref ty = case ty of
                AST.MemRefType{} -> True
                _ -> False

isI8Memref ty = case ty of
                  (AST.MemRefType [_] (AST.IntegerType AST.Signless 8) _ _) -> True
                  _ -> False

-- | generate affine.apply based on expression
applyAffineExpr :: AST.Location
                   -> M.Map String (Int, BU.ByteString)
                   -> M.Map String (Int, BU.ByteString)
                   -> Affine.Expr -> EnvM [BindingOrName]
applyAffineExpr loc dimensions symbols e = do
  id <- freshName
  let ds = L.sortBy (\a b -> compare (a^._2._1) (b^._2._1)) (M.toList dimensions) ^..traverse._2._2
      syms = L.sortBy (\a b -> compare (a^._2._1) (b^._2._1)) (M.toList symbols) ^..traverse._2._2
  return [Left $ id AST.:= Affine.apply loc (Affine.Map (M.size dimensions) (M.size symbols) [e]) (ds++syms), Right id]

data Options = Options {toLLVM :: Bool, dumpLoc :: Bool, jits :: [String], simplize :: Bool}

defaultOptions = Options {toLLVM = False, dumpLoc = False, jits = [], simplize = True}

-- | Translate c AST to MLIR
translateToMLIR :: Options -> CTranslUnit -> IO (Either String String)
translateToMLIR opts tu = do
  MLIR.withContext (\ctx -> do
    MLIR.registerAllDialects ctx
    let res = runTrav initEnv $ do
             -- analyze globals
             withExtDeclHandler (analyseAST tu) handlers
             -- record all kernels
             recordKernelFunctions tu
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
    case res of
      Left errs -> return $ Left $ show errs
      Right ((ast, fs), _) -> do 
        nativeOp <- AST.fromAST ctx (mempty, mempty) ast
        check <- do
                   -- run passes to llvm ir
                   Just m <- MLIR.moduleFromOperation nativeOp
                   MLIR.withPassManager ctx $ \pm -> do
                     when (toLLVM opts) $ do
                       MLIR.addConvertLinalgToStandardPass pm
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
        if check then do
          if not . null $ jits opts then do
            -- run jit
            Right . join <$> forM (jits opts) (\fn -> do
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
                          return (MLIR.SomeStorable (0::Int64), vec)
                inputs <- mapM buffer argSizes
                (Just eng) <- ContT $ MLIR.withExecutionEngine m
                name <- ContT $ MLIR.withStringRef (BU.fromString fn)
                (Just ()) <- liftIO $ MLIR.executionEngineInvoke @() eng name (inputs ^..traverse._1)
                liftIO $ join <$> mapM (fmap show . V.unsafeFreeze) (inputs ^..traverse._2))
          else
            Right . BU.toString <$> (if dumpLoc opts then MLIR.showOperationWithLocation
                                     else MLIR.showOperation) nativeOp
        else return $ Left "mlir conversion failed")

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
  (name, (ty, sign, _)) <- varDecl node var
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
      _ -> unsupported decl

-- | Register all function types into env
registerFunction :: Decl -> EnvM ()
registerFunction f@(Decl var node) = do
  (name, (ty, sign, tn)) <- varDecl node var
  addVar name (BU.fromString name, (ty, sign, tn), False)

-- | Translate a function to mlir AST
transFunction :: FunDef -> EnvM AST.Binding
transFunction f@(FunDef var stmt node) = do
  (name, (ty, sign, _)) <- varDecl node var
  modifyUserState (\s -> s{funsWithBody=M.insert name ty (funsWithBody s)})
  underScope $ do
    modifyUserState (\s -> s{isAffineScope = True})
    ps <- params node var
    argIds <- mapM (\(n, t) -> do
                      id <- freshName
                      addVar n (id, t, False)
                      return (id, t^._1)) [(a ^._1, a ^._2) | a <- ps]
    indBs <- mapM (\(n, t, id) ->
                      if t == AST.IntegerType AST.Signless 32 then do
                        (indBs, indId) <- toIndex (getPos node) id t
                        addAffineSymbol n indId
                        case indBs of
                          Left indBs -> return [indBs]
                          _ -> return []
                      else return [])
                    [ (p ^._1, p ^._2._1, id ^._1) | p <- ps | id <- argIds]
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
transBlock args _ s _ = unsupported s

-- | Translate a statement in block
transBlockItem :: CCompoundBlockItem NodeInfo -> EnvM [BindingOrName]
transBlockItem (CBlockStmt s) = transStmt s
transBlockItem (CBlockDecl (CDecl q ds node)) = do
  join <$> mapM (\d -> do
    case d of
      (Just decl, _, _) -> do
        objDef <- M.lookup (posOf decl) . objDefs <$> getUserState
        case objDef of
          Just objDef -> transLocalDecl objDef
          Nothing -> errMsg node $ "cannot find " ++ show decl
      _ -> errMsg node $ "unsupported " ++ show d) ds
transBlockItem s = unsupported s

-- | Translate a local variable declaration
transLocalDecl :: ObjDef -> EnvM [BindingOrName]
transLocalDecl d@(ObjDef var@(VarDecl name attrs orgTy) init node) = do
  let storage = declStorage var
  case storage of
    Static{} -> errMsg node "static is not supported"
    _ -> return ()
  id <- freshName
  id0 <- freshName
  initBs <- mapM transInit init
  (n, t) <- varDecl node var
  let (isPtr, isConst) = case orgTy of
                (PtrType t quals _) -> (True, constant quals)
                (DirectType _ quals _) -> (False, constant quals)
                (ArrayType _ _ quals _) -> (False, constant quals)
                (TypeDefType _ quals _) -> (False, constant quals)
                _ -> (False, False)
      (mt, isAssignable, isArray) =
        if isPtr then (if isConst then t ^._1
                       else AST.MemRefType [Nothing] (t ^._1) Nothing Nothing, not isConst, False)
        else case t of
               (t@AST.MemRefType{}, _, _) -> (t, False, True)
               (t, _, _) -> (if isConst && isn't _Nothing initBs
                          then t else AST.MemRefType [Nothing] t Nothing Nothing, not isConst, False)
      (bs, resId) | isConst && isn't _Nothing initBs =
                     let id = lastId node (join $ fromJust initBs)
                      in ([Right id], id)
                  | isArray = ([Left $ id AST.:= MemRef.alloca (getPos node) mt [] []], id)
                  | otherwise =
                     ([Left $ id0 AST.:= constIndex1 (getPos node)
                      ,Left $ id AST.:= MemRef.alloca (getPos node) mt [id0] []], id)
  st <- if isn't _Nothing initBs && not isConst then do
          -- initialize local variable
          (^._1) <$> foldM (\(s, index) initBs -> do
                       ds <- case mt of
                                  (AST.MemRefType ds _ _ _) -> return ds
                                  _ -> unsupported d
                       let shape = ds ^..traverse.non 1
                           strides = tail $ L.foldl' (\s i -> (i*head s):s) [1] (reverse shape)
                       ids <- mapM (const freshName) ds
                       let consts = L.foldl' (\(s, d) id -> (s++[Left $ id AST.:= constInt (getPos node) AST.IndexType
                                           (mod (div index (strides !! d)) (shape !! d))], d+1)) ([], 0) ids
                       return (s++consts ^._1++[Left $ AST.Do $ Affine.store (getPos node) (lastId node initBs) id ids], index+1))
                       ([], 0::Int)
                      (fromJust initBs)
        else return []
  -- add variable
  addVar n (resId, t, isAssignable)
  isAffineS <- isAffineScope <$> getUserState
  indBs <- if isAffineS && isConst && isn't _Nothing initBs && t^._1 == AST.IntegerType AST.Signless 32 then do
             (indBs, indId) <- toIndex (getPos node) resId (t^._1)
             addAffineSymbol n indId
             -- record affine expr if it is constant expression so that we can inline it in the future
             case init of
               Just (CInitExpr e _) -> addAffineExpr n e
               _ -> return ()
             return [indBs]
           else return []
  return $ join (fromMaybe [[]] initBs) ++ bs ++ st ++ indBs

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
transInit init = unsupported init

-- | Translate a statement
transStmt :: CStatement NodeInfo -> EnvM [BindingOrName]
-- translate return
transStmt (CReturn Nothing node) =
  return [Left $ AST.Do $ Std.Return (getPos node) []]
transStmt (CReturn (Just e) node) = do
  (bs, ty) <- transExpr e
  let id = lastId node bs
  return $ bs ++ [Left $ AST.Do $ Std.Return (getPos node) [id]]

-- translate expression
transStmt (CExpr (Just e) node) = do
  (bs, ty) <- transExpr e
  return bs

-- translate for
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
      -- i += 1
      (CAssign CAddAssOp (CVar ident2 _) step _) -> ident1 == ident2
      -- ++i or i++
      (CUnary op (CVar ident2 _) _) | op == CPostIncOp || op == CPreIncOp -> ident1 == ident2
      _ -> False) = do
  ds <- affineDimensions <$> getUserState
  syms <- affineSymbols <$> getUserState
  exprs <- affineExprs <$> getUserState
  let name = identName ident0
      loc = getPos node
  step <- case stepE of
              (CAssign CAddAssOp (CVar ident2 _) step _) -> return step
              (CUnary op (CVar ident2 _) _) | op == CPostIncOp || op == CPreIncOp ->
                return $ CConst (CIntConst (cInteger 1) node)
              _ -> unsupported stepE
  let lbAE = exprToAffineExpr ds syms exprs lb
      ubAE = exprToAffineExpr ds syms exprs ub
  if isn't _Nothing lbAE && isn't _Nothing ubAE then do
    lbInd <- applyAffineExpr loc ds syms $ fromJust lbAE ^._1
    ubInd <- applyAffineExpr loc ds syms $ fromJust ubAE ^._1
    b <- underScope $ do
      modifyUserState (\s -> s{isAffineScope = True})
      varName <- freshName
      let ty = AST.IntegerType AST.Signless 32
      (index, id) <- fromIndex loc varName ty
      addVar name (id, (ty, True, Nothing), False)
      addAffineDimension name varName
      transBlock [(varName, AST.IndexType)]
        [b | isn't _Right index, (Left b) <- [index]]
        body
        [AST.Do $ Affine.yield (getPos node) []]
    let for = AST.Do $ Affine.for
                    (getPos node)
                    (lastId node lbInd)
                    (lastId node ubInd)
                    (fromIntegral $ fromJust $ intValue step)
                    $ AST.Region [b]
    return $ lbInd ++ ubInd ++ [Left for]
  else do
    -- translate to scf.for
    b <- underScope $ do
      varName <- freshName
      let ty = AST.IntegerType AST.Signless 32
      (index, id) <- fromIndex loc varName ty
      addVar name (id, (ty, True, Nothing), False)
      -- addAffineSymbol name varName
      transBlock [(varName, AST.IndexType)]
        [b | isn't _Right index, (Left b) <- [index]]
        body
        [AST.Do $ SCF.yield loc []]
    (lbBs, (lbTy, _, _)) <- transExpr lb
    (ubBs, (ubTy, _, _)) <- transExpr ub
    (stepBs, (stepTy, _, _)) <- transExpr step
    (lbB, lbId) <- toIndex loc (lastId node lbBs) lbTy
    (ubB, ubId) <- toIndex loc (lastId node ubBs) ubTy
    (stepB, stepId) <- toIndex loc (lastId node stepBs) stepTy
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
              (AST.Region [AST.Block condId [] (condBs ^..traverse ._Left ++ [AST.Do $ SCF.condition loc (lastId node condBs) []])])
              (AST.Region [bodyBs])
  return $ initBs ++ [Left while]

-- translate while
transStmt (CWhile cond body isDoWhile node) = do
  -- translate while to scf.while
  bodyBs <- if isDoWhile then do
              let (CCompound _ bs _) = body
              join <$> mapM transBlockItem bs
            else return []
  forBs <- transStmt (CFor (Left Nothing) (Just cond) Nothing body node)
  return $ bodyBs ++ forBs

-- translate if else
transStmt (CIf cond t (Just f) node) = do
  -- translate ifelse to scf.if
  let loc = getPos node
  (condBs, _) <- transExpr cond
  tb <- underScope $ transBlock [] [] t [AST.Do $ SCF.yield loc []]
  fb <- underScope $ transBlock [] [] f [AST.Do $ SCF.yield loc []]
  let if_ = AST.Do $ SCF.ifelse loc [] (lastId node condBs) (AST.Region [tb]) (AST.Region [fb])
  return $ condBs ++ [Left if_]
transStmt (CIf cond t Nothing node) = do
  -- translate if to scf.if
  let loc = getPos node
  (condBs, _) <- transExpr cond
  tb <- underScope $ transBlock [] [] t [AST.Do $ SCF.yield loc []]
  let if_ = AST.Do $ SCF.ifelse loc [] (lastId node condBs) (AST.Region [tb]) (AST.Region [])
  return $ condBs ++ [Left if_]

-- translate scope
transStmt s@(CCompound labels items node) = do
  let loc = getPos node
  b <- underScope $ transBlock [] [] s [AST.Do $ MemRef.allocaScopeReturn loc [] []]
  return [Left $ AST.Do $ MemRef.allocaScope loc (AST.Region [b])]

-- unsupported
transStmt e = unsupported e

-- | Translate an expression
transExpr :: CExpression NodeInfo -> EnvM ([BindingOrName], SType)
-- translate constant
transExpr (CConst c) = transConst c

-- translate variable
transExpr (CVar ident node) = do
  let name = identName ident
  enum <- M.lookup name . enums <$> getUserState
  case enum of
    Just enum -> do
      id <- freshName
      let ty = AST.IntegerType AST.Signless 32
      return ([Left $ id AST.:= constInt (getPos node) ty (fromInteger enum), Right id], (ty, True, Nothing))
    Nothing -> do
      (id, (ty, sign, tn), isAssignable) <- lookupVar node name
      if isAssignable then do
        id0 <- freshName
        id1 <- freshName
        let c0 = id0 AST.:= constIndex0 (getPos node)
            ld = Affine.load (getPos node) ty id [id0]
            op1 = id1 AST.:= ld
        return ([Left c0, Left op1, Right id1], (ty, sign, tn))
      else return ([Right id], (ty, sign, tn))

-- translate assignment expression
transExpr a@(CAssign op lhs rhs node) = do
  let (src, indices) = collectIndices lhs []
  (id, ty, srcBs, isAssignable, member) <- case src of
                       CVar ident _ ->
                        (\(a, b, c) -> (a, b, [], c, Nothing)) <$> lookupVar node (identName ident)
                       CMember s member _ _ -> do
                         (a, (_, b, tn)) <- transExpr s
                         (index, ty) <- calcStructFieldIndex node tn member
                         return (lastId node a, ty, a, False, Just index)
                       (CUnary CIndOp e node) | null indices ->
                         (\(a, b) -> (lastId node a, b, a, False, Nothing)) <$> transExpr e
                       _ -> (\(a, b) -> (lastId node a, b, a, False, Nothing)) <$> transExpr src
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
  let rhsId = lastId node rhsBs
  if null indices then do
    id0 <- freshName
    let c0 = [Left $ id0 AST.:= constInt (getPos node) AST.IndexType (member ^.non 0)]
        st = Affine.store (getPos node) rhsId id [id0]
        op1 = AST.Do st{AST.opLocation = getPos node}
    return (srcBs ++ rhsBs ++ c0 ++ [Left op1], ty)
  else do
    (dstTy, sign, tn) <- case ty of
                  (AST.MemRefType _ ty _ _, sign, tn) -> return (ty, sign, tn)
                  _ -> unsupported src
    id0 <- freshName
    id1 <- freshName
    st <- if isAssignable
          then ([Left $ id0 AST.:= constIndex0 (getPos node)
                ,Left $ id1 AST.:= Affine.load (getPos node) (ty^._1) id [id0]] ++) <$>
                   tryStore (getPos node) rhsId id1 indices member
          else tryStore (getPos node) rhsId id indices member
    return (srcBs ++ rhsBs ++ st, (dstTy, sign, tn))
  where tryStore loc vId dstId indices member = do
          let fIndex:res = indices
          indexBs <- mapM transExpr ((if isn't _Nothing member then
                                     CBinary CAddOp (CConst (CIntConst (cInteger $ fromIntegral (fromJust member)) node))
                                                    fIndex node
                                     else fIndex): res)
          let indexIds = map (lastId node) (indexBs ^.. traverse . _1)
          toIndices <- mapM (uncurry (toIndex (getPos node))) [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
          ds <- affineDimensions <$> getUserState
          syms <- affineSymbols <$> getUserState
          exprs <- affineExprs <$> getUserState
          let affineExprs = map (exprToAffineExpr ds syms exprs) indices
              isAffineLoad = all (isn't _Nothing) affineExprs
          if isAffineLoad then do
            let es = map fromJust affineExprs ^..traverse._1
            indBs <- mapM (applyAffineExpr loc ds syms) es
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ join indBs ++ [Left $ AST.Do $ Affine.store loc vId dstId (map (lastId node) indBs)]
          else
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ [Left $ AST.Do (MemRef.Store vId dstId (toIndices ^.. traverse . _2)){AST.opLocation=getPos node}]

-- translate binary operations
transExpr (CBinary bop lhs rhs node) = do
  (lhsBs, (lhsTy, lhsSign, lhsTn)) <- transExpr lhs
  (rhsBs, (rhsTy, rhsSign, rhsTn)) <- transExpr rhs
  let lhsId = lastId node lhsBs
      rhsId = lastId node rhsBs
      loc = getPos node
  id <- freshName
  let isF = case lhsTy of
              AST.IntegerType _ _ -> False
              AST.IndexType -> False
              AST.VectorType _ t ->
                case t of
                  AST.IntegerType _ _ -> False
                  AST.IndexType -> False
                  _ -> True
              _ -> True
      boolTy = case lhsTy of
                 AST.VectorType ds t -> AST.VectorType ds (AST.IntegerType AST.Signless 1)
                 _ -> AST.IntegerType AST.Signless 1
      (resTy, resSign, resTn) | bop == CEqOp ||
                         bop == CNeqOp ||
                         bop == CLeOp ||
                         bop == CGrOp ||
                         bop == CLeqOp ||
                         bop == CGeqOp = (boolTy, False, Nothing)
                       | otherwise = (lhsTy, lhsSign, lhsTn)
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
  return (lhsBs ++ rhsBs ++ [Left op], (resTy, resSign, resTn))

-- translate comma expression
transExpr (CComma es _) = do
  bs <- mapM transExpr es
  let ty = last bs ^._2
  return (join (bs ^..traverse._1), ty)

-- translate select expression
transExpr (CCond cond (Just lhs) rhs node) = do
  (condBs, (condTy, condSign, condTn)) <- transExpr cond
  (lhsBs, (lhsTy, lhsSign, lhsTn)) <- transExpr lhs
  (rhsBs, (rhsTy, rhsSign, rhsTn)) <- transExpr rhs
  id <- freshName
  let sel = id AST.:= Std.Select (getPos node)
                        lhsTy (lastId node condBs)
                        (lastId node lhsBs)
                        (lastId node rhsBs)
  return (condBs ++ lhsBs ++ rhsBs ++
          [Left sel, Right id], (lhsTy, lhsSign, lhsTn))

-- translate array index acccess
transExpr (CIndex e index node) = do
  let (src, indices) = collectIndices e [index]
  (srcId, (srcTy, sign, srcTn), srcBs, isAssignable) <-
     case src of
       CVar ident _ -> (\(a, b, c) -> (a, b, [],c)) <$> lookupVar node (identName ident)
       _ -> (\(a, b) -> (lastId node a, b, a, False)) <$> transExpr src
  ty <- case srcTy of
             AST.MemRefType _ ty _ _ -> return ty
             _ -> unsupported src
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  ld <- if isAssignable
        then ([Left $ id0 AST.:= constIndex0 (getPos node)
              ,Left $ id1 AST.:= Affine.load (getPos node) srcTy srcId [id0]] ++) <$>
                tryLoad (getPos node) ty srcTy id id1 indices
        else tryLoad (getPos node) ty srcTy id srcId indices
  return (srcBs ++ ld ++ [Right id], (ty, sign, srcTn))
  where tryLoad loc ty srcTy id srcId indices = do
          indexBs <- mapM transExpr indices
          let indexIds = map (lastId node) (indexBs ^.. traverse . _1)
          toIndices <- mapM (uncurry (toIndex (getPos node)))
                        [(i, t)|i<-indexIds|t<-indexBs^..traverse._2._1]
          ds <- affineDimensions <$> getUserState
          syms <- affineSymbols <$> getUserState
          exprs <- affineExprs <$> getUserState
          let affineExprs = map (exprToAffineExpr ds syms exprs) indices
              isAffineLoad = all (isn't _Nothing) affineExprs
          if isAffineLoad then do
            let es = map fromJust affineExprs ^..traverse._1
            indBs <- mapM (applyAffineExpr loc ds syms) es
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++ join indBs ++
                       [Left $ id AST.:= Affine.load loc ty srcId (map (lastId node) indBs)]
          else
            return $ join (indexBs ^.. traverse . _1) ++
                      toIndices ^.. traverse . _1 ++
                      [Left $ id AST.:= (MemRef.Load ty srcId (toIndices ^.. traverse . _2)){AST.opLocation=getPos node}]

-- translate memory view
transExpr c@(CCast ty (CIndex e index _) node) = do
  let (src, indices) = collectIndices e [index]
  (srcId, (srcTy, srcSign, srcTn), srcBs, isAssignable) <-
     case src of
       CVar ident _ -> (\(a, b, c) -> (a, b, [],c)) <$> lookupVar node (identName ident)
       _ -> (\(a, b) -> (lastId node a, b, a, False)) <$> transExpr src
  (dstTy, dstSign, dstTn) <- analyseTypeDecl ty >>= type_ node 0
  if srcTy == dstTy then return (srcBs, (srcTy, srcSign, srcTn))
  else do
    unless (isStaticShapeMemref dstTy) $
      errMsg node "expected a static shape as dst type"
    when (isJust $ AST.memrefTypeLayout srcTy) $
      errMsg node "cast's src type should not has affine map"
    id <- freshName
    id0 <- freshName
    id1 <- freshName
    (subview, resTy) <- if isAssignable
        then first ( [Left $ id0 AST.:= constIndex0 (getPos node),
                      Left $ id1 AST.:= Affine.load (getPos node) srcTy srcId [id0]] ++)
                             <$> subView (getPos node) dstTy srcTy id id1 indices
        else subView (getPos node) dstTy srcTy id srcId indices
    return (srcBs ++ subview ++ [Right id], (resTy, dstSign, dstTn))
  where subView loc dstTy srcTy id srcId indices = do
          offsets <- mapM transExpr indices
          offsetsBs <- mapM (uncurry (toIndex (getPos node))) [(lastId node i, t) |(i, (t,_,_))<-offsets]
          srcDims <- mapM (\i -> do id0 <- freshName
                                    id1 <- freshName
                                    return [Left $ id0 AST.:= constInt loc AST.IndexType i
                                           ,Left $ id1 AST.:= MemRef.dim loc srcId id0])
                          [0..L.length (AST.memrefTypeShape dstTy)-1]
          id0 <- freshName
          srcStridesBs <- foldM (\s d -> do
                                   id0 <- freshName
                                   return $ [Left $ id0 AST.:= Arith.MulI loc AST.IndexType (lastId node (head s)) (lastId node d)]:s)
                                   [[Left $ id0 AST.:= constIndex1 loc]] $ reverse $ tail srcDims
          id1 <- freshName
          offsetBs <- foldM (\ast (offBs, strideBs) -> do
                                id0 <- freshName
                                id1 <- freshName
                                let res = [Left $ id0 AST.:= Arith.MulI loc AST.IndexType (lastId node offBs) (lastId node strideBs)
                                          ,Left $ id1 AST.:= Arith.AddI loc AST.IndexType (lastId node (head ast)) id0]
                                return $ res:ast)
                                [[Left $ id1 AST.:= constIndex0 loc]]
                                [([offset], stride) | offset <- offsetsBs^..traverse._1|stride <- srcStridesBs]
          let dstSizes = map fromJust (AST.memrefTypeShape dstTy)
              dstStrides = L.foldl' (\s i -> i * head s : s) [1] $ reverse $ tail dstSizes
          dstStridesBs <- mapM (\s -> (\id -> [Left $ id AST.:= constInt loc AST.IndexType s]) <$> freshName) dstStrides
          let rank = L.length dstSizes
              layout = AST.AffineMapAttr (Affine.Map rank rank
                            [L.foldl' Affine.Add (Affine.Add (Affine.Dimension (rank-1)) (Affine.Symbol 0))
                               [Affine.Mul (Affine.Dimension d) (Affine.Symbol s) |d <- [0..rank-2] | s <- reverse [1..rank-1]]])
              resTy = dstTy{AST.memrefTypeMemorySpace=AST.memrefTypeMemorySpace srcTy, AST.memrefTypeLayout=Just layout}
              v = id AST.:= MemRef.reinterpretCast loc resTy srcId
                                [lastId node (head offsetBs)] [] (map (lastId node) $ init dstStridesBs)
                                [minBound] dstSizes (map (const minBound) (init dstStrides) ++ [last dstStrides])
          return (join (offsets ^..traverse._1) ++ (offsetsBs ^..traverse._1) ++
                  join srcDims ++ join (reverse srcStridesBs) ++
                  join (reverse offsetBs) ++ join dstStridesBs ++ [Left v], resTy)

-- translate casting experssion
transExpr c@(CCast t e node) = do
  (srcBs, (srcTy, srcSign, srcTn)) <- transExpr e
  (dstTy, dstSign, dstTn) <- analyseTypeDecl t >>= type_ node 0
  if srcTy == dstTy then return (srcBs, (srcTy, srcSign, srcTn))
  else do
    dstId <- freshName
    id <- freshName
    let loc = getPos node
        srcId = lastId node srcBs
        casts
          | isFloat srcTy && isFloat dstTy = do
            return [Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncF else Arith.ExtF) loc dstTy srcId]
          | isInt srcTy && isInt dstTy =
            return [Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncI
                                         else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc dstTy srcId]
          | isFloat srcTy && isInt dstTy && bits srcTy == bits dstTy =
            return [Left $ dstId AST.:= (if srcSign then Arith.FPToSI else Arith.FPToUI) loc (AST.IntegerType AST.Signless (bits srcTy)) srcId]
          | isFloat srcTy && isInt dstTy =
            return [Left $ id AST.:= (if srcSign then Arith.FPToSI else Arith.FPToUI) loc (AST.IntegerType AST.Signless (bits srcTy)) srcId
                   ,Left $ dstId AST.:= (if bits srcTy > bits dstTy then Arith.TruncI
                                         else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc dstTy id]
          | isInt srcTy && isFloat dstTy && bits srcTy == bits dstTy =
            return [Left $ dstId AST.:= (if srcSign then Arith.SIToFP else Arith.UIToFP) loc (floatTy $ bits srcTy) srcId]
          | isInt srcTy && isFloat dstTy =
            return [Left $ id AST.:= (if bits srcTy > bits dstTy then Arith.TruncI
                                      else (if srcSign then Arith.ExtSI else Arith.ExtUI)) loc (AST.IntegerType AST.Signless (bits dstTy)) srcId
                   ,Left $ dstId AST.:= (if srcSign then Arith.SIToFP else Arith.UIToFP) loc dstTy id]
          | isI8Memref srcTy && isMemref dstTy = do
            -- char * to others
            when (isJust $ AST.memrefTypeLayout srcTy) $
              errMsg node "cast's src type should not has affine map"
            dstTy <- return dstTy{AST.memrefTypeMemorySpace=AST.memrefTypeMemorySpace srcTy}
            let ds = AST.memrefTypeShape dstTy
            sizes <- foldM (\(s, index) d ->
                              if isn't _Just d then do
                                id0 <- freshName
                                id1 <- freshName
                                return (s ++ [[Left $ id0 AST.:= constInt loc AST.IndexType index
                                              ,Left $ id1 AST.:= MemRef.dim loc srcId id0]], index+1)
                              else return (s, index+1)) ([], 0::Int) ds
            return $ join (sizes ^._1) ++ [Left $ id AST.:= constIndex0 loc
                   ,Left $ dstId AST.:= MemRef.view loc dstTy srcId id (map (lastId node) (sizes ^._1))]
          | isMemref srcTy && isMemref dstTy = do
            when (isJust $ AST.memrefTypeLayout srcTy) $ errMsg node "cast's src type should not has affine map"
            dstTy <- return dstTy{AST.memrefTypeMemorySpace=AST.memrefTypeMemorySpace srcTy}
            let srcRank = L.length $ AST.memrefTypeShape srcTy
                dstRank = L.length $ AST.memrefTypeShape dstTy
            if srcRank /= dstRank then do
              if AST.memrefTypeShape dstTy == [Nothing] then do
                size <- foldM (\(s, index) d -> do
                                id0 <- freshName
                                id1 <- freshName
                                id2 <- freshName
                                return (s ++ [Left $ id0 AST.:= constInt loc AST.IndexType index
                                              ,Left $ id1 AST.:= MemRef.dim loc srcId id0
                                              ,Left $ id2 AST.:= Arith.MulI loc AST.IndexType id1 (lastId node s)], index+1))
                                              ([Left $ id AST.:= constInt loc AST.IndexType 1], 0::Int)
                                              (AST.memrefTypeShape srcTy)
                id0 <- freshName
                id1 <- freshName
                let shape = id0 AST.:= MemRef.alloca loc (AST.MemRefType [Just 1] AST.IndexType Nothing Nothing) [] []
                    c0 = id1 AST.:= constIndex0 loc
                    st = AST.Do $ Affine.store loc (lastId node (size ^._1)) id0 [id1]
                return $ size^._1 ++ [Left shape, Left c0, Left st, Left $ dstId AST.:= MemRef.reshape loc dstTy srcId id0]
              else if all (isn't _Nothing) (AST.memrefTypeShape dstTy) then do
                sizes <- mapM (\d -> do
                                id0 <- freshName
                                return [Left $ id0 AST.:= constInt loc AST.IndexType (fromJust d)])
                                (AST.memrefTypeShape dstTy)
                id0 <- freshName
                let shape = id0 AST.:= MemRef.alloca loc (AST.MemRefType [Just dstRank] AST.IndexType Nothing Nothing) [] []
                st <- foldM (\(s, index) size -> do
                              id1 <- freshName
                              let c = id1 AST.:= constInt loc AST.IndexType index
                              return (s ++ [Left c, Left $ AST.Do $ Affine.store loc (lastId node size) id0 [id1]], index+1))
                              ([], 0::Int) sizes
                return $ join sizes ++ [Left shape] ++ st ^._1 ++ [Left $ dstId AST.:= MemRef.reshape loc dstTy srcId id0]
              else unsupported c
            else return [Left $ dstId AST.:= MemRef.cast loc dstTy srcId]
          | otherwise = unsupported c
    casts <- casts
    return (srcBs ++ casts ++ [Right dstId], (dstTy, dstSign, dstTn))
  where isFloat ty = case ty of
                       AST.Float16Type -> True
                       AST.Float32Type -> True
                       AST.Float64Type -> True
                       _ -> False
        floatTy bits = case bits of
                         16 -> AST.Float16Type
                         32 -> AST.Float32Type
                         64 -> AST.Float64Type
                         _ -> AST.Float32Type
        isInt ty = case ty of
                     AST.IntegerType _ _ -> True
                     _ -> False
        bits ty = case ty of
                    AST.Float16Type -> 16
                    AST.Float32Type -> 32
                    AST.Float64Type -> 64
                    AST.IntegerType _ bs -> bs
                    _ -> 32

-- translate builtin dma_start
transExpr c@(CCall (CVar ident _) [src', dst', tag', size] node) | identName ident == "dma_start" = do
  let loc = getPos node
      (src, srcIndices) = collectIndices src' []
      (dst, dstIndices) = collectIndices dst' []
      (tag, tagIndices) = collectIndices tag' []
  (srcBs, (srcTy, srcSign, srcTn)) <- transExpr src
  (dstBs, (dstTy, dstSign, dstTn)) <- transExpr dst
  (tagBs, (tagTy, tagSign, tagTn)) <- transExpr tag
  (sizeBs, (sizeTy, sizeSign, sizeTn)) <- transExpr size
  ds <- affineDimensions <$> getUserState
  syms <- affineSymbols <$> getUserState
  exprs <- affineExprs <$> getUserState
  (sizeIndBs, sizeId) <- toIndex loc (lastId node sizeBs) sizeTy
  let srcIndexAEs = map (exprToAffineExpr ds syms exprs) srcIndices
      dstIndexAEs = map (exprToAffineExpr ds syms exprs) dstIndices
      tagIndexAEs = map (exprToAffineExpr ds syms exprs) tagIndices
  if all (isn't _Nothing) srcIndexAEs && all (isn't _Nothing) dstIndexAEs && all (isn't _Nothing) tagIndexAEs then do
    srcInds <- mapM (applyAffineExpr loc ds syms . fst . fromJust) srcIndexAEs
    dstInds <- mapM (applyAffineExpr loc ds syms . fst . fromJust) dstIndexAEs
    tagInds <- mapM (applyAffineExpr loc ds syms . fst . fromJust) tagIndexAEs
    let dma = AST.Do $ Affine.dmaStart loc (lastId node srcBs) (map (lastId node) srcInds)
                                           (lastId node dstBs) (map (lastId node) dstInds)
                                           (lastId node tagBs) (map (lastId node) tagInds)
                                           sizeId
    return (srcBs++join srcInds++dstBs++join dstInds++tagBs++join tagInds++sizeBs++[sizeIndBs, Left dma], (dstTy, dstSign, dstTn))
  else do
    srcIndBs <- mapM transExpr srcIndices
    dstIndBs <- mapM transExpr dstIndices
    tagIndBs <- mapM transExpr tagIndices
    srcToIndex <- mapM (uncurry (toIndex loc)) [(lastId node $ id ^._1, id^._2._1)|id <- srcIndBs]
    dstToIndex <- mapM (uncurry (toIndex loc)) [(lastId node $ id ^._1, id^._2._1)|id <- dstIndBs]
    tagToIndex <- mapM (uncurry (toIndex loc)) [(lastId node $ id ^._1, id^._2._1)|id <- tagIndBs]
    let dma = AST.Do $ MemRef.dmaStart loc (lastId node srcBs) (srcToIndex ^..traverse._2)
                                           (lastId node dstBs) (dstToIndex ^..traverse._2)
                                           (lastId node tagBs) (tagToIndex ^..traverse._2)
                                           sizeId
    return (srcBs++join (srcIndBs^..traverse._1)++(srcToIndex^..traverse._1)++
            dstBs++join (dstIndBs^..traverse._1)++(dstToIndex^..traverse._1)++
            tagBs++join (tagIndBs^..traverse._1)++(tagToIndex^..traverse._1)++
            sizeBs++[sizeIndBs, Left dma], (dstTy, dstSign, dstTn))

-- translate builtin dma_wait
transExpr c@(CCall (CVar ident _) [tag', size] node) | identName ident == "dma_wait" = do
  let loc = getPos node
      (tag, tagIndices) = collectIndices tag' []
  (tagBs, (tagTy, tagSign, tagTn)) <- transExpr tag
  (sizeBs, (sizeTy, sizeSign, sizeTn)) <- transExpr size
  ds <- affineDimensions <$> getUserState
  syms <- affineSymbols <$> getUserState
  exprs <- affineExprs <$> getUserState
  (sizeIndBs, sizeId) <- toIndex loc (lastId node sizeBs) sizeTy
  let tagIndexAEs = map (exprToAffineExpr ds syms exprs) tagIndices
  if all (isn't _Nothing) tagIndexAEs then do
    tagInds <- mapM (applyAffineExpr loc ds syms . fst . fromJust) tagIndexAEs
    let dma = AST.Do $ Affine.dmaWait loc (lastId node tagBs) (map (lastId node) tagInds) sizeId
    return (tagBs++join tagInds++sizeBs++[sizeIndBs, Left dma], (tagTy, tagSign, tagTn))
  else do
    tagIndBs <- mapM transExpr tagIndices
    tagToIndex <- mapM (uncurry (toIndex loc)) [(lastId node $ id ^._1, id^._2._1)|id <- tagIndBs]
    let dma = AST.Do $ MemRef.dmaWait loc (lastId node tagBs) (tagToIndex ^..traverse._2)
                                           sizeId
    return (tagBs++join (tagIndBs^..traverse._1)++(tagToIndex^..traverse._1)++
            sizeBs++[sizeIndBs, Left dma], (tagTy, tagSign, tagTn))

-- translate builtin vload
transExpr c@(CCall (CVar ident _) [src', dst] node) | identName ident == "vload" = do
  let loc = getPos node
      (src, indices) = collectIndices src' []
  (srcBs, (srcTy, srcSign, srcTn)) <- transExpr src
  indexBs <- mapM transExpr indices
  toIndex <- mapM (\(b, (ty, _, _)) -> toIndex loc (lastId node b) ty) indexBs
  (dstBs, (dstTy, dstSign, dstTn)) <- transExpr dst
  ty <- case dstTy of
             AST.MemRefType [Nothing] t _ _ -> return t
             _ -> errMsg node "vload expected a pointer to vector type"
  id <- freshName
  id0 <- freshName
  let load = id AST.:= Vector.vload loc ty (lastId node srcBs) (toIndex ^..traverse._2)
      c0 = id0 AST.:= constIndex0 loc
      st = AST.Do $ Affine.store loc id (lastId node dstBs) [id0]
  return (srcBs ++ join (indexBs ^..traverse._1) ++ toIndex ^..traverse._1++dstBs++
          [Left load, Left c0, Left st, Right id], (ty, dstSign, dstTn))
transExpr c@(CCall (CVar ident _) [v, dst'] node) | identName ident == "vstore" = do
  let loc = getPos node
      (dst, indices) = collectIndices dst' []
  (dstBs, (dstTy, dstSign, dstTn)) <- transExpr dst
  indexBs <- mapM transExpr indices
  toIndex <- mapM (\(b, (ty, _, _)) -> toIndex loc (lastId node b) ty) indexBs
  (vBs, (vTy, vSign, vTn)) <- transExpr v
  let store = AST.Do $ Vector.vstore loc (lastId node vBs) (lastId node dstBs) (toIndex ^..traverse._2)
  return (dstBs ++ join (indexBs ^..traverse._1) ++ toIndex ^..traverse._1++ vBs ++ [Left store], (vTy, vSign, vTn))

-- translate function call
transExpr c@(CCall (CVar ident _) args node) = do
  let name = identName ident
      loc = getPos node
  argsBs <- mapM transExpr args
  id <- freshName
  case name of
    -- builtin functions
    "malloc" -> do
      let resTy = AST.MemRefType [Nothing] (AST.IntegerType AST.Signless 8) Nothing Nothing
          (sizeB, (sizeTy, sizeSign, sizeTn)) = head argsBs
      (toB, toId) <- toIndex loc (lastId node sizeB) sizeTy
      let malloc = id AST.:= MemRef.alloc loc resTy [toId] []
      return (join (argsBs ^..traverse._1) ++ [toB] ++ [Left malloc, Right id], (resTy, sizeSign, sizeTn))
    "free" -> do
      when (L.length argsBs /= 1) $ errMsg node "free expected 1 arguments"
      let (mB, (mTy, mSign, mTn)) = head argsBs
          free = AST.Do $ MemRef.dealloc loc (lastId node mB)
      return (join (argsBs ^..traverse._1) ++ [Left free], (mTy, mSign, mTn))
    "memcpy" -> do
      when (L.length argsBs /= 2) $ errMsg node "memcpy expected 2 arguments"
      let (dstB, (dstTy, dstSign, dstTn)) = head argsBs
          (srcB, (srcTy, srcSign, srcTn)) = argsBs !! 1
          copy = AST.Do $ MemRef.copy loc (lastId node srcB) (lastId node dstB)
      return (join (argsBs ^..traverse._1) ++ [Left copy], (dstTy, dstSign, dstTn))

    -- convolution
    "conv_1d_nwc_wcf" -> do
      attrs <- getAttributes name (drop 3 args)
      convLikeFunc name loc Linalg.conv1dNwcWcf (take 3 argsBs) attrs 2
    "conv_1d" -> convLikeFunc name loc Linalg.conv1d argsBs [] 0
    "conv_2d_nchw_fchw" -> do
      attrs <- getAttributes name (drop 3 args)
      convLikeFunc name loc Linalg.conv2dNchwFchw (take 3 argsBs) attrs 4
    "conv_2d_nhwc_hwcf" -> do
      attrs <- getAttributes name (drop 3 args)
      convLikeFunc name loc Linalg.conv2dNhwcHwcf (take 3 argsBs) attrs 4
    "conv_2d" -> convLikeFunc name loc Linalg.conv2d argsBs [] 0

    -- matmul
    "matmul" -> convLikeFunc name loc Linalg.matmul argsBs [] 0
    "transpose" -> do
      when (null argsBs) $ errMsg node "transpose expected >= 1 arguments"
      let (mB, (mTy, mSign, mTn)) = head argsBs
      unless (isStaticShapeMemref mTy) $ errMsg node "transpose expected a array as argument"
      when (isJust $ AST.memrefTypeLayout mTy) $ errMsg node "transpose's src type should not has affine map"
      indicesBs <- mapM (\_-> (\id -> [Left $ id AST.:= constIndex0 loc]) <$> freshName) [0..L.length (AST.memrefTypeShape mTy)-1]
      id0 <- freshName
      id1 <- freshName
      id2 <- freshName
      let permute = map intValue $ tail args
      when (L.length permute /= L.length (AST.memrefTypeShape mTy)) $ errMsg node "permute mismatch with shape"
      when (any (isn't _Just) permute) $ errMsg node "transpose expected constant int as permute"
      let vTy = AST.VectorType (map fromJust (AST.memrefTypeShape mTy)) (AST.memrefTypeElement mTy)
          v = id0 AST.:= Vector.vload loc vTy (lastId node mB) (map (lastId node) indicesBs)
          shape = [AST.memrefTypeShape mTy !! fromIntegral (fromJust i) | i <- permute]
          vecTy = AST.VectorType (map fromJust shape) (AST.memrefTypeElement mTy)
          transp = id1 AST.:= Vector.vtranspose loc vecTy id0 (map (fromIntegral.fromJust) permute)
          dstTy = mTy{AST.memrefTypeShape=shape}
          dstM = id2 AST.:= MemRef.reinterpretCast loc dstTy (lastId node mB) [] [] [] [0] (map fromJust shape)
                      (L.foldl' (\s i -> i*head s:s) [1] $ reverse $ tail $ map fromJust shape)
          st = AST.Do $ Vector.vstore loc id1 id2 (map (lastId node) indicesBs)
      return (join (argsBs ^..traverse._1) ++ join indicesBs ++ [Left v, Left transp, Left dstM, Left st, Right id2], (dstTy, mSign, mTn))

    -- math
    "abs"   -> builtinFunc name loc id Math.abs argsBs 1
    "atan2" -> builtinFunc name loc id Math.atan2 argsBs 2
    "atan"  -> builtinFunc name loc id Math.atan argsBs 1
    "ceil"  -> builtinFunc name loc id Math.ceil argsBs 1
    "cos"   -> builtinFunc name loc id Math.cos argsBs 1
    "erf"   -> builtinFunc name loc id Math.erf argsBs 1
    "exp2"  -> builtinFunc name loc id Math.exp2 argsBs 1
    "expm1" -> builtinFunc name loc id Math.expm1 argsBs 1
    "exp"   -> builtinFunc name loc id Math.exp argsBs 1
    "floor" -> builtinFunc name loc id Math.floor argsBs 1
    "fma"   -> builtinFunc name loc id Math.fma argsBs 3
    "log10" -> builtinFunc name loc id Math.log10 argsBs 1
    "log1p" -> builtinFunc name loc id Math.log1p argsBs 1
    "log2"  -> builtinFunc name loc id Math.log2 argsBs 1
    "log"   -> builtinFunc name loc id Math.log argsBs 1
    "powf"  -> builtinFunc name loc id Math.powf argsBs 2
    "rsqrt" -> builtinFunc name loc id Math.rsqrt argsBs 1
    "sin"   -> builtinFunc name loc id Math.sin argsBs 1
    "sqrt"  -> builtinFunc name loc id Math.sqrt argsBs 1
    "tanh"  -> builtinFunc name loc id Math.tanh argsBs 1

    -- normal function call
    _ -> do
      (_, (ty, sign, tn), _) <- lookupVar node name
      resTy <- case ty of
                    AST.FunctionType _ resTy -> return resTy
                    _ -> errMsg node "expected a function type"
      let call = id AST.:= Std.call loc resTy (BU.fromString name) (map (lastId node) $ argsBs ^..traverse._1)
      return (join (argsBs ^..traverse._1) ++ [Left call, Right id], (if null resTy then AST.NoneType else head resTy, sign, tn))
  where getAttributes name args =
          mapM (\v -> case intValue v of
                               Just v -> return $ fromIntegral v
                               Nothing -> errMsg node $ name ++ " expected int constant " ++ show v) args
        builtinFunc name loc id op argsBs n = do
          when (L.length argsBs /= n) $ errMsg node $ name ++ " expected " ++ show n ++ " arguments"
          let (aB, (aTy, aSign, aTn)) = head argsBs
              ast = id AST.:= op loc aTy (map (lastId node) $ argsBs ^..traverse._1)
          return (join (argsBs ^..traverse._1) ++ [Left ast, Right id], (aTy, aSign, aTn))
        convLikeFunc name loc op argsBs attrs n = do
          when (L.length attrs /= n) $ errMsg node $ name ++ " expected " ++ show n ++ " attributes"
          when (L.length argsBs /= 3) $ errMsg node $ name ++ " expected 3 arguments"
          unless (all (\case
                       AST.MemRefType{} -> True
                       _ -> False) (argsBs ^..traverse._2._1)) $
                errMsg node $ name ++ " expected array as arguments"
          id0 <- freshName
          id1 <- freshName
          id2 <- freshName
          id3 <- freshName
          let (lhsB, (lhsTy, lhsSign, lhsTn)) = head argsBs
              (rhsB, (rhsTy, rhsSign, rhsTn)) = argsBs !! 1
              (outputB, (outputTy, outputSign, outputTn)) = argsBs !! 2
          b <- underScope $ do
            let lhsN = BU.toString id1
                rhsN = BU.toString id2
                outputN = BU.toString id3
            addVar lhsN (id1, (AST.memrefTypeElement lhsTy, lhsSign, lhsTn), False)
            addVar rhsN (id2, (AST.memrefTypeElement rhsTy, rhsSign, rhsTn), False)
            addVar outputN (id3, (AST.memrefTypeElement outputTy, outputSign, outputTn), False)
            transExpr (CBinary CAddOp (CVar (Ident outputN (read outputN) node) node)
                               (CBinary CMulOp (CVar (Ident lhsN (read lhsN) node) node)
                                               (CVar (Ident rhsN (read rhsN) node) node) node) node)
          let ast = AST.Do $ op loc (lastId node lhsB) (lastId node rhsB) (lastId node outputB) attrs
                                 (AST.Block id0 [(id1, AST.memrefTypeElement lhsTy),
                                                 (id2, AST.memrefTypeElement rhsTy),
                                                 (id3, AST.memrefTypeElement outputTy)]
                                                (b^._1 ^..traverse._Left ++
                                                [AST.Do $ Linalg.yield2 loc [lastId node $ b^._1]]))
          return (join (argsBs ^..traverse._1) ++ [Left ast], (outputTy, outputSign, outputTn))

-- translate ++i
transExpr (CUnary CPreIncOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (incBs, _) <- transExpr (CAssign CAddAssOp e const1 node)
  (bs, sty) <- transExpr e
  return (incBs ++ bs, sty)

-- translate --i
transExpr (CUnary CPreDecOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (incBs, _) <- transExpr (CAssign CSubAssOp e const1 node)
  (bs, sty) <- transExpr e
  return (incBs ++ bs, sty)

-- translate i++
transExpr (CUnary CPostIncOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (bs, sty) <- transExpr e
  (incBs, _) <- transExpr (CAssign CAddAssOp e const1 node)
  return (bs ++ incBs ++ [Right $ lastId node bs], sty)

-- translate i--
transExpr (CUnary CPostDecOp e node) = do
  let const1 = CConst (CIntConst (cInteger 1) node)
  (bs, sty) <- transExpr e
  (incBs, _) <- transExpr (CAssign CSubAssOp e const1 node)
  return (bs ++ incBs ++ [Right $ lastId node bs], sty)

-- translate +i
transExpr (CUnary CPlusOp e node) = transExpr e

-- translate -i
transExpr (CUnary CMinOp e node) = do
  let loc = getPos node
  (eBs, (eTy, eSign, eTn)) <- transExpr e
  id <- freshName
  id1 <- freshName
  let minus =
         case eTy of
          AST.IntegerType _ _ ->
            [Left $ id1 AST.:= constInt loc eTy 0
            ,Left $ id AST.:= Arith.SubI loc eTy id1 (lastId node eBs)]
          _ -> [Left $ id AST.:= Arith.NegF (getPos node) eTy (lastId node eBs)]
  return (eBs ++ minus ++ [Right id], (eTy, eSign, eTn))

-- translate ! i
transExpr (CUnary CNegOp e node) = do
  let loc = getPos node
  (eBs, (eTy, eSign, eTn)) <- transExpr e
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  id2 <- freshName
  let bs = [Left $ id0 AST.:= constInt loc eTy 0
            ,Left $ id1 AST.:= constInt loc eTy 1
            ,Left $ id2 AST.:= Arith.SubI loc eTy id0 id1
            ,Left $ id AST.:= Arith.XOrI loc eTy id2 (lastId node eBs)]
  return (eBs ++ bs ++ [Right id], (eTy, eSign, eTn))

-- translate *v
transExpr (CUnary CIndOp e node) = do
  (eBs, (eTy, eSign, eTn)) <- transExpr e
  id <- freshName
  id0 <- freshName
  let loc = getPos node
  (resTy, ms) <- case eTy of
                AST.MemRefType [Nothing] t _ ms -> return (t, ms)
                _ -> unsupported e
  let bs = [Left $ id0 AST.:= constIndex0 loc
           ,Left $ id AST.:= Affine.load loc resTy (lastId node eBs) [id0]]
  return (eBs ++ bs ++ [Right id], (resTy, eSign, eTn))

-- translate &v
transExpr adr@(CUnary CAdrOp (CVar ident _) node) = do
  let name = identName ident
  (id, (ty, sign, tn), isAssignable)  <- lookupVar node name
  unless isAssignable $ errMsg node "& only support lvalue"
  return ([Right id], (AST.MemRefType [Nothing] ty Nothing Nothing, sign, tn))

-- translate struct member access
transExpr m@(CMember e ident _ node) = do
  (eBs, (eTy, eSign, eTn)) <- transExpr e
  (index, (resTy, resSign, resTn)) <- calcStructFieldIndex node eTn ident
  id0 <- freshName
  id1 <- freshName
  let loc = getPos node
      cIndex = id0 AST.:= constInt loc AST.IndexType index
      elem = id1 AST.:= Affine.load loc resTy (lastId node eBs) [id0]
  return (eBs ++ [Left cIndex, Left elem, Right id1], (resTy, resSign, resTn))

-- translate sizeof(type)
transExpr (CSizeofType decl node) = do
  md <- machine <$> getUserState
  t <- analyseTypeDecl decl
  s <- sizeofType md node t
  transExpr (CConst (CIntConst (cInteger s) node))

-- translate sizeof(expr)
transExpr (CSizeofExpr e node) = do
  md <- machine <$> getUserState
  t <- tExpr [] RValue e
  s <- sizeofType md node t
  transExpr (CConst (CIntConst (cInteger s) node))

-- translate alignof(type)
transExpr (CAlignofType decl node) = do
  md <- machine <$> getUserState
  t <- analyseTypeDecl decl
  s <- alignofType md node t
  transExpr (CConst (CIntConst (cInteger s) node))

-- translate alignof(expr)
transExpr (CAlignofExpr e node) = do
  md <- machine <$> getUserState
  t <- tExpr [] RValue e
  s <- alignofType md node t
  transExpr (CConst (CIntConst (cInteger s) node))

-- unsupported
transExpr e = unsupported e

-- | calculate struct's field index
calcStructFieldIndex :: NodeInfo -> Maybe SUERef -> Ident -> EnvM (Int, SType)
calcStructFieldIndex pos ref field = do
  let fn = identName field
  when (isn't _Just ref) $ errMsg pos "unknown struct "
  tdef <- M.lookup (fromJust ref) . compTypeDefs <$> getUserState
  when (isn't _Just tdef) $ errMsg pos $ "cannot find struct " ++ show ref
  let (CompType _ StructTag members attrs node) = fromJust tdef
  res <- (^._1) <$> foldM (\(s, i) m -> case m of
                   MemberDecl decl e node -> do
                     (n, t) <- varDecl pos decl
                     if n == fn then return ([(i, t)], i+1)
                     else return (s, i+1)
                   m -> unsupported m) ([], 0::Int) members
  when (null res) $ errMsg pos $ "cannot find field " ++ show field ++ " of struct " ++ show ref
  return $ head res

data DimensionOrSymbolOrConst =
  ADimension | ASymbol | AConst
  deriving (Eq)

inferDsc :: DimensionOrSymbolOrConst -> DimensionOrSymbolOrConst -> DimensionOrSymbolOrConst
inferDsc lDsc rDsc
  | lDsc == ADimension || rDsc == ADimension = ADimension
  | lDsc == ASymbol || rDsc == ASymbol = ASymbol
  | otherwise = AConst

-- | Translate to affine Expr
exprToAffineExpr :: M.Map String (Int, BU.ByteString) -> M.Map String (Int, BU.ByteString) -> M.Map String CExpr -> CExpr
                   -> Maybe (Affine.Expr, DimensionOrSymbolOrConst)
exprToAffineExpr ds syms exprs (CVar ident node) =
  let name = identName ident
      d = M.lookup name ds
   in case d of
        Just (d, _) -> Just (Affine.Dimension d, ADimension)
        Nothing ->
          case M.lookup name exprs of
            Just s -> case exprToAffineExpr ds syms exprs s of
                       s@(Just _) -> s
                       Nothing -> fmap (\v -> (Affine.Symbol $ v ^._1, ASymbol)) (M.lookup name syms)
            Nothing -> fmap (\v -> (Affine.Symbol $ v ^._1, ASymbol)) (M.lookup name syms)
exprToAffineExpr ds syms exprs c@(CConst (CIntConst _ _)) =
  fmap (\c -> (Affine.Constant $ fromIntegral c, AConst)) (intValue c)
exprToAffineExpr ds syms exprs (CBinary CAddOp lhs rhs node) = do
  (l, lDsc) <- exprToAffineExpr ds syms exprs lhs
  (r, rDsc) <- exprToAffineExpr ds syms exprs rhs
  return (Affine.Add l r, inferDsc lDsc rDsc)
exprToAffineExpr ds syms exprs (CBinary CMulOp lhs rhs node) = do
  (l, lDsc) <- exprToAffineExpr ds syms exprs lhs
  (r, rDsc) <- exprToAffineExpr ds syms exprs rhs
  if lDsc == ADimension && rDsc == ADimension then Nothing
  else return (Affine.Mul l r, inferDsc lDsc rDsc)
exprToAffineExpr ds syms exprs (CBinary op lhs rhs node)
  | op == CDivOp ||
    op == CRmdOp = do
  (l, lDsc) <- exprToAffineExpr ds syms exprs lhs
  (r, rDsc) <- exprToAffineExpr ds syms exprs rhs
  if rDsc == ADimension then Nothing
  else return ((case op of
              CRmdOp -> Affine.Mod
              CDivOp -> Affine.FloorDiv
              _ -> Affine.Mod) l r, inferDsc lDsc rDsc)
exprToAffineExpr _ _ _ _ = Nothing

-- | Translate a constant expression
transConst :: CConstant NodeInfo -> EnvM ([BindingOrName], SType)
transConst (CIntConst i node) = transInt i (getPos node)
transConst (CCharConst c node) = transChar node c (getPos node)
transConst (CFloatConst f node) = transFloat f (getPos node)
transConst (CStrConst s node) = transStr s (getPos node)

-- | Translate an integer literal
transInt :: CInteger -> AST.Location -> EnvM ([BindingOrName], SType)
transInt (CInteger i _ flag) loc = do
  id <- freshName
  md <- machine <$> getUserState
  let bits | testFlag FlagUnsigned flag = 8 * fromIntegral (iSize md TyUInt)
           | testFlag FlagLong flag = 8 * fromIntegral (iSize md TyLong)
           | testFlag FlagLongLong flag = 8 * fromIntegral (iSize md TyLLong)
           | testFlag FlagImag flag = 32
           | otherwise = 32
      sign | testFlag FlagUnsigned flag = False
           | testFlag FlagLong flag = True
           | testFlag FlagLongLong flag = True
           | testFlag FlagImag flag = False
           | otherwise = True
      ty = AST.IntegerType AST.Signless bits
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral i))], (ty, sign, Nothing))

-- | Translate a char literal
transChar :: NodeInfo -> CChar -> AST.Location -> EnvM ([BindingOrName], SType)
transChar pos (CChar c _) loc = do
  id <- freshName
  let ty = AST.IntegerType AST.Signless 8
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.IntegerAttr ty (fromIntegral $ ord c))], (ty, True, Nothing))
transChar pos c loc = errMsg pos "unsupported chars"

-- | Translate float literal
transFloat :: CFloat -> AST.Location -> EnvM ([BindingOrName], SType)
transFloat (CFloat str) loc = do
  id <- freshName
  let lastC = last str
      ty = case lastC of
            c | c == 'l' || c == 'L' -> AST.Float64Type
            _ -> AST.Float32Type
      str' = if lastC == 'l' || lastC == 'L' || lastC == 'f' || lastC == 'F' then init str else str
  return ([Left $ id AST.:= Arith.Constant loc ty (AST.FloatAttr ty $ read str')], (ty, True, Nothing))

-- | Translate a string literal
transStr :: CString -> AST.Location -> EnvM ([BindingOrName], SType)
transStr s@(CString str _) loc = do
  id <- freshName
  id0 <- freshName
  id1 <- freshName
  id2 <- freshName
  let ty = AST.VectorType [L.length str] (AST.IntegerType AST.Signless 8)
      cs = AST.DenseElementsAttr ty $
             AST.DenseUInt8 $ listArray (0 :: Int, L.length str-1) $ fromIntegral . ord <$> str
      pTy = AST.MemRefType [Nothing] (AST.IntegerType AST.Signless 8) Nothing Nothing
      c = id0 AST.:= Arith.Constant loc ty cs
      size = id1 AST.:= constInt loc AST.IndexType (L.length str)
      c0 = id2 AST.:= constIndex0 loc
      m = id AST.:= MemRef.alloca loc pTy [id1] []
      st = AST.Do $ Vector.vstore loc id0 id [id2]
  return ([Left c, Left size, Left c0, Left m, Left st, Right id], (pTy, True, Nothing))

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
handlers (TagEvent (CompDef compT@(CompType ref _ _ _ _))) = do
  modifyUserState (\s -> s{compTypeDefs=M.insert ref compT (compTypeDefs s)})
handlers (TagEvent (EnumDef (EnumType _ es _ node))) = modifyUserState (\s -> s{enumerators=enumerators s ++ es})
handlers (DeclEvent (Declaration decl)) = modifyUserState (\s -> s{decls=decls s ++ [decl]})
handlers (DeclEvent (FunctionDef funDef)) = do
  modifyUserState (\s -> s{funDefs=funDefs s ++ [funDef]})
handlers (TypeDefEvent typeDef@(TypeDef ident ty attrs node)) = do
  let name = identName ident
  modifyUserState (\s -> s{typeDefs=M.insert name typeDef $ typeDefs s})
handlers (LocalEvent (ObjectDef objDef)) = modifyUserState (\s -> s{objDefs=M.insert (posOf objDef) objDef (objDefs s)})
handlers (AsmEvent (CStrLit c n)) = return ()
handlers _ = return ()

identName :: Ident -> String
identName (Ident ident _ _) = ident

varName :: VarName -> String
varName (VarName ident _) = identName ident
varName NoName = ""

type_ :: NodeInfo -> Int -> Type -> EnvM SType
type_ pos ms (FunctionType ty attrs) = f ty
  where f (FunType resType argTypes _) = do
          ps <- mapM (fmap (^. _2 . _1) . paramDecl pos) argTypes
          rs <- mapM (fmap (^. _1) . type_  pos ms) [resType]
          rt <- type_ pos ms resType
          return (AST.FunctionType ps [t | t <- rs, t /= AST.NoneType], rt ^. _2, rt ^._3)
        f (FunTypeIncomplete ty) = type_ pos ms ty
type_ pos ms ty@(DirectType name quals attrs) = do
  md <- machine <$> getUserState
  case name of
    TyVoid                       -> return (AST.NoneType, False, Nothing)
    TyIntegral (id -> TyBool)    -> return (AST.IntegerType AST.Signless 1, False, Nothing)
    TyIntegral (id -> TyChar)    -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyChar)), True, Nothing)
    TyIntegral (id -> TySChar)   -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TySChar)), True, Nothing)
    TyIntegral (id -> TyUChar)   -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyUChar)), False, Nothing)
    TyIntegral (id -> TyShort)   -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyShort)), True, Nothing)
    TyIntegral (id -> TyUShort)  -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyUShort)), False, Nothing)
    TyIntegral (id -> TyInt)     -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyInt)), True, Nothing)
    TyIntegral (id -> TyUInt)    -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyUInt)), False, Nothing)
    TyIntegral (id -> TyInt128)  -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyInt128)), True, Nothing)
    TyIntegral (id -> TyUInt128) -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyUInt128)), False, Nothing)
    TyIntegral (id -> TyLong)    -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyLong)), True, Nothing)
    TyIntegral (id -> TyULong)   -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyULong)), False, Nothing)
    TyIntegral (id -> TyLLong)   -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyLLong)), True, Nothing)
    TyIntegral (id -> TyULLong)  -> return (AST.IntegerType AST.Signless (8 * fromIntegral (iSize md TyULLong)), False, Nothing)
    TyFloating (id -> TyFloat)   -> return (AST.Float32Type, True, Nothing)
    TyFloating (id -> TyDouble)  -> return (AST.Float64Type, True, Nothing)
    TyFloating (id -> TyLDouble) -> return (AST.Float64Type, True, Nothing)
    TyFloating (id -> TyFloatN n _) -> errMsg pos $ "unsupported: " ++ show ty
    TyComplex t -> do (ct, sign, tn) <- type_ pos ms (DirectType (TyFloating t) quals attrs)
                      return (AST.ComplexType ct, sign, tn)
    TyComp (CompTypeRef ref _ _) -> do
      tdef <- M.lookup ref . compTypeDefs <$> getUserState
      case tdef of
        Just tdef -> structType ms tdef
        Nothing -> errMsg pos $ "cannot find comp type " ++ show ref
    TyEnum ref -> return (AST.IntegerType AST.Signless 32, True, Nothing)
    TyBuiltin _ -> errMsg pos $ "unsupported: " ++ show ty
    _ -> errMsg pos $ "unsupported: " ++ show ty
type_ pos ms ty@(PtrType t quals attrs) = do
  (tt, sign, tn) <- type_ pos ms t
  return (if tt == AST.NoneType then AST.IntegerType AST.Signless 8
          else AST.MemRefType [Nothing] tt Nothing (Just $ AST.IntegerAttr (AST.IntegerType AST.Signless 64) ms), sign, tn)
type_ pos ms (ArrayType t size quals attrs) = do
  s <- arraySize pos size
  let msAttr = AST.IntegerAttr (AST.IntegerType AST.Signless 64) ms
  mt <- type_ pos ms t
  case mt of
    (AST.MemRefType sizes t attrs ms, sign, tn) | all (isn't _Nothing) sizes ->
      return (AST.MemRefType (s:sizes) t attrs ms, sign, tn)
    (t, sign, tn) -> return (AST.MemRefType [s] t Nothing (Just msAttr), sign, tn)
type_ pos ms ty@(TypeDefType (TypeDefRef ident t _) quals attrs) = do
  tdefs <- typeDefs <$> getUserState
  let name = identName ident
      tdef = M.lookup name tdefs
  case tdef of
    Just (TypeDef _ _ attrs _) -> do
      let vsAttrs = getExtVectorAttrs attrs
      (tt, sign, tn) <- type_ pos ms t
      if null vsAttrs then return (tt, sign, tn)
      else return (AST.VectorType vsAttrs tt, sign, tn)
    Nothing -> errMsg pos $ "unsupported: " ++ show ty

structType :: Int -> CompType -> EnvM SType
structType ms t@(CompType ref StructTag members attrs node) = do
  mTypes <- mapM (\case
                    m@(MemberDecl decl e node) -> do
                      d <- varDecl node decl
                      case d of
                        (_, (AST.MemRefType{}, _, _)) -> unsupported m
                        _ -> return d
                    t -> unsupported t) members
  when (L.length (L.foldl' (\s t -> if null s then [t] else if head s ^._2._1 /= t ^._2._1 then t:s else s) [] mTypes) /= 1) $
    errMsg node "currently struct only supports all fields with same type"
  return (AST.MemRefType [Just $ L.length mTypes] (head mTypes ^._2._1) Nothing Nothing, False, Just ref)
structType _ t = unsupported t

getExtVectorAttrs :: Attributes -> [Int]
getExtVectorAttrs attrs = [fromIntegral $ fromJust $ intValue e| (Attr ident [e] node) <- attrs,
                          identName ident == "__ext_vector_type__" && isn't _Nothing (intValue e)]

arraySize :: NodeInfo -> ArraySize -> EnvM (Maybe Int)
arraySize pos (UnknownArraySize static) =
  errMsg pos "unsupported dynamic array size"
arraySize pos (ArraySize static expr) =
  case intValue expr of
    Just e -> return $ Just $ fromIntegral e
    Nothing -> errMsg pos "unsupported dynamic array size"

paramDecl :: NodeInfo -> ParamDecl -> EnvM (String, SType)
paramDecl pos (ParamDecl var _) = varDecl pos var
paramDecl pos (AbstractParamDecl var _) = varDecl pos var

varDecl :: NodeInfo -> VarDecl -> EnvM (String, SType)
varDecl pos v@(VarDecl name attrs ty) = (varName name,) <$> type_ pos (memorySpace v) ty

params :: NodeInfo -> VarDecl -> EnvM [(String, SType)]
params pos (VarDecl name attr ty) = f ty
  where f (FunctionType ty attrs) = ps ty
        f _ = errMsg pos $ "unsupported: " ++ show ty
        ps (FunType resType argTypes _) = mapM (paramDecl pos) argTypes
        ps (FunTypeIncomplete ty) = errMsg pos $ "unsupported: " ++ show ty

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