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
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Node ( NodeInfo )
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
                labels :: M.Map String AST.BlockName}

initEnv = Env{decls = [],
              objDefs = [],
              funDefs = [],
              enumerators = [],
              typeDefs = [],
              labels = M.empty}

underScope action = do
  env <- lift getUserState
  result <- action
  lift $ modifyUserState (const env)
  return result

addLabel name label = 
  lift $ modifyUserState (\s -> s{labels=M.insert name label (labels s)})

lookupLabel name = do
  l <- lift $ M.lookup name <$> getUserState
  case l of
    Just l -> return l
    Nothing -> error $ "cannot find label " ++ name

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
                              lift (funDefs <$> getUserState) >>= mapM_ transFunction
                   case res of
                     Left errs -> error $ show errs
                     Right (res, _) -> res
     --MLIR.dump nativeOp
     check <- MLIR.verifyOperation nativeOp
     unless check $ exitWith (ExitFailure 1)
     BU.toString <$> MLIR.showOperationWithLocation nativeOp)

transFunction (FunDef var stmt _) = underScope $ do
  let (name, ty) = varDecl var
  case ty of
    AST.FunctionType argTypes resultTypes -> do
      AST.buildFunction (BU.fromString name) resultTypes AST.NoAttrs $ do
        transBlock stmt
        AST.endOfRegion
    ty -> error $ "expected a function type, but got " ++ show (pretty var)

transBlock :: AST.MonadNameSupply m => CStatement NodeInfo -> AST.RegionBuilderT m ()
transBlock (CCompound labels items _) = do
  let lnames = map identName labels
  label <- AST.buildBlock $ do
    mapM_ transBlockItem $ init items
    transBlockTerminator $ last items
  -- addLabel (head lnames) label
  return ()
transBlock s = unsupported s

transBlockItem :: AST.MonadBlockBuilder m => CCompoundBlockItem NodeInfo -> m ()
transBlockItem (CBlockStmt s) = transNoTerminator s
transBlockItem s = unsupported s

transNoTerminator :: AST.MonadBlockBuilder m => CStatement NodeInfo -> m ()
transNoTerminator s = unsupported s

transBlockTerminator :: AST.MonadBlockBuilder m => CCompoundBlockItem NodeInfo -> m AST.EndOfBlock
transBlockTerminator (CBlockStmt s) = transTerminator s
transBlockTerminator s = unsupported s

transTerminator :: AST.MonadBlockBuilder m => CStatement NodeInfo -> m AST.EndOfBlock
transTerminator (CReturn Nothing _) = Std.return []
transTerminator (CReturn (Just e) _) = do
  result <- transExpr e
  Std.return [result]
transTerminator e = unsupported e

transExpr :: AST.MonadBlockBuilder m => CExpression NodeInfo -> m AST.Value
transExpr (CConst c) = transConst c
transExpr e = unsupported e

transConst :: AST.MonadBlockBuilder m => CConstant NodeInfo -> m AST.Value
transConst (CIntConst i _) = transInt i
transConst (CCharConst c _) = transChar c
transConst (CFloatConst f _) = transFloat f
transConst (CStrConst s _) = transStr s

transInt :: AST.MonadBlockBuilder m => CInteger -> m AST.Value
transInt (CInteger i _ _) = do
  let ty = AST.IntegerType AST.Signless 32
  Arith.constant ty (AST.IntegerAttr ty (fromIntegral i))

transChar :: AST.MonadBlockBuilder m => CChar -> m AST.Value
transChar (CChar c _) = do
  let ty = AST.IntegerType AST.Signless 8
  Arith.constant ty (AST.IntegerAttr ty (fromIntegral $ ord c))
transChar c = error $ "unsupported chars"

transFloat :: AST.MonadBlockBuilder m => CFloat -> m AST.Value
transFloat (CFloat str) = do
  let ty = AST.Float32Type
  Arith.constant ty (AST.FloatAttr ty $ read str)

transStr :: AST.MonadBlockBuilder m => CString -> m AST.Value
transStr s@(CString str _) = error "xxx"

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
