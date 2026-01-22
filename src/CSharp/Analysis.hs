{-# LANGUAGE LambdaCase #-}
module CSharp.Analysis (analyzeClass) where

import CSharp.AbstractSyntax
import qualified Data.Map as M
import Control.Monad (when, foldM)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

-- Environment types
type VarEnv = M.Map Ident Type
type FuncEnv = M.Map Ident ([Type], RetType)
type MemberEnv = M.Map Ident Type

data GlobalEnv = GlobalEnv
  { gFuncEnv :: FuncEnv
  , gMemberEnv :: MemberEnv
  } deriving Show

data LocalEnv = LocalEnv
  { lVarEnv :: VarEnv
  , lGlobal :: GlobalEnv
  } deriving Show

-- analyze class and exit with code on error
analyzeClass :: Class -> IO ()
analyzeClass cls@(Class _ members) = do
  let globalEnv = collectGlobals cls
  mapM_ (checkMember globalEnv) members

-- get global info
collectGlobals :: Class -> GlobalEnv
collectGlobals (Class _ members) =
  GlobalEnv (collectFuncs members) (collectMembers members)
  where
    collectMembers [] = M.empty
    collectMembers (MemberD (Decl (NV t) name) : rest) = M.insert name t (collectMembers rest)
    collectMembers (_ : rest) = collectMembers rest
    
    collectFuncs [] = M.empty
    collectFuncs (MemberM retType name params _ : rest) =
      let paramTypes = [t | Decl (NV t) _ <- params]
      in M.insert name (paramTypes, retType) (collectFuncs rest)
    collectFuncs (_ : rest) = collectFuncs rest

-- membercheck
checkMember :: GlobalEnv -> Member -> IO ()
checkMember _ (MemberD _) = return ()
checkMember gEnv (MemberM retType name params body) = do
  let paramTypes = [(pname, t) | Decl (NV t) pname <- params]
      localEnv = LocalEnv (M.fromList paramTypes) gEnv
  _ <- checkStatement localEnv body   -- ignore returned localenv
  return ()                           -- now io matches


-- check statement (returns updated env)
checkStatement :: LocalEnv -> Stat -> IO LocalEnv

checkStatement env (StatDecl (Decl TyVoid _)) = do
  hPutStrLn stderr "Error: cannot declare void variable"
  exitWith (ExitFailure 25)

checkStatement env (StatDecl (Decl (NV t) name)) =
  case M.lookup name (lVarEnv env) of
    Just _ -> do
      hPutStrLn stderr $ "Error: variable " ++ name ++ " already declared"
      exitWith (ExitFailure 25)
    Nothing -> return $ env { lVarEnv = M.insert name t (lVarEnv env) }

checkStatement env (StatExpr expr) = do
  _ <- checkExpression env expr
  return env

checkStatement env (StatIf cond thenStat elseStat) = do
  condType <- checkExpression env cond
  when (condType /= TyBool) $ do
    hPutStrLn stderr "Error: if condition must be boolean"
    exitWith (ExitFailure 25)
  _ <- checkStatement env thenStat
  _ <- checkStatement env elseStat
  return env

checkStatement env (StatWhile cond body) = do
  condType <- checkExpression env cond
  when (condType /= TyBool) $ do
    hPutStrLn stderr "Error: while condition must be boolean"
    exitWith (ExitFailure 25)
  _ <- checkStatement env body
  return env

checkStatement env (StatReturn expr) = do
  _ <- checkExpression env expr
  return env

checkStatement env (StatBlock stats) = do
  _ <- foldM checkStatement env stats
  return env  -- dont keep decl from block

checkStatement env (StatFor (inits, cond, updates) body) = do
  -- check inits
  env' <- foldM checkExprDecl env inits
  -- check conds
  condType <- checkExpression env' cond
  when (condType /= TyBool) $ do
    hPutStrLn stderr "Error: for condition must be boolean"
    exitWith (ExitFailure 25)
  -- -. updates
  mapM_ (checkExprDecl env') updates
  -- -. body
  _ <- checkStatement env' body
  return env

checkExprDecl :: LocalEnv -> ExprDecl -> IO LocalEnv
checkExprDecl env (ForDecl decl) = checkStatement env (StatDecl decl)
checkExprDecl env (ForExpr expr) = do
  _ <- checkExpression env expr
  return env

-- -. expr and return type
checkExpression :: LocalEnv -> Expr -> IO Type

checkExpression _ (ExprLit (LitInt _)) = return TyInt
checkExpression _ (ExprLit (LitBool _)) = return TyBool

checkExpression env (ExprVar name) =
  case M.lookup name (lVarEnv env) of
    Just t -> return t
    Nothing -> case M.lookup name (gMemberEnv (lGlobal env)) of
      Just t -> return t
      Nothing -> do
        hPutStrLn stderr $ "Error: undefined variable: " ++ name
        exitWith (ExitFailure 21)

checkExpression env (ExprOper OpAsg left right) = do
  leftType <- checkExpression env left
  rightType <- checkExpression env right
  when (leftType /= rightType) $ do
    hPutStrLn stderr "Error: type mismatch in assignment"
    exitWith (ExitFailure 22)
  return leftType

checkExpression env (ExprOper op left right) = do
  leftType <- checkExpression env left
  rightType <- checkExpression env right
  case op of
    OpAdd -> checkBinOp "+" TyInt TyInt TyInt leftType rightType
    OpSub -> checkBinOp "-" TyInt TyInt TyInt leftType rightType
    OpMul -> checkBinOp "*" TyInt TyInt TyInt leftType rightType
    OpDiv -> checkBinOp "/" TyInt TyInt TyInt leftType rightType
    OpMod -> checkBinOp "%" TyInt TyInt TyInt leftType rightType
    OpAnd -> checkBinOp "&&" TyBool TyBool TyBool leftType rightType
    OpOr  -> checkBinOp "||" TyBool TyBool TyBool leftType rightType
    OpXor -> checkBinOp "^" TyBool TyBool TyBool leftType rightType
    OpLt  -> checkBinOp "<" TyInt TyInt TyBool leftType rightType
    OpLeq -> checkBinOp "<=" TyInt TyInt TyBool leftType rightType
    OpGt  -> checkBinOp ">" TyInt TyInt TyBool leftType rightType
    OpGeq -> checkBinOp ">=" TyInt TyInt TyBool leftType rightType
    OpEq  -> checkEqOp "==" leftType rightType
    OpNeq -> checkEqOp "!=" leftType rightType
    _ -> do
      hPutStrLn stderr "Error: unknown operator"
      exitWith (ExitFailure 25)

checkExpression env (ExprCall "print" args) = do
  _ <- mapM (checkExpression env) args
  return TyInt  -- print doesnt ret value but need type

checkExpression env (ExprCall funcName args) = do
  case M.lookup funcName (gFuncEnv (lGlobal env)) of
    Nothing -> do
      hPutStrLn stderr $ "Error: undefined function: " ++ funcName
      exitWith (ExitFailure 21)
    Just (paramTypes, retType) -> do
      argTypes <- mapM (checkExpression env) args
      when (length argTypes /= length paramTypes) $ do
        hPutStrLn stderr $ "Error: function " ++ funcName ++ " expects " ++ 
                           show (length paramTypes) ++ " arguments but got " ++ 
                           show (length argTypes)
        exitWith (ExitFailure 23)
      when (argTypes /= paramTypes) $ do
        hPutStrLn stderr $ "Error: function " ++ funcName ++ " called with wrong argument types"
        exitWith (ExitFailure 24)
      case retType of
        NV t -> return t
        TyVoid -> return TyInt  -- shouldnt use void func as expr

checkBinOp :: String -> Type -> Type -> Type -> Type -> Type -> IO Type
checkBinOp opName expLeft expRight result actLeft actRight =
  if actLeft == expLeft && actRight == expRight
    then return result
    else do
      hPutStrLn stderr $ "Error: operator " ++ opName ++ " type mismatch"
      exitWith (ExitFailure 25)

checkEqOp :: String -> Type -> Type -> IO Type
checkEqOp opName leftType rightType =
  if leftType == rightType
    then return TyBool
    else do
      hPutStrLn stderr $ "Error: operator " ++ opName ++ " requires matching types"
      exitWith (ExitFailure 25)

-- dummy alg for compatibility
analysisAlgebra = undefined