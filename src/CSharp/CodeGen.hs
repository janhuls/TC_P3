module CSharp.CodeGen where

import CSharp.AbstractSyntax 
import CSharp.Algebra 

import SSM

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                          -- Class
type M = Code                          -- Member
type S = (Env, Code)                   -- Statement
type E = Env -> ValueOrAddress -> Code -- Expression
type Env = M.Map Ident Int

extendEnv :: Ident -> Env -> Env
extendEnv x env = M.insert x (M.size env) env --reserve next free stack slot

lookupVar :: Ident -> Env -> Int
lookupVar x env =
  case M.lookup x env of
    Just i  -> i
    Nothing -> error ("use of undeclared variable: " ++ x) --shouldn't happen if analysis is correct

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp

fClass :: ClassName -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M
fMembDecl d = []

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps (_, s) = [LABEL x] ++ s ++ [RET]

fStatDecl :: Decl -> S
fStatDecl (Decl _ ident) = (M.singleton ident 0, [LDC 0])

fStatExpr :: E -> S
fStatExpr e = (M.empty, e M.empty Value ++ [pop])

fStatIf :: E -> S -> S -> S
fStatIf e (env1, s1) (env2, s2) = (M.empty, c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2) where
  c        = e M.empty Value
  (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: E -> S -> S
fStatWhile e (env1, s1) = (M.empty, [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]) where
  c = e M.empty Value
  (n, k) = (codeSize s1, codeSize c)

fStatReturn :: E -> S
fStatReturn e = (M.empty, e M.empty Value ++ [pop] ++ [RET])

fStatBlock :: [S] -> S
fStatBlock ss =
  ( finalEnv
  , concat codes
  )
  where
    (finalEnv, codes, _) = foldl step (M.empty, [], 0) ss

    step (env, codeAcc, offset) (env', code') =
      let
        shiftedEnv = M.map (+ offset) env'
        newEnv     = M.union env shiftedEnv
      in
        ( newEnv
        , codeAcc ++ code'
        , offset + M.size env'
        )

fExprLit :: Literal -> E
fExprLit l va  = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x va = case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = 42

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp op    e1 e2 va = e1 Value ++ e2 Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0 
