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
type S = Env -> (Env, Code)            -- Statement
type E = Env -> ValueOrAddress -> Code -- Expression
type Env = M.Map Ident Int

extendEnv :: Ident -> Env -> Env
extendEnv x env = M.insert x (M.size env) env --reserve next free stack slot

lookupVar :: Ident -> Env -> Int
lookupVar x env =
  case M.lookup x env of
    Just i  -> i
    Nothing -> error ("use of undeclared variable: " ++ x ++ " in environment" ++ show env) --shouldn't happen if analysis is correct

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
  fExprCall

fClass :: ClassName -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M
fMembDecl d = []

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps body = [LABEL x] ++ code ++ ([RET | t == TyVoid]) where
  initEnv = M.fromList [(x, i) | (Decl _ x, i) <- zip ps [0..]]
  (_, code) = body initEnv --run the body with parameter env

fStatDecl :: Decl -> S
fStatDecl (Decl _ ident) env =
  let env' = extendEnv ident env
  in (env', [LDC 0])

fStatExpr :: E -> S
fStatExpr e env = (env, e env Value ++ [pop])

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (env, c ++ [BRF (n1 + 2)] ++ s1' ++ [BRA n2] ++ s2') where
  (_, s1') = s1 env
  (_, s2') = s2 env
  c        = e env Value
  (n1, n2) = (codeSize s1', codeSize s2')

fStatWhile :: E -> S -> S
fStatWhile e s1 env = (env, [BRA n] ++ s1' ++ c ++ [BRT (-(n + k + 2))]) where
  (env1, s1') = s1 env
  c = e env Value
  (n, k) = (codeSize s1', codeSize c)

fStatReturn :: E -> S
fStatReturn e env = (env, e env Value ++ [RET])

fStatBlock :: [S] -> S
fStatBlock ss env0 = foldl step (env0, []) ss where
  step :: (Env, Code) -> (Env -> (Env, Code)) -> (Env, Code)
  step (env, codeAcc) stmt =
    let (env', code) = stmt env
    in (env', codeAcc ++ code)

fExprLit :: Literal -> E
fExprLit l env va  = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x env va = case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = lookupVar x env

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
fExprOp op    e1 e2 env va = e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

fExprCall :: Ident -> [E] -> E
fExprCall "print" exprs env _  = concatMap (\arg -> arg env Value ++ [TRAP 0]) exprs
fExprCall func exprs env Value = concatMap (\arg -> arg env Value) exprs ++ [Bsr func]
fExprCall _ _ _ Address        = error "function call cannot be an address"

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
