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

type C = Code
type M = Code
type S = Env -> (Env, Code)
type E = Env -> ValueOrAddress -> Code
type Env = M.Map Ident Int

extendEnv :: Ident -> Env -> Env
extendEnv x env = M.insert x (M.size env) env

lookupVar :: Ident -> Env -> Int
lookupVar x env =
  case M.lookup x env of
    Just i  -> i
    Nothing -> error ("use of undeclared variable: " ++ x)

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
fClass _ ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M
fMembDecl _ = []

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth retType funcName params body =
  let initEnv = M.fromList [(name, i) | (Decl _ name, i) <- zip params [0..]]
      (finalEnv, bodyCode) = body initEnv
      numLocals = M.size finalEnv - length params
      linkCode = if numLocals > 0 then [LINK numLocals] else []
      unlinkCode = if numLocals > 0 then [UNLINK] else []
      retCode = case retType of
        TyVoid -> [RET]
        _ -> []
  in [LABEL funcName] ++ linkCode ++ bodyCode ++ unlinkCode ++ retCode

fStatDecl :: Decl -> S
fStatDecl (Decl _ ident) env =
  let env' = extendEnv ident env
  in (env', [LDC 0])

fStatExpr :: E -> S
fStatExpr e env = (env, e env Value ++ [AJS (-1)])

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env =
  let c = e env Value
      (_, s1') = s1 env
      (_, s2') = s2 env
      (n1, n2) = (codeSize s1', codeSize s2')
  in (env, c ++ [BRF (n1 + 2)] ++ s1' ++ [BRA n2] ++ s2')

fStatWhile :: E -> S -> S
fStatWhile e s1 env =
  let (_, s1') = s1 env
      c = e env Value
      (n, k) = (codeSize s1', codeSize c)
  in (env, [BRA n] ++ s1' ++ c ++ [BRT (-(n + k + 2))])

fStatReturn :: E -> S
fStatReturn e env = (env, e env Value ++ [RET])

fStatBlock :: [S] -> S
fStatBlock statements env0 =
  let -- thread environment through statements,, accumulating code
      step (currentEnv, accCode) stmt =
        let (newEnv, stmtCode) = stmt currentEnv
        in (newEnv, accCode ++ stmtCode)
      (finalEnv, allCode) = foldl step (env0, []) statements
      -- count new locals declared here
      numNewLocals = M.size finalEnv - M.size env0
      -- clean up locals at end of here
      cleanupCode = if numNewLocals > 0 then [AJS (-numNewLocals)] else []
  in (env0, allCode ++ cleanupCode)  -- ret og env in scope here

fExprLit :: Literal -> E
fExprLit l _ _ = [LDC n]
  where
    n = case l of
      LitInt n -> n
      LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x env va =
  let loc = lookupVar x env
  in case va of
    Value -> [LDL loc]
    Address -> [LDLA loc]

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 env va =
  let c2 = e2 env Value
      c1 = e1 env Address
      storeCode = [STA 0]
      loadCode = case va of
        Value -> [LDL 0]
        Address -> []
  in c2 ++ c1 ++ storeCode ++ loadCode

fExprOp OpAnd e1 e2 env _ =
  let c1 = e1 env Value
      c2 = e2 env Value
      n2 = codeSize c2
  in c1 ++ [BRF (n2 + 2)] ++ c2 ++ [BRA 2, LDC 0]

fExprOp OpOr e1 e2 env _ =
  let c1 = e1 env Value
      c2 = e2 env Value
      n2 = codeSize c2
  in c1 ++ [BRT (n2 + 2)] ++ c2 ++ [BRA 2, LDC (-1)]

fExprOp op e1 e2 env _ =
  let c1 = e1 env Value
      c2 = e2 env Value
      opCode = case op of
        OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV
        OpMod -> MOD; OpXor -> XOR
        OpLeq -> LE; OpLt -> LT; OpGeq -> GE; OpGt -> GT
        OpEq -> EQ; OpNeq -> NE
        _ -> error "unknown operator"
  in c1 ++ c2 ++ [opCode]

fExprCall :: Ident -> [E] -> E
fExprCall "print" args env _ =
  concatMap (\arg -> arg env Value ++ [TRAP 0]) args

fExprCall funcName args env Value =
  concatMap (\arg -> arg env Value) args ++ [Bsr funcName]

fExprCall _ _ _ Address =
  error "function call cannot be used as an address"

data ValueOrAddress = Value | Address deriving Show

bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0