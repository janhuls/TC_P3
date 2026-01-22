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
type C = Code -- class
type M = Code -- member
type S = Env -> (Env, Code)  -- statement
type E = Env -> ValueOrAddress -> Code -- expression

-- maps var names to stack pos
type Env = M.Map Ident Int

-- helper for env management
extendEnv :: Ident -> Env -> Env
extendEnv x env = M.insert x (M.size env) env

lookupVar :: Ident -> Env -> Int
lookupVar x env =
  case M.lookup x env of
    Just i  -> i
    Nothing -> error ("use of undeclared variable: " ++ x)

-- main code gen alg
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
fMembDecl (Decl _ x) = []  -- member vars apart handled in real implem

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth retType funcName params body =
  let -- build init env from params
      initEnv = M.fromList [(name, i) | (Decl _ name, i) <- zip params [0..]]
      (finalEnv, bodyCode) = body initEnv
      -- calc number local decl
      numLocals = M.size finalEnv - length params
      -- reserve local sapce
      linkCode = if numLocals > 0 then [LINK numLocals] else []
      -- bye link -> return
      unlinkCode = if numLocals > 0 then [UNLINK] else []
      retCode = case retType of
        TyVoid -> [RET]
        _      -> []  -- non void should have ret with value
  in [LABEL funcName] ++ linkCode ++ bodyCode ++ unlinkCode ++ retCode

fStatDecl :: Decl -> S
fStatDecl (Decl _ ident) env =
  let env' = extendEnv ident env
  in (env', [LDC 0])  -- get space -> init to 0

fStatExpr :: E -> S
fStatExpr e env = (env, e env Value ++ [AJS (-1)])  -- pop res

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
  let -- process statements seq, paralell env
      processStats [] currentEnv codes = (currentEnv, codes)
      processStats (s:ss) currentEnv codes =
        let (newEnv, code) = s currentEnv
        in processStats ss newEnv (codes ++ code)
      
      (finalEnv, allCode) = processStats statements env0 []
      
      numLocals = M.size finalEnv - M.size env0
      
      cleanupCode = if numLocals > 0 then [AJS (-numLocals)] else []
      
  in (env0, allCode ++ cleanupCode)  -- ret og env

fExprLit :: Literal -> E
fExprLit l env va = [LDC n]
  where
    n = case l of
      LitInt n -> n
      LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x env va =
  let loc = lookupVar x env
  in case va of
    Value   -> [LDL loc]
    Address -> [LDLA loc]

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 env va =
  let c2 = e2 env Value     -- eval right
      c1 = e1 env Address   -- get address of left
      storeCode = [STA 0]   -- store val at address
      loadCode = case va of
        Value   -> [LDL 0]  -- if used as value load
        Address -> []       -- if used as address dont
  in c2 ++ c1 ++ storeCode ++ loadCode

-- lazy evaluation and
fExprOp OpAnd e1 e2 env va =
  let c1 = e1 env Value
      c2 = e2 env Value
      n2 = codeSize c2
  in c1 ++ [BRF (n2 + 2)] ++ c2 ++ [BRA 2, LDC 0]

-- lazy eval or
fExprOp OpOr e1 e2 env va =
  let c1 = e1 env Value
      c2 = e2 env Value
      n2 = codeSize c2
  in c1 ++ [BRT (n2 + 2)] ++ c2 ++ [BRA 2, LDC (-1)]

-- other eager mfs
fExprOp op e1 e2 env va =
  let c1 = e1 env Value
      c2 = e2 env Value
      opCode = case op of
        OpAdd -> ADD
        OpSub -> SUB
        OpMul -> MUL
        OpDiv -> DIV
        OpMod -> MOD
        OpXor -> XOR
        OpLeq -> LE
        OpLt  -> LT
        OpGeq -> GE
        OpGt  -> GT
        OpEq  -> EQ
        OpNeq -> NE
        _     -> error "unknown operator"
  in c1 ++ c2 ++ [opCode]

fExprCall :: Ident -> [E] -> E
fExprCall "print" args env _ =
  concatMap (\arg -> arg env Value ++ [TRAP 0]) args

fExprCall funcName args env Value =
  let -- eval all args and push on stack
      argCode = concatMap (\arg -> arg env Value) args
  in argCode ++ [Bsr funcName]

fExprCall _ _ _ Address =
  error "function call cannot be used as an address"

-- helper for distinguishinf
data ValueOrAddress = Value | Address
  deriving Show

-- bool2int c:
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0