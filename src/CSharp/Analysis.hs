module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra


analysisAlgebra :: CSharpAlgebra Bool () () ()
analysisAlgebra = undefinedAlgebra { clas = \_ _ -> True }

undefinedAlgebra :: CSharpAlgebra a b c d
undefinedAlgebra = CSharpAlgebra
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined

scopeCheckingAlgebra :: CSharpAlgebra Bool Bool ([Ident] -> ([Ident], Bool)) ([Ident] -> ([Ident], Bool))
scopeCheckingAlgebra = CSharpAlgebra
  scClas
  scMemberD
  scMemberM
  scStatDecl
  scStatExpr
  scStatIf
  scStatWhile
  scStatReturn
  scStatBlock
  scExprLit
  scExprVar
  scExprOper
  scExprCall

scClas _ = and


scMemberD _ = True

scMemberM :: RetType -> Ident -> [Decl] -> ([Ident] -> ([Ident], Bool)) -> Bool
scMemberM _ name _ env = True -- ??


scStatDecl :: Decl -> [Ident] -> ([Ident], Bool)
scStatDecl (Decl ty name) env = (name:env, True)

scStatExpr :: ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool))
scStatExpr e env = (env, snd $ e env)

scStatIf :: ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool))
scStatIf e s1 s2 env = (env, s1' && s2' && b) where
  (_, s1') = s1 env
  (_, s2') = s2 env
  (_, b)   = e env

scStatWhile :: ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool))
scStatWhile e s env = (env, s' && b) where
  (_, s') = s env
  (_, b)  = e env

scStatReturn :: ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool))
scStatReturn e env = (env, snd $ e env)

scStatBlock :: [[Ident] -> ([Ident], Bool)] -> ([Ident] -> ([Ident], Bool))
scStatBlock ss env = foldl step (env, True) ss where
  step :: ([Ident], Bool) -> ([Ident] -> ([Ident], Bool)) -> ([Ident], Bool)
  step (oldEnv, b) s =
    let (newEnv, newb) = s oldEnv
    in  (newEnv, b && newb)


scExprLit :: Literal -> ([Ident] -> ([Ident], Bool))
scExprLit _ env = (env, True)

scExprVar :: Ident -> ([Ident] -> ([Ident], Bool))
scExprVar ident env = (env, ident `elem` env)

scExprOper :: Operator -> ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool)) -> ([Ident] -> ([Ident], Bool))
scExprOper _ e1 e2 env = (env'', e1' && e2') where
  (env', e1') = e1 env
  (env'', e2') = e2 env'

scExprCall :: Ident -> [[Ident] -> ([Ident], Bool)] -> ([Ident] -> ([Ident], Bool))
scExprCall fIdent = scStatBlock  -- currently doesn't check for func ident in env