{-# LANGUAGE RecordWildCards #-}
module Lam.Infer where

import Lam.Data
import Lam.Nat (lookupMaybe)

type TypeSubst = TypeVarId -> Maybe TypeL

appTypeSubst :: TypeSubst -> TypeL -> TypeL
appTypeSubst _ BoolT = BoolT
appTypeSubst _ IntT = IntT
appTypeSubst sig (Arrow t1 t2) = Arrow (appTypeSubst sig t1) (appTypeSubst sig t2)
appTypeSubst sig (Prod t1 t2) = Prod (appTypeSubst sig t1) (appTypeSubst sig t2)
appTypeSubst sig (Sum t1 t2) = Sum (appTypeSubst sig t1) (appTypeSubst sig t2)
appTypeSubst sig (TypeVar i) =
  case sig i of
    Just t' -> t'
    Nothing -> TypeVar i

data Constraint = Constraint { lhs :: TypeL, rhs :: TypeL }

mkConstr :: TypeL -> TypeL -> Constraint
mkConstr l r = Constraint { lhs = l, rhs = r }

instance Show Constraint where
  show (Constraint {..}) = show lhs ++ " = " ++ show rhs

type TypingContext = [TypeL]

genTypeConstraints :: TypingContext -> TypeVarId -> Expr -> (TypeL, TypeVarId, [Constraint])
genTypeConstraints gam x (Var i) =
  case lookupMaybe i gam of
    Nothing -> (BoolT, x, [ mkConstr BoolT IntT ])
    Just t -> (t, x, [])
genTypeConstraints gam x (Lam _ t e) =
  let (t', x', c') = genTypeConstraints (t : gam) x e in
  (Arrow t t', x', c')
genTypeConstraints _ x (Const (NumC _)) = (IntT, x, [])
genTypeConstraints _ x (Const (BoolC _)) = (BoolT, x, [])
genTypeConstraints gam x (App e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let mv = TypeVar x2 in
  let c = c1 ++ c2 ++ [mkConstr t1 (Arrow t2 mv)] in
  (mv, x2 + 1, c)
genTypeConstraints gam x (Inl e) =
  let (t, x', c) = genTypeConstraints gam x e in
  let mv = TypeVar x' in
  (Sum t mv, x' + 1, c)
genTypeConstraints gam x (Inr e) =
  let (t, x', c) = genTypeConstraints gam x e in
  let mv = TypeVar x' in
  (Sum mv t, x' + 1, c)
genTypeConstraints gam x (Ite b t e) =
  let (t1, x1, c1) = genTypeConstraints gam x b in
  let (t2, x2, c2) = genTypeConstraints gam x1 t in
  let (t3, x3, c3) = genTypeConstraints gam x2 e in
  let c = c1 ++ c2 ++ c3 ++ [ mkConstr t1 BoolT, mkConstr t2 t3 ] in
  (t2, x3 + 1, c)
genTypeConstraints gam x (UnaryOp Not e) =
  let (t, x', c) = genTypeConstraints gam x e in
  (BoolT, x' + 1, c ++ [ mkConstr t BoolT ])
genTypeConstraints gam x (UnaryOp Proj1 e) =
  let (t, x', c) = genTypeConstraints gam x e in
  let mv1 = TypeVar x' in
  let mv2 = TypeVar (x' + 1) in
  let c' = c ++ [ mkConstr t (Prod mv1 mv2) ] in
  (mv1, x' + 2, c')
genTypeConstraints gam x (UnaryOp Proj2 e) =
  let (t, x', c) = genTypeConstraints gam x e in
  let mv1 = TypeVar x' in
  let mv2 = TypeVar (x' + 1) in
  let c' = c ++ [ mkConstr t (Prod mv1 mv2) ] in
  (mv2, x' + 2, c')
genTypeConstraints gam x (BinOp Add e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 IntT, mkConstr t2 IntT ] in
  (IntT, x2 + 1, c)
genTypeConstraints gam x (BinOp Sub e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 IntT, mkConstr t2 IntT ] in
  (IntT, x2 + 1, c)
genTypeConstraints gam x (BinOp Mul e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 IntT, mkConstr t2 IntT ] in
  (IntT, x2 + 1, c)
genTypeConstraints gam x (BinOp LtInt e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 IntT, mkConstr t2 IntT ] in
  (BoolT, x2 + 1, c)
genTypeConstraints gam x (BinOp And e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 BoolT, mkConstr t2 BoolT ] in
  (BoolT, x2 + 1, c)
genTypeConstraints gam x (BinOp Or e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  let c = c1 ++ c2 ++ [ mkConstr t1 BoolT, mkConstr t2 BoolT ] in
  (BoolT, x2 + 1, c)
genTypeConstraints gam x (BinOp MkPair e1 e2) =
  let (t1, x1, c1) = genTypeConstraints gam x e1 in
  let (t2, x2, c2) = genTypeConstraints gam x1 e2 in
  (Prod t1 t2, x2 + 1, c1 ++ c2)
genTypeConstraints gam x (Case e _ el _ er) =
  let (t1, x1, c1) = genTypeConstraints gam x e in
  let mv1 = TypeVar x1 in
  let mv2 = TypeVar (x1 + 1) in
  let (t2, x2, c2) = genTypeConstraints (mv1 : gam) (x1 + 2) el in
  let (t3, x3, c3) = genTypeConstraints (mv2 : gam) x2 er in
  let c = c1 ++ c2 ++ c3 ++ [ mkConstr t1 (Sum mv1 mv2), mkConstr t2 t3 ] in
  (t2, x3, c)
genTypeConstraints gam x (Fix e) =
  let (t1, x1, c1) = genTypeConstraints gam x e in
  let mv = TypeVar x1 in
  let c = c1 ++ [ mkConstr t1 (Arrow mv mv) ] in
  (t1, x1 + 1, c)


