module Lam.Data where

open import Data.Bool using (true; false)

open import Haskell.Prelude using (Int; Bool; String)

Id : Set
Id = String

{-# COMPILE AGDA2HS Id #-}

data Nat : Set where
  Z : Nat
  S : Nat → Nat

{-# COMPILE AGDA2HS Nat deriving (Eq, Show) #-}

data RawType : Set where
  RawBoolT : RawType
  RawIntT  : RawType
  RawU     : RawType
  RawArrow : RawType → RawType → RawType
  FreeType : Id → RawType

{-# COMPILE AGDA2HS RawType deriving Show #-}

data Type : Set where
  BoolT : Type
  IntT  : Type
-- U is an opaque type
  U     : Type
  Arrow : Type → Type → Type

{-# COMPILE AGDA2HS Type #-}

data Prim : Set where
  PlusPrim  : Prim
  MinusPrim : Prim
  MultPrim  : Prim

{-# COMPILE AGDA2HS Prim deriving Show #-}

eqPrim : Prim → Prim → Bool
eqPrim PlusPrim  PlusPrim  = true
eqPrim MinusPrim MinusPrim = true
eqPrim MultPrim  MultPrim  = true
eqPrim _ _                 = false

{-# COMPILE AGDA2HS eqPrim #-}

data RawExpr : Set where
  RawVar     : Id → RawExpr
  RawLam     : Id → RawType → RawExpr → RawExpr
  RawApp     : RawExpr → RawExpr → RawExpr
  RawNumVal  : Int → RawExpr
  RawBoolVal : Bool → RawExpr
  RawPrimE   : Prim → RawExpr

{-# COMPILE AGDA2HS RawExpr deriving Show #-}

data Expr : Set where
  Var     : Nat → Expr
  Lam     : Id → Type → Expr → Expr
  App     : Expr → Expr → Expr
  NumVal  : Int → Expr
  BoolVal : Bool → Expr
  PrimE   : Prim → Expr

{-# COMPILE AGDA2HS Expr #-}
