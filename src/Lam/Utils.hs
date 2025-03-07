{-# LANGUAGE OverloadedStrings #-}

module Lam.Utils where

import Lam.Data
import Lam.Nat

import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)

-- this must be here since we don't have access to Int in Agda
toNat :: Int -> Nat
toNat i =
  case compare i 0 of
    GT -> S (toNat (i - 1))
    EQ -> Z
    LT -> error "[toNat]: negative input"

pickFresh :: [Id] -> Id -> Id
pickFresh ctx nm
 | nm `elem` ctx = pickFresh ctx (nm ++ "'")
 | otherwise     = nm

prettyPrintType :: TypeL -> String
prettyPrintType BoolT = "Bool"
prettyPrintType IntT = "Int"
prettyPrintType (Prod t1 t2) =
  unwords [ "( Prod", prettyPrintType t1, prettyPrintType t2, ")" ]
prettyPrintType (Sum t1 t2) =
  unwords [ "( Sum", prettyPrintType t1, prettyPrintType t2, ")" ]
prettyPrintType (Arrow t1 t2) =
  unwords [ "(", prettyPrintType t1, "=>", prettyPrintType t2, ")" ]

-- print respecting Lam's syntax
prettyPrint :: Bool -> Expr -> String
prettyPrint = go []
  where ppBinOp Add = "+"
        ppBinOp Sub = "-"
        ppBinOp Mul = "*"
        ppBinOp And = "&&"
        ppBinOp Or = "||"
        ppBinOp LtInt = "<"
        ppBinOp MkPair = undefined
        ppUnOp Not = "!"
        ppUnOp Proj1 = "proj1"
        ppUnOp Proj2 = "proj2"
        go :: [Id] -> Bool -> Expr -> String
        go ctx isUntyped (Ite b t e) =
          let f = go ctx isUntyped in
          unwords ["(", "if", f b, "then", f t, "else", f e, ")"]
        go ctx isUntyped (BinOp MkPair e1 e2) = unwords
          ["( <", go ctx isUntyped e1, ",", go ctx isUntyped e2, "> )"]
        go ctx isUntyped (BinOp op e1 e2)     = unwords
          ["(", go ctx isUntyped e1, ppBinOp op, go ctx isUntyped e2, ")"]
        go ctx isUntyped (UnaryOp op e)   = unwords
          ["(", ppUnOp op, go ctx isUntyped e, ")"]
        go ctx isUntyped (Inl e t)   =
          if isUntyped then unwords ["inl", go ctx isUntyped e] else
            unwords ["inl", go ctx isUntyped e, "as", prettyPrintType t ]
        go ctx isUntyped (Inr e t)   =
          if isUntyped then unwords ["inr", go ctx isUntyped e] else
            unwords ["inr", go ctx isUntyped e, "as", prettyPrintType t ]
        go ctx isUntyped (Case e1 id2 e2 id3 e3) =
            unwords [ "( case"
                    , go ctx isUntyped e1
                    , "of inl"
                    , id2
                    , "=>"
                    , go (id2 : ctx) isUntyped e2
                    , "| inr"
                    , id3
                    , "=>"
                    , go (id3 : ctx) isUntyped e3
                    , ")"
                    ]
        go _ _ (Const (NumC z))      = show z
        go _ _ (Const (BoolC True))  = "true"
        go _ _ (Const (BoolC False)) = "false"
        go ctx _ (Var i) = fromJust $ lookupMaybe i ctx
        go ctx isUntyped (Lam n ty e) =
            let freshName = pickFresh ctx n
             in unwords $ [ "( lam"
                          , freshName
                          ] ++
                          -- show types depending on the parameter
                          [":: " <> prettyPrintType ty | not isUntyped] ++
                          [ "->"
                          , go (freshName : ctx) isUntyped e
                          , ")"
                          ]
        go ctx isUntyped (App e1 e2) =
           let f = go ctx isUntyped
           in unwords ["(", f e1, ".", f e2, ")"]
        go ctx isUntyped (Fix e) =
            unwords [ "( fix", go ctx isUntyped e, ")" ]

untypedPrettyPrint, typedPrettyPrint :: Expr -> String
untypedPrettyPrint = prettyPrint True
typedPrettyPrint   = prettyPrint False

putStrLnFlush :: String -> IO ()
putStrLnFlush str =
    putStrLn str >> hFlush stdout

putStrFlush :: String -> IO ()
putStrFlush str =
    putStr str >> hFlush stdout
