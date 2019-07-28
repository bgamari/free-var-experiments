module Expr where

data Expr
  = EVar Var
  | ELam Var Expr
  | EApp Expr Expr
  | ELit Integer

newtype Var = Var Int
  deriving (Eq, Ord, Show)

