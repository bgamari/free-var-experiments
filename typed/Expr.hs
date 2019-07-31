module Expr where

data Type
  = TVar Var
  | TForall Var Type
  | TApp Type Type
  | TExpr Expr

data Expr
  = EVar Var Type
  | ELam Var Expr
  | EApp Expr Expr
  | ELit Integer

newtype Var = Var Int
  deriving (Eq, Ord, Show)

