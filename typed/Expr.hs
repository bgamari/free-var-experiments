module Expr where

data Type
  = TVar Var
  | TForall Var Type
  | TApp Type Type
  | TExpr Expr
  deriving (Show, Read)

data Expr
  = EVar Var Type
  | ELam Var Expr
  | EApp Expr Expr
  | ELit Integer
  deriving (Show, Read)

newtype Var = Var Int
  deriving (Eq, Ord, Show, Read)

