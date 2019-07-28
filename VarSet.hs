{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VarSet where

import qualified Data.IntSet as IS

import Expr

newtype VarSet = VarSet IS.IntSet
  deriving (Monoid, Semigroup, Show)

unitVarSet :: Var -> VarSet
unitVarSet (Var v) = VarSet $ IS.singleton v

delVarSet :: Var -> VarSet -> VarSet
delVarSet (Var v) (VarSet xs) = VarSet $ IS.delete v xs

insertVarSet :: Var -> VarSet -> VarSet
insertVarSet (Var v) (VarSet xs) = VarSet $ IS.insert v xs

memberVarSet :: Var -> VarSet -> Bool
memberVarSet (Var v) (VarSet xs) = IS.member v xs
