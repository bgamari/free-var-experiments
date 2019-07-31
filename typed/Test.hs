module Test where

import GHC.Exts

import Expr
import VarSet

class Monoid a => FreeVarStrategy a where
  unitFV :: Var -> a
  bindVar :: Var -> a -> a


-- | The optimised approach from "Step2".
newtype FV = FV { runFV :: VarSet   -- bound variable set
                        -> VarSet   -- the accumulator
                        -> VarSet   -- the result
                }

instance Monoid FV where
  mempty = FV $ \_ acc -> acc
instance Semigroup FV where
  fv1 <> fv2 = FV $ oneShot $ \boundVars -> oneShot $ \acc -> 
    runFV fv1 boundVars (runFV fv2 boundVars acc)
instance FreeVarStrategy FV where
  unitFV v = FV $ \boundVars acc ->
    if memberVarSet v boundVars
    then acc
    else insertVarSet v acc
  bindVar v fv = FV $ \boundVars acc ->
    runFV fv (insertVarSet v boundVars) acc

fvToVarSet :: FV -> VarSet
fvToVarSet fv = runFV fv mempty mempty


newtype NoFreeVars = NoFreeVars { runNoFreeVars :: VarSet   -- bound variable set
                                                -> Bool     -- True is free variable set is empty
                                }

instance Monoid NoFreeVars where
  mempty = NoFreeVars $ const True
  {-# INLINE mempty #-}

instance Semigroup NoFreeVars where
  NoFreeVars f <> NoFreeVars g = NoFreeVars $ oneShot $ \boundVars ->
    f boundVars && g boundVars
  {-# INLINE (<>) #-}

instance FreeVarStrategy NoFreeVars where
  unitFV v = NoFreeVars $ \boundVars -> memberVarSet v boundVars
  bindVar v (NoFreeVars f) = NoFreeVars $ \boundVars ->
    f $! insertVarSet v boundVars

  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

noFreeVars :: NoFreeVars -> Bool
noFreeVars (NoFreeVars f) = f mempty


typeFV :: FreeVarStrategy a => Type -> a
typeFV (TVar v)       = unitFV v
typeFV (TForall v ty) = bindVar v (typeFV ty)
typeFV (TApp ty1 ty2) = typeFV ty1 <> typeFV ty2
typeFV (TExpr e)      = exprFV e

exprFV :: FreeVarStrategy a => Expr -> a
exprFV (EVar v ty)    = unitFV v <> typeFV ty
exprFV (ELam v e)     = bindVar v (exprFV e)
exprFV (EApp fun arg) = exprFV fun <> exprFV arg
exprFV (ELit _)       = mempty

{-# SPECIALISE exprFV :: Expr -> NoFreeVars #-}
{-# SPECIALISE exprFV :: Expr -> FV #-}
