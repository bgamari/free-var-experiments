module Step2 (freeVars) where

import Expr
import VarSet

-- | A computation to build a free variable set.
newtype FV = FV { runFV :: VarSet   -- bound variable set
                        -> VarSet   -- the accumulator
                        -> VarSet   -- the result
                }

instance Monoid FV where
  mempty = FV $ \_ acc -> acc

instance Semigroup FV where
  fv1 <> fv2 = FV $ \boundVars acc -> 
    runFV fv1 boundVars (runFV fv2 boundVars acc)

-- | Introduce a variable binding.
bindVar :: Var -> FV -> FV
bindVar v fv = FV $ \boundVars acc ->
  runFV fv (insertVarSet v boundVars) acc

unitFV :: Var -> FV
unitFV v = FV $ \boundVars acc ->
  if memberVarSet v boundVars
  then acc
  else insertVarSet v acc

fvToVarSet :: FV -> VarSet
fvToVarSet fv = runFV fv mempty mempty

exprFV :: Expr -> FV
exprFV (EVar v)       = unitFV v
exprFV (ELam v e)     = bindVar v (exprFV e)
exprFV (EApp fun arg) = exprFV fun <> exprFV arg
exprFV (ELit _)       = mempty

freeVars = fvToVarSet . exprFV
