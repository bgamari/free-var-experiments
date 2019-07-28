module Step3 where 

import Expr
import VarSet

class Monoid a => FreeVarStrategy a where
  unitFV :: Var -> a
  bindVar :: Var -> a -> a

exprFV :: FreeVarStrategy a => Expr -> a
exprFV (EVar v)       = unitFV v
exprFV (ELam v e)     = bindVar v (exprFV e)
exprFV (EApp fun arg) = exprFV fun <> exprFV arg
exprFV (ELit _)       = mempty


-- | The naive approach from "Step0".
newtype Naive = Naive { runNaive :: VarSet }

instance Monoid Naive where
  mempty = Naive mempty
instance Semigroup Naive where
  Naive a <> Naive b = Naive (a <> b)
instance FreeVarStrategy Naive where
  unitFV = Naive . unitVarSet
  bindVar v (Naive xs) = Naive (delVarSet v xs)


-- | The optimised approach from "Step2".
newtype FV = FV { runFV :: VarSet   -- bound variable set
                        -> VarSet   -- the accumulator
                        -> VarSet   -- the result
                }

instance Monoid FV where
  mempty = FV $ \_ acc -> acc
instance Semigroup FV where
  fv1 <> fv2 = FV $ \boundVars acc -> 
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

instance Semigroup NoFreeVars where
  NoFreeVars f <> NoFreeVars g = NoFreeVars $ \boundVars ->
    f boundVars && g boundVars

instance FreeVarStrategy NoFreeVars where
  unitFV v = NoFreeVars $ \boundVars -> memberVarSet v boundVars
  bindVar v (NoFreeVars f) = NoFreeVars $ \boundVars ->
    f $ insertVarSet v boundVars

noFreeVars :: NoFreeVars -> Bool
noFreeVars (NoFreeVars f) = f mempty


{-# SPECIALISE exprFV :: Expr -> Naive #-}
{-# SPECIALISE exprFV :: Expr -> FV #-}
{-# SPECIALISE exprFV :: Expr -> NoFreeVars #-}
