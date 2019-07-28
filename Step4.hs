module Step4 where 

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


newtype NoFreeVars = NoFreeVars { runNoFreeVars :: VarSet   -- bound variable set
                                                -> Bool     -- True is free variable set is empty
                                }

instance Monoid NoFreeVars where
  mempty = NoFreeVars $ const True
  {-# INLINE mempty #-}

instance Semigroup NoFreeVars where
  NoFreeVars f <> NoFreeVars g = NoFreeVars $ \boundVars ->
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

{-# SPECIALISE exprFV :: Expr -> NoFreeVars #-}
