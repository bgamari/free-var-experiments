module Step1 where 

import Expr
import VarSet

freeVars' :: VarSet    -- ^ bound variable set
          -> Expr      -- ^ the expresson we want the free variables of
          -> VarSet    -- ^ the accumulator
          -> VarSet    -- ^ the final free variable set

freeVars' boundVars (EVar v) acc
  | memberVarSet v boundVars
  = acc  -- the variable isn't free, nothing to do

  | otherwise
  = insertVarSet v acc
freeVars' boundVars (ELam v e) acc
  = let -- we are binding 'v' so add it to our bound variable set
        boundVars' = insertVarSet v boundVars
    in freeVars' boundVars' e acc
freeVars' boundVars (EApp fun arg) acc
  = freeVars' boundVars fun (freeVars' boundVars arg acc)
freeVars' _boundVars (ELit _) acc
  = acc

freeVars :: Expr -> VarSet
freeVars e = freeVars' mempty e mempty

