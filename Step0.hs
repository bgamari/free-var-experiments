module Step0 where 

import Expr
import VarSet

exprFV :: Expr -> VarSet
exprFV (EVar v) = unitVarSet v
exprFV (ELam v e) = delVarSet v (exprFV e)
exprFV (EApp e f) = exprFV e <> exprFV f
exprFV (ELit _) = mempty
