import Criterion
import Criterion.Main
import Expr
import Step3

readExpr :: FilePath -> IO Expr
readExpr f = do
  e <- read <$> readFile f
  return $! e

main = do
  e <- readExpr "in"
  defaultMain
    [ bench "exprNoFreeVars" (whnf exprNoFreeVars e)
    , bench "exprFreeVars" (whnf exprFreeVars e)
    , bench "exprFreeVarsNaive" (whnf exprFreeVarsNaive e)
    ]
