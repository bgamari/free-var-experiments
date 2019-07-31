import System.Environment
import Test.QuickCheck

import Expr

succBinders :: Expr -> Expr
succBinders = go
  where
    go (EVar v)   = EVar v
    go (ELam v e) = ELam (succVar v) (go e)
    go (EApp a b) = EApp (go a) (go b)
    go e@(ELit _) = e

    succVar (Var n) = Var (n+1)

arbExpr :: Gen Expr
arbExpr = do
  s <- getSize
  frequency
    [ (30, leaf)
    , (s, node)
    ]
  where
    var = Var <$> arbitrary
    leaf = oneof
      [ EVar <$> var
      , pure $ ELit 0
      ]
    node = oneof
      [ ELam (Var 0) <$> arbExpr
      , EApp <$> arbExpr <*> arbExpr
      ]

main = do
  args <- getArgs
  let n = case args of
            [] -> 30
            n:_ -> read n
  generate (resize n arbExpr) >>= print
