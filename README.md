# Haskell-lambda-calculus
Progetto di Functional Programming: semplice collezione di funzioni per giocare con il lambda calcolo.

# Implementazioni:
Le definzioni di base sono:
 - data LambdaExpr = Name String | Apply LambdaExpr LambdaExpr | Lambda String LambdaExpr  deriving (Show)
 - is_free :: String -> LambdaExpr -> Bool: verifica se una variabile sia libera o meno
 - l2s :: LambdaExpr -> String: converte un'espressione in una stringa leggibile

Al momento, sono state implementate le expressioni nel lambda calcolo:
 - Ω = ωω = (λx.xx)(λx.xx)
 - succ = λn.λf.λx.f((nf)x)
 - sum = λn.λm.λf.λx.(nf)((mf)x)
 - zero = λf.λx.x
 - false = λt.λf.f
 - true = λt.λf.t
 - cond = λc.λa.λb.cab
 - fixed_point_combinator

Sono presenti le seguenti funzioni di appoggio:
 - printLambda :: LambdaExpr -> IO()
 - apply :: LambdaExpr -> LambdaExpr -> LambdaExpr
 - lambda2num :: LambdaExpr -> Maybe Int
 - num2lambda :: Int -> LambdaExpr
 - lambdaNumbers :: \[LambdaExpr\]
