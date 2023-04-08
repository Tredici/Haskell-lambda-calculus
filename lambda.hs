import Data.List

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
{- ++++++++++++++++++++++++++++++++++++++++++++ DEFINIZIONI +++++++++++++++++++++++++++++++++++++++++++++ -}
{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

data LambdaExpr = Name String | Apply LambdaExpr LambdaExpr | Lambda String LambdaExpr  deriving (Show)

{- VERIFICA CHE LA VARIABILE DATA SIA LIBERA -}
is_free :: String -> LambdaExpr -> Bool
is_free v (Name n)      = (v == n)
is_free v (Apply e1 e2) = (is_free v e1) || (is_free v e2)
is_free v (Lambda n e)  = if (v == n) then False else is_free v e


{- TRASFORMA IN FORMATO LEGGIBILE LA LAMBDA -}
l2s :: LambdaExpr -> String
l2s (Name n)      = n
l2s (Apply e1 e2) = "(" ++ (l2s e1) ++ " " ++ (l2s e2) ++ ")"
l2s (Lambda n e)  = "\\" ++ n ++ "." ++ (l2s e)


{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
{- ++++++++++++++++++++++++++++++++++++++++++++++ CONTEGGI ++++++++++++++++++++++++++++++++++++++++++++++ -}
{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
{- OTTIENE TUTTI I NOMI (senza duplicazioni) PRESENTI IN UN'ESPRESSIONE -}
allNames :: LambdaExpr -> [String]
allNames = nub . helper
  where helper (Name n) = [n]
        helper (Lambda n lexpr) = n:helper lexpr
        helper (Apply l1 l2) = helper l1 ++ helper l2

{- OTTIENE TUTTI I NOMI DI VARIABILI LIBERE PRESENTI IN UN'ESPRESSIONE -}
frees = map is_free . allNames

{- CONTA IL NUMERO DI NOMI (anche duplicati) PRESENTI NELL'ESPRESSIONE -}
countLambda :: LambdaExpr -> Integer
countLambda (Name _) = 0
countLambda (Lambda _ expr) = 1 + countLambda expr
countLambda (Apply le1 le2) = countLambda le1 + countLambda le2


{- RINOMINA UNA VARIABILE ESISTENTE CON UN ALTRO NOME FORNITO, DA UTILIZZARSI PER EVITARE CONFLITTI IN AUSILIO A "replace" -}
rename :: String -> String -> LambdaExpr -> LambdaExpr
rename old_x new_z expr = case expr of (Name n) -> Name (if n == old_x then new_z else n)
                                       (Apply le1 le2) -> Apply (rename old_x new_z le1) (rename old_x new_z le2)
                                       (Lambda name lexpr) -> if name == new_z then (Lambda name lexpr) else 
                                                              (Lambda (if name == old_x then new_z else name) (rename old_x new_z lexpr))


{- RIMPIAZZA LA VARIABILE IDENTIFICATA DALLA STRINGA  -}
replace :: String -> LambdaExpr -> LambdaExpr -> LambdaExpr
replace x subs expr = case expr of (Name n) -> if n == x then subs else (Name n)
                                   (Apply le1 le2) -> Apply (replace x subs le1) (replace x subs le2)
                                   (Lambda param le) -> if param == x then (Lambda param le)    -- no replacement necessary due to shadowing
                                                        else if is_free param subs then (       -- in case of conflicting names has to find a new one
                                                            -- search a name that do not cause conflicts
                                                            let newname = head . filter (\s -> not $ is_free s subs) . map (\n -> concat . take n . repeat $ param)  $ [2..]
                                                            in (Lambda newname $ replace x subs (rename param newname le))
                                                        )
                                                        else (Lambda param $ replace x subs le) -- perform replacement in sub expression



{- PERFORM A SINGLE (in order to avoid infinite loops) REDUCTION STEP IN THE LAMBDA EXPRESSION -}
reduce :: LambdaExpr -> LambdaExpr
reduce (Name n) = Name n
reduce (Lambda n le) = Lambda n $ reduce le
reduce (Apply (Name x) (Name y)) = Apply (Name x) (Name y)
reduce (Apply (Name x) le2) = Apply (Name x) $ reduce le2
reduce (Apply (Lambda param lambda) le2) = replace param (reduce le2) lambda
reduce (Apply le1 le2) = Apply (reduce le1) (reduce le2)


{- sometimes a single reduction step is not sufficient -}
reduceMany :: LambdaExpr -> LambdaExpr
reduceMany expr = let red = rep reduce (countLambda expr) in red expr
    where rep f 0 = id
          rep f 1 = f
          rep f n = (rep f $ n-1) . f

{- succ (λn.λf.λx.f((nf)x)) -}
succ :: LambdaExpr
succ = Lambda "n" $ Lambda "f" $ Lambda "x" $ Apply (Name "f") $ Apply (Apply (Name "n") (Name "f")) $ Name "x"


applySucc :: LambdaExpr -> LambdaExpr
applySucc = reduceMany . Apply Main.succ


{- GENERA TUTTI I NUMERI NATURALI COME ESPRESSIONI LAMBDA -}
lambdaNumbers :: [LambdaExpr]
lambdaNumbers = map gen [0..]
    where gen 0 = zero
          gen n = reduce $ applySucc (lambdaNumbers !! (n-1))

{- Fornisce il numero naturale corrispondente alla codifica data -}
num2lambda :: Int -> LambdaExpr
num2lambda = (!!) lambdaNumbers

{- STAMPA I NUMERI NATURALI COME GENERATI DAL LAMBDA CALCOLO -}
-- sequence . map putStrLn . map l2s . take 10 $ lambdaNumbers
-- mapM_ (print.l2s) . take 10 $ lambdaNumbers

{- STAMPA UN SINGOLO NUMERO NATURALE ESPRESSO CON IL LAMBDA CALCOLO -}
printN = putStrLn . l2s . num2lambda


{- (λn.λm.λf.λx.(nf)((mf)x)) -}
sumLambda :: LambdaExpr
sumLambda = Lambda "n" $ Lambda "m" $ Lambda "f"  $ Lambda "x" $ Apply (Apply (Name "n") $ Name "f") (Apply (Apply (Name "m") $ Name "f") $ Name "x")

sum :: LambdaExpr -> LambdaExpr -> LambdaExpr
sum n m = reduceMany $ Apply (Apply sumLambda n) m

mulLambda :: LambdaExpr
mulLambda = Lambda "m" $ Lambda "n" $ Lambda "f" $(Apply (Name "m") $ (Apply (Name "n") $ Name "f"))

powLambda :: LambdaExpr
powLambda = Lambda "m" $ Lambda "n" $ Apply (Name "n") $ Name "m"

printLambda :: LambdaExpr -> IO()
printLambda = putStrLn . l2s

{- apply lambda expression -}
apply :: LambdaExpr -> LambdaExpr -> LambdaExpr
apply n m = reduceMany . Apply n $ m

lambda2num :: LambdaExpr -> Maybe Int
lambda2num expr = let (_,_,count_f,is_valid) = helper (Nothing :: Maybe [Char], Nothing :: Maybe [Char], 0, True) expr
    in if is_valid then Just count_f else Nothing
    -- status (Maybe "f", Maybe "x", count_f, is_valid)
    where fail = (Nothing, Nothing, 0, False) -- to be returned in case of error
          -- catch worst fail
          helper (_,_,_,False) expr = fail
          -- check first term
          helper (Nothing,Nothing,0,True) expr = case expr of (Lambda f le) -> helper (Just f, Nothing, 0, True) le
                                                              _ -> fail
          -- catch second term
          helper (Just f,Nothing,0,True) expr  = case expr of (Lambda x le) -> if f==x then fail else (helper (Just f, Just x, 0, True) le)
                                                              _ -> fail
          -- check core
          helper (Just f,Just x,n,True) expr  =  case expr of (Name var) -> if var == x then (Just f,Just x,n,True) else fail
                                                              (Apply (Name var) le) -> if var /= f then fail else (helper (Just f, Just x, n+1, True) le)
                                                              _ -> fail
          -- general fail
          helper _ _ = fail

{- λf.λx.x -}
zero :: LambdaExpr
zero = Lambda "f" (Lambda "x" (Name "x"))

{- λt.λf.f -}
false :: LambdaExpr
false = Lambda "t" (Lambda "f" (Name "f"))

{- λt.λf.t -}
true :: LambdaExpr
true = Lambda "t" (Lambda "f" (Name "t"))

{- λc.λa.λb.cab -}
cond :: LambdaExpr
cond = Lambda "c" $ Lambda "a" $ Lambda "b" (Apply (Apply (Name "c") $ Name "a") $ Name "b")

-- expression reduced to one
calc1 = Apply (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Name "f") (Apply (Apply (Name "n") (Name "f")) (Name "x")))))) (Lambda "f" (Lambda "x" (Name "x")))
-- putStrLn $ l2s $ reduceMany  calc1
one = Lambda "f" (Lambda "x" (Apply (Name "f") (Name "x")))

fixed_point_combinator :: LambdaExpr
fixed_point_combinator = Lambda "f" $ Apply  (Lambda "x" $ Apply (Name "f") $ Apply (Name "x") $ Name "x") (Lambda "x" $ Apply (Name "f") $ Apply (Name "x") $ Name "x")

{- Ω = ωω = (λx.xx)(λx.xx) -}
omega_combinator :: LambdaExpr
omega_combinator = Apply fun fun
    where fun = Lambda "x" $ Apply (Name "x") (Name "x")
