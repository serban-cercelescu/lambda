module Lambda where

data Lambda = App Lambda Lambda
            | Abs String Lambda
            | Var String
            | Macro String
            deriving (Eq)

freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (Abs x t) = filter (/= x) (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Macro _) = error "Encountered Macro in freeVars"

betaReduction :: Lambda -> Lambda
betaReduction (App (Abs x t1) t2) = substitute t1 x t2
betaReduction anything = anything
betaReduction (Macro _) = error "Encountered Macro in betaReduction"

substitute :: Lambda -> String -> Lambda -> Lambda
substitute (Var x) y t = if x == y then t else Var x
substitute (Abs x t1) y t = if x == y then Abs x t1 else Abs x (substitute t1 y t)
substitute (App t1 t2) y t = App (substitute t1 y t) (substitute t2 y t)
substitute (Macro _) _ _ = error "Encountered Macro in substitute"

alphaConversion :: String -> String -> Lambda -> Lambda
alphaConversion x y (Var z) = if z == x then Var y else Var z
alphaConversion x y (Abs z t) = if z == x then Abs z t else Abs z (alphaConversion x y t)
alphaConversion x y (App t1 t2) = App (alphaConversion x y t1) (alphaConversion x y t2)
alphaConversion x y (Macro _) = error "Encountered Macro in alphaConversion"

reduction :: Lambda -> Maybe Lambda -- perform the uppermost beta reduction available
reduction (App (Abs x t1) t2) = Just $ substitute t1 x t2
reduction (App t1 t2) = case reduction t1 of
                         Just t1' -> Just $ App t1' t2
                         Nothing -> case reduction t2 of
                                      Just t2' -> Just $ App t1 t2'
                                      Nothing -> Nothing
reduction (Abs x t) = case reduction t of
    Just t' -> Just $ Abs x t'
    Nothing -> Nothing

reduction _ = Nothing

tryReduce :: Lambda -> Lambda
tryReduce t = case reduction t of
    Just t' -> tryReduce t'
    Nothing -> t

instance Show Lambda where
    show (Var name) = name
    show (Abs name body) = "λ" ++ name ++ "." ++ show body 
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Macro name) = '$':name
