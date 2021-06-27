import Data.List

main :: IO ()
main = do
  putStrLn "Working!"

data Ty
  = UnitTy
  | BoolTy
  | NatTy
  | FnTy Ty Ty
  | InferTy
  deriving Eq

instance Show Ty where
  show UnitTy = "()"
  show BoolTy = "Bool"
  show NatTy = "Nat"
  show (FnTy param ret) = show param ++ " => " ++ show ret
  show InferTy = "_"

type Ctx = [(String, Ty)]

data Expr
  = Var String
  | UnitConst
  | BoolConst Bool
  | NatConst Int
  | FnExpr Param Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Ann Expr Ty

instance Show Expr where
  show (Var x) = x
  show UnitConst = "()"
  show (BoolConst x) = if x then "true" else "false"
  show (NatConst x) = show x
  show (FnExpr param body) = "{ |" ++ show param ++ "| " ++ show body ++ " }"
  show (App fn arg) = show fn ++ " " ++ show arg
  show (If cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no
  show (Ann expr ty) = show expr ++ ": " ++ show ty

data Param
  = Infer String
  | AnnParam String Ty

paramName :: Param -> String
paramName (Infer name) = name
paramName (AnnParam name _) = name

instance Show Param where
  show (Infer x) = x
  show (AnnParam x ty) = x ++ ": " ++ show ty

data Res a
  = Ok a
  | Err Error

instance Show a => Show (Res a) where
  show (Ok a) = "✔️: " ++ show a
  show (Err e) = "❗: " ++ show e

data Error
  = RootCause String
  | ResultingError String Error
  | MultiError Error Error

instance Show Error where
  show (RootCause explanation) = explanation ++ "."
  show (ResultingError extraInfo e) = extraInfo ++ " because...\n   " ++ show e
  show (MultiError e1 e2) = show e1 ++ "\nAlso...\n   " ++ show e2

toRes :: Maybe a -> Error -> Res a
toRes (Just a) _ = Ok a
toRes Nothing reason = Err reason

addError :: Res a -> String -> Res a
addError (Ok a) _ = Ok a
addError (Err err) errMsg = Err (ResultingError errMsg err)

multiError :: [Res a] -> Res a
multiError rs = Err $ go rs
  where 
    go ((Err e):[Ok _]) = e
    go [Err e] = e
    go ((Err e):es) = MultiError e (go es)
    go ((Ok _):es) = go es

infer :: Ctx -> Expr -> Res Ty
infer ctx (Var x) = toRes (lookup x ctx) reason
  where reason = RootCause ("The variable `" ++ x ++ "` is not defined anywhere")
infer ctx (UnitConst) = Ok UnitTy
infer ctx (BoolConst _) = Ok BoolTy
infer ctx (NatConst _) = Ok NatTy
infer ctx (Ann expr ty) = (check ctx expr ty) `addError` ("Expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`")
infer ctx (App fn arg) =
  case infer ctx fn of
       Ok (FnTy argTy retTy) ->
         case check ctx arg argTy of
              Ok _ -> Ok retTy
              err -> err `addError` ("The function`" ++ show fn ++ "` was expecting a `" ++ show argTy ++ "` but was given `" ++ show arg ++ "` which has a different type")
       err -> err `addError` ("I couldn't infer the type of `" ++ show fn ++ "` in function application")

infer ctx (FnExpr (AnnParam param paramTy) body) =
  case infer ctx body of
    Ok bodyTy -> Ok $ FnTy paramTy bodyTy
    err -> err
infer ctx (FnExpr (Infer param) body) = Err $ RootCause $ "I can't infer the type of the parameter `" ++ param ++ "`"
infer _ expr = Err $ RootCause $ "I don't have enough information to infer the type of `" ++ show expr ++ "`"

check :: Ctx -> Expr -> Ty -> Res Ty
check ctx (If cond yes no) ty =
  case ( check ctx cond BoolTy
       , check ctx yes ty
       , check ctx no ty
       ) of
         (Ok _, Ok _, Ok _) -> Ok ty
         (condErr@(Err _), yesRes, noRes) ->
           multiError [condErr', yesRes, noRes]
            where condErr' = condErr `addError` ("The condition of an `if` must have type `" ++ show BoolTy ++ "`, but this one doesn't")
         (_, yesRes, noRes) -> multiError [yesRes, noRes]
       
check ctx fn@(FnExpr param body) (FnTy paramTy bodyTy) =
  let pName = paramName param
  in case check ((pName, paramTy) : ctx) body bodyTy of
       Ok _ -> Ok (FnTy paramTy bodyTy)
       err -> err `addError` ("Body of function `" ++ show fn ++ "` does not match expected type `" ++ show bodyTy ++ "`")

check ctx expr ty = 
  case infer ctx expr of
       Ok ty' | ty' == ty || ty == InferTy -> Ok ty'
       Ok ty' -> Err $ RootCause $ "Expression `" ++ show expr ++ "` has type `" ++ show ty' ++ "`, not `" ++ show ty ++ "`"
       err -> err `addError` ("I couldn't typecheck `" ++ show expr ++ "`")

-- print $ check [] (Ann (FnExpr (AnnParam "x" NatTy) (If (NatConst 1) (App (Var "f") UnitConst) (FnExpr (Infer "z") UnitConst))) (FnTy InferTy NatTy)) UnitTy
-- print $ check [] (BoolConst True) InferTy
