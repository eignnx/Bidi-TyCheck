import Data.List

main :: IO ()
main = do
  putStrLn "Working!"

data Ty
  = UnitTy
  | BoolTy
  | NatTy
  | FnTy Ty Ty
  deriving Eq

instance Show Ty where
  show UnitTy = "()"
  show BoolTy = "Bool"
  show NatTy = "Nat"
  show (FnTy param ret) = show param ++ " => " ++ show ret

type Ctx = [(String, Ty)]

data Expr
  = Var String
  | UnitConst
  | BoolConst Bool
  | NatConst Int
  | FnExpr String Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Ann Expr Ty

instance Show Expr where
  show (Var x) = x
  show UnitConst = "()"
  show (BoolConst x) = if x then "true" else "false"
  show (NatConst x) = show x
  show (FnExpr param body) = "{ |" ++ param ++ "| " ++ show body ++ " }"
  show (App fn arg) = show fn ++ " " ++ show arg
  show (If cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no
  show (Ann expr ty) = show expr ++ ": " ++ show ty

data Res a
  = Ok a
  | Err [String]

instance Show a => Show (Res a) where
  show (Ok a) = "✔️: " ++ show a
  show (Err []) = "❗: Something went wrong but I'm not sure what."
  show (Err es) = "❗: " ++ intercalate " because...\n   " es ++ "."

toRes :: Maybe a -> String -> Res a
toRes (Just a) _ = Ok a
toRes Nothing reason = Err [reason]

mapErr :: Res a -> String -> Res a
mapErr (Ok a) _ = Ok a
mapErr (Err reason) reason' = Err (reason' : reason)

infer :: Ctx -> Expr -> Res Ty
infer ctx (Var x) = toRes (lookup x ctx) reason
  where reason = "Unbound variable `" ++ x ++ "`"
infer ctx (UnitConst) = Ok UnitTy
infer ctx (BoolConst _) = Ok BoolTy
infer ctx (NatConst _) = Ok NatTy
infer ctx (Ann expr ty) = mapErr (check ctx expr ty) reason
  where reason = "Expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`"
infer ctx (App fn arg) =
  case infer ctx fn of
       Ok (FnTy argTy retTy) -> case check ctx arg argTy of
                                        Ok _ -> Ok retTy
                                        Err r -> Err (("The function`" ++ show fn ++ "` was expecting a `" ++ show argTy ++ "` but was given `" ++ show arg ++ "` which has a different type") : r)
       Err r -> Err (("I couldn't infer the type of `" ++ show fn ++ "` in function application") : r)

infer ctx (FnExpr param body) = Err ["I can't infer the type of the parameter `" ++ param ++ "`"]
infer _ expr = Err ["I don't have enough information to infer the type of `" ++ show expr ++ "`"]


check :: Ctx -> Expr -> Ty -> Res Ty
check ctx (If cond yes no) ty =
  case ( check ctx cond BoolTy
       , check ctx yes ty
       , check ctx no ty
       ) of
         (Ok _, Ok _, Ok _) -> Ok ty
         (Err r, Ok _, Ok _) -> Err (("The condition of an `if` must have type `" ++ show BoolTy ++ "`, but this one doesn't") : r)
         _ -> Err ["Hmmm, type error while checking an if expression"]
       
check ctx fn@(FnExpr param body) (FnTy paramTy bodyTy) =
  case check ((param, paramTy) : ctx) body bodyTy of
       Ok _ -> Ok (FnTy paramTy bodyTy)
       Err r -> Err (("Body of function `" ++ show fn ++ "` does not match expected type `" ++ show bodyTy ++ "`") : r)

check ctx expr ty = 
  case infer ctx expr of
       Ok ty' -> if ty' == ty
                   then Ok ty
                   else Err ["Expression `" ++ show expr ++ "` has type `" ++ show ty' ++ "`, not `" ++ show ty ++ "`"]
       Err r -> Err (("I could not check type of `" ++ show expr ++ "`") : r)
