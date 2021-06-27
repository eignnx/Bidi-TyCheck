module Main ( main
            , infer
            , check
            ) where

import Data.List
import System.IO

import Parse (parseExpr)
import Ty
import Syntax

main :: IO ()
main = do
  putStr "::> "
  hFlush stdout
  input <- getLine
  print $ parseExpr input
  main


type Ctx = [(String, Ty)]

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
infer ctx (UnitConst) = Ok unitTy
infer ctx (BoolConst _) = Ok boolTy
infer ctx (NatConst _) = Ok natTy
infer ctx (Ann expr ty) =
  (check ctx expr ty) `addError` ("Expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`")
infer ctx (App fn arg) =
  case infer ctx fn of
       Ok (FnTy argTy retTy) ->
         case check ctx arg argTy of
              Ok _ -> Ok retTy
              err -> err `addError` ("The function `" ++ show fn ++ "` was expecting a `" ++ show argTy ++ "` but was given `" ++ show arg ++ "`. This is a problem")
       Ok wrongFnTy -> Err $ RootCause $ "`" ++ show fn ++ "` is a `" ++ show wrongFnTy ++ "`, not a function"
       err -> err

infer ctx (FnExpr (AnnParam param paramTy) body) =
  let ctx' = (param, paramTy) : ctx
  in case infer ctx' body of
    Ok bodyTy -> Ok $ FnTy paramTy bodyTy
    err -> err
infer ctx fn@(FnExpr (Infer param) body) =
  Err $ RootCause $ "I can't infer the type of the parameter `" ++ param ++ "` in the function `" ++ show fn ++ "`"

infer ctx ifExpr@(If cond yes no) =
  case (infer ctx yes, infer ctx no) of
       (Ok yesTy, _) -> check ctx ifExpr yesTy
       (_, Ok noTy) -> check ctx ifExpr noTy
       (err1, err2) -> multiError [err1, err2] `addError` "I couldn't infer the type of the `if` expression"

infer _ expr = Err $ RootCause $ "I don't have enough information to infer the type of `" ++ show expr ++ "`"

check :: Ctx -> Expr -> Ty -> Res Ty
check ctx (If cond yes no) ty =
  case ( check ctx cond boolTy
       , check ctx yes ty
       , check ctx no ty
       ) of
         (Ok _, Ok _, Ok _) -> Ok ty
         (condErr@(Err _), yesRes, noRes) ->
           multiError [condErr', yesRes, noRes]
            where condErr' = condErr `addError` ("The condition of an `if` must have type `" ++ show boolTy ++ "`, but this one doesn't")
         (_, yesRes, noRes) -> multiError [yesRes, noRes]
       
check ctx fn@(FnExpr param body) (FnTy paramTy retTy) =
  let pName = paramName param
  in case check ((pName, paramTy) : ctx) body retTy of
       Ok _ -> Ok (FnTy paramTy retTy)
       err -> err `addError` ("Body of function `" ++ show fn ++ "` does not match expected type `" ++ show retTy ++ "`")

check ctx (Let (AnnParam var varTy) binding body) ty =
  case check ctx binding varTy of
       Ok _ -> check ((var, varTy):ctx) body ty
       err -> err `addError` ("The variable `" ++ var ++ "` is declared as a `" ++ show varTy ++ "`, but is bound to `" ++ show binding ++ "`. This is a problem")
  -- check ctx (App (FnExpr var body) binding) ty

check ctx (Let (Infer var) binding body) ty =
  case infer ctx binding of
       Ok varTy -> check ((var, varTy):ctx) body ty
       err -> err `addError` ("The declaration of `" ++ var ++ "` needs a type annotation")

check ctx expr ty = 
  case infer ctx expr of
       Ok ty' | ty' == ty || ty == InferTy -> Ok ty'
       Ok ty' -> Err $ RootCause $ "Expression `" ++ show expr ++ "` has type `" ++ show ty' ++ "`, not `" ++ show ty ++ "`"
       err -> err `addError` ("The expression `" ++ show expr ++ "` doesn't typecheck")

-- print $ check [] (Ann (FnExpr (AnnParam "x" NatTy) (If (NatConst 1) (App (Var "f") UnitConst) (FnExpr (Infer "z") UnitConst))) (FnTy InferTy NatTy)) unitTy
-- print $ check [] (BoolConst True) InferTy
-- print $ check [] (Let (Infer "x") (FnExpr (Infer "y") UnitConst) UnitConst) InferTy
-- print $ infer [] (If (BoolConst True) UnitConst UnitConst)
-- print $ infer [] (If (BoolConst True) (Var "foo") (FnExpr (Infer "x") UnitConst))