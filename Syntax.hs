module Syntax ( Expr(..)
              , Param(..)
              , paramName
              ) where

import Ty ( Ty(..)
          )

data Expr
  = Var String
  | UnitConst
  | BoolConst Bool
  | NatConst Integer
  | FnExpr Param Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Ann Expr Ty
  | Let Param Expr Expr

instance Show Expr where
  show (Var x) = x
  show UnitConst = "()"
  show (BoolConst x) = if x then "true" else "false"
  show (NatConst x) = show x
  show (FnExpr param body) = "{ |" ++ show param ++ "| " ++ show body ++ " }"
  show (App fn arg) = show fn ++ " " ++ show arg
  show (If cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no
  show (Ann expr ty) = show expr ++ ": " ++ show ty
  show (Let var binding body) = "let " ++ show var ++ " = " ++ show binding ++ " in " ++ show body

data Param
  = Infer String
  | AnnParam String Ty

paramName :: Param -> String
paramName (Infer name) = name
paramName (AnnParam name _) = name

instance Show Param where
  show (Infer x) = x
  show (AnnParam x ty) = x ++ ": " ++ show ty