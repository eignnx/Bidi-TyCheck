module Ty ( Ty(..)
          , unitTy
          , boolTy
          , natTy
          , listTy
          , setTy
          , mapTy
          , optionTy
          , resultTy
          ) where

data Ty
  = PolyTy String [Ty]
  | FnTy Ty Ty -- Yes, I know this could be expressed as `PolyTy "=>" [a, b]`. Sorry not sorry.
  | InferTy
  deriving Eq

unitTy = PolyTy "()" []
boolTy = PolyTy "Bool" []
natTy = PolyTy "Nat" []
listTy a = PolyTy "$List" [a] -- Singly-linked list/Vector<T> from Bodil's `im` crate
setTy a = PolyTy "$Set" [a]
mapTy k v = PolyTy "$Map" [k, v]
optionTy a = PolyTy "$Option" [a]
resultTy t e = PolyTy "$Result" [t, e]

instance Show Ty where
  show (PolyTy "$List" [a]) = "[" ++ show a ++ "]"
  show (PolyTy "$Set" [a]) = "#[" ++ show a ++ "]"
  show (PolyTy "$Map" [k, v]) = "#[" ++ show k ++ " : " ++ show v ++ "]"
  show (PolyTy "$Option" [a]) = "?" ++ show a
  show (PolyTy "$Result" [t, e]) = show t ++ "!" ++ show e
  show (PolyTy name []) = name
  show (FnTy paramTy retTy) = show paramTy ++ " => " ++ show retTy
  show InferTy = "_"