module ExpressionUtils (
  simplify,
  prettify
  ) where

import Expression

isZero :: Expression -> Bool
isZero (Val u) = (read u :: Float) == 0
isZero _ = False
  
isOne :: Expression -> Bool
isOne (Val u) = (read u :: Float) == 1
isOne _ = False
  
  
-- reduce zeros and ones in operations  
simplify :: Expression -> Expression
simplify (Neg (Neg v)) = simplify v
simplify (Neg u) 
  | isZero u = Val "0"
  | simplify u /= u = simplify (Neg (simplify u))
  | otherwise = Neg (simplify u)
simplify (Add u v)
  | isZero (simplify u) = simplify v
  | isZero (simplify v) = simplify u
  | simplify u /= u || simplify v /= v = simplify (Add (simplify u) (simplify v))
  | otherwise = Add (simplify u) (simplify v)
simplify (Sub u v)
  | isZero (simplify u) = simplify (Neg v)
  | isZero (simplify v) = simplify u
  | simplify u == simplify v = Val "0"
  | simplify u /= u || simplify v /= v = simplify (Sub (simplify u) (simplify v))
  | otherwise = Sub (simplify u) (simplify v)
simplify (Mul u v)
  | isZero (simplify u) || isZero (simplify v) = Val "0"
  | isOne (simplify u) = simplify v
  | isOne (simplify v) = simplify u
  | simplify u /= u || simplify v /= v = simplify (Mul (simplify u) (simplify v))
  | otherwise = Mul (simplify u) (simplify v)
simplify (Div u v) 
  | isZero (simplify u) = Val "0"
  | isOne (simplify v) = simplify u
  | simplify u /= u || simplify v /= v = simplify (Div (simplify u) (simplify v))
  | otherwise = Div (simplify u) (simplify v)
simplify (Pow u v)
  | isZero (simplify u) == isZero (simplify v) = Pow (simplify u) (simplify v)
  | isZero (simplify u) = Val "0"
  | isZero (simplify v) || isOne (simplify u) = Val "1"
  | isOne (simplify v) = simplify u
  | simplify u /= u || simplify v /= v = simplify (Pow (simplify u) (simplify v))
  | otherwise = Pow (simplify u) (simplify v)
--simplify (Exp u)   =
--simplify (Sin u)   =
--simplify (Cos u)   =
--simplify (Tg u)    =
--simplify (Ln u)    =
simplify x = x

prettify :: String -> String
prettify ('(':x:')':xs) = x : prettify xs
prettify (x:xs) = x : prettify xs
prettify [] = []