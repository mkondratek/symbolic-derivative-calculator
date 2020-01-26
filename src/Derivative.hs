module Derivative (
  der
  ) where

import Expression
    
-- calculate derivative for given variable
der :: Expression -> Char -> Expression
der (Val u) _   = Val "0"
der (Var u) x   = if x == u then Val "1" else Val "0"
der (Neg u) x   = Mul (Val "-1") (der u x)
der (Add u v) x = Add (der u x) (der v x)
der (Sub u v) x = Sub (der u x) (der v x)
der (Mul u v) x = Add (Mul u (der v x)) (Mul (der u x) v)
der (Div u v) x = Div (Sub (Mul (der u x) v) (Mul u (der v x))) (Mul v v)
der (Pow u v) x = Mul (Pow u (Sub v (Val "1"))) (Add (Mul u (Mul (der v x) (Ln u))) (Mul v (der u x)))
der (Exp u) x   = Mul (der u x) (Exp u)
der (Sin u) x   = Mul (der u x) (Cos u)
der (Cos u) x   = Neg (Mul (der u x) (Sin u))
der (Tg u) x    = der (Div (Sin u) (Cos u)) x
der (Ln u) x    = Mul (der u x) (Div (Val "1") u)
