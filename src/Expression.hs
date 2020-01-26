module Expression (
  Expression(..),
  parse
  ) where

import RPNOperations

data Expression
  = Val String
  | Var Char
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Pow Expression Expression
  | Exp Expression
  | Sin Expression
  | Cos Expression
  | Tg Expression
  | Ln Expression
  deriving Eq
  
smartParentesizer :: Expression -> String
smartParentesizer (Val u) = u
smartParentesizer (Var u) = [u]
smartParentesizer x = "(" ++ show x ++ ")"

instance Show Expression where
  show (Val u) = u
  show (Var u) = [u]
  show (Neg u) = "(-" ++ show u ++ ")"
  show (Add u v) = smartParentesizer u ++ " + " ++ smartParentesizer v
  show (Sub u v) = show u ++ " - " ++ smartParentesizer v
  show (Mul u v) = smartParentesizer u ++ " * " ++ smartParentesizer v
  show (Div u v) = smartParentesizer u ++ " / " ++ smartParentesizer v
  show (Pow u v) = smartParentesizer u ++ " ^ " ++ smartParentesizer v
  show (Exp u) = "exp(" ++ show u ++ ")"
  show (Sin u) = "sin(" ++ show u ++ ")"
  show (Cos u) = "cos(" ++ show u ++ ")"
  show (Tg u)  = "tg(" ++ show u ++ ")"
  show (Ln u)  = "ln(" ++ show u ++ ")"
  

parse :: String -> Expression
parse s = rpnToExpression (toRPN (filter (/=' ') ("(" ++ s ++ ")")) []) []

instance Read Expression where
  readsPrec _ s = [(parse s, "")]
  