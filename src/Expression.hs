module Expression (
  Expression(..),
  parse,
  rpnToExpression
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

rpnToExpression :: [String] -> [Expression] -> Expression
rpnToExpression [] es = head es
rpnToExpression ("+":xs) (e:f:es) = rpnToExpression xs (Add f e : es)
rpnToExpression ("-":xs) (e:f:es) = rpnToExpression xs (Sub f e : es)
rpnToExpression ("/":xs) (e:f:es) = rpnToExpression xs (Div f e : es)
rpnToExpression ("*":xs) (e:f:es) = rpnToExpression xs (Mul f e : es)
rpnToExpression ("^":xs) (e:f:es) = rpnToExpression xs (Pow f e : es)
rpnToExpression ("e":xs) (e:es) = rpnToExpression xs (Exp e : es)
rpnToExpression ("s":xs) (e:es) = rpnToExpression xs (Sin e : es)
rpnToExpression ("c":xs) (e:es) = rpnToExpression xs (Cos e : es)
rpnToExpression ("t":xs) (e:es) = rpnToExpression xs (Tg e : es)
rpnToExpression ("l":xs) (e:es) = rpnToExpression xs (Ln e : es)
rpnToExpression (x:xs) es
  | head x `elem` "1234567890" = rpnToExpression xs (Val x : es)
  | head x `elem` "xyz" = rpnToExpression xs (Var (head x) : es)