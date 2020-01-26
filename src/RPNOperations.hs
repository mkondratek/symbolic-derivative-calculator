module RPNOperations (
  toRPN,
  rpnToExpression
 ) where

import Expression

opPriority :: Char -> Int 
opPriority c 
 | (=='(') c = 0
 | (`elem` "+-)") c = 1
 | (`elem` "*/") c = 2
 | (=='^') c = 3

  
parseNumPrfx :: String -> String
parseNumPrfx = takeWhile (`elem` "0123456789.")

ignoreNumPrfx :: String -> String
ignoreNumPrfx = dropWhile (`elem` "0123456789.")

ignoreFunPrfx :: String -> String
ignoreFunPrfx ('e':'x':'p':xs) = xs
ignoreFunPrfx ('s':'i':'n':xs) = xs
ignoreFunPrfx ('c':'o':'s':xs) = xs
ignoreFunPrfx ('t':'g':xs) = xs
ignoreFunPrfx ('l':'n':xs) = xs

funNames :: String
funNames = "esctl"

-- convert infix to postfix (expr, stack, result que)
toRPN :: String -> String -> [String]
toRPN "" st = map (: []) st
toRPN ('(':expr) st = toRPN expr ('(' : st)
toRPN (')':expr) st = do
  let (b, e) = break (== '(') st
  if not (null (tail e)) && head (tail e) `elem` funNames
    then map (: []) (b ++ [head (tail e)]) ++ toRPN expr (tail (tail e))
    else map (: []) b ++ toRPN expr (tail e)
toRPN expr st
  | head expr `elem` "0123456789" = parseNumPrfx expr : toRPN (ignoreNumPrfx expr) st
  | head expr `elem` "xyz" = [head expr] : toRPN (tail expr) st
  | head expr `elem` "esctl" = toRPN (ignoreFunPrfx expr) (head expr : st)
  | head expr `elem` "+-/*^" =
    if opPriority (head expr) > opPriority (head st)
      then toRPN (tail expr) (head expr : st)
      else [head st] : toRPN expr (tail st)
  | otherwise = toRPN (tail expr) (head expr : st)

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

