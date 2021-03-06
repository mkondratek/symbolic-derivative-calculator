module RPNOperations
  ( toRPN
  ) where

opPriority :: Char -> Int
opPriority c
  | (== '(') c = 0
  | (`elem` "+-)") c = 1
  | (`elem` "*/") c = 2
  | (== '^') c = 3

parseNumPrfx :: String -> String
parseNumPrfx = takeWhile (`elem` "0123456789.")

ignoreNumPrfx :: String -> String
ignoreNumPrfx = dropWhile (`elem` "0123456789.")

ignoreFunPrfx :: String -> String
ignoreFunPrfx ('e':'x':'p':xs) = xs
ignoreFunPrfx ('s':'i':'n':xs) = xs
ignoreFunPrfx ('c':'o':'s':xs) = xs
ignoreFunPrfx ('t':'g':xs)     = xs
ignoreFunPrfx ('l':'n':xs)     = xs

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
