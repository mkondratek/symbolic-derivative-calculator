module Main where

import Expression
import Derivative
import ExpressionUtils

main :: IO ()
main = print "Witaj Świecie"

derivative :: String -> [Expression]
derivative s = map (der (read s :: Expression)) "xyz"
