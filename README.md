# symbolic-derivative-calculator

Symbolic Differentiation in Haskell

## Execuction

Calculation of partial derivatives (for x, y and z):
```
derivative <expr-string>
```
 - Example: `derivative "sin(1/x)*exp(y^3-2)"`

Making expressions more "human-readable":
```
simplify <Expression>
```
 - Example: `map simplify (derivative "sin(1/x)*exp(y^3-2)")`

