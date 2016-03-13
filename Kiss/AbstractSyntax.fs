module AbstractSyntax

type Expression =
| Int of int
| Add of Expression * Expression
| New
| Use of string
| Fun of string list * Expression

type Statement = 
| Assign of string * Expression

type Prog = Program of Statement list