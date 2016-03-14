module AbstractSyntax

type Prog = Program of Statement list

and Statement = 
| Create of string * Expression
| Assign of string * Expression
| Return of Expression

and Expression =
| Int of int
| Add of Expression * Expression
| New
| Use of string
| Call of string
| Fun of string list * Statement list
| Variable of string
