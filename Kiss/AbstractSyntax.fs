module AbstractSyntax

type Prog = Program of Statement list

and Statement = 
| Create of string * Expression
| Assign of Variable * Expression
| Return of Expression

and Expression =
| Int of int
| Add of Expression * Expression
| New of Property list
| Use of string
| Call of string
| Fun of string list * Statement list
| Get of Variable

and Property = 
| PropertySetter of string * Expression

and Variable = 
| Variable of string
| Property of Variable * string