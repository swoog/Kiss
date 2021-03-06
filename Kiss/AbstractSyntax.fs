﻿module AbstractSyntax

type Prog = Program of Statement list

and Statement = 
| Create of string * Expression
| Assign of Variable * Expression
| Return of Expression

and Expression =
| Int of int
| Bool of bool
| Float of float
| String of string
| Add of Expression * Expression
| New of Property list
| Use of string
| Call of Variable
| Fun of string list * Statement list
| Get of Variable
| Greater of Expression * Expression
| GreaterOrEqual of Expression * Expression
| Less of Expression * Expression
| LessOrEqual of Expression * Expression

and Property = 
| PropertySetter of string * Expression

and Variable = 
| Variable of string
| Property of Variable * string
