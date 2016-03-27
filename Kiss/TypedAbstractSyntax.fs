module TypedAbstractSyntax

type TypeName =
| Type of string * (string * TypeName) list
| TypeInt
| TypeFloat
| TypeBool
| TypeVoid
| TypeGeneric of string
| TypeFunc of TypeName List * TypeName

type TypedProg = TypedProgram of TypedStatement list

and TypedStatements = 
| TypedStatements of TypeName * TypedStatement list

and TypedStatement = 
| TypedCreate of TypeName * string * TypedExpression
| TypedAssign of TypedVariable * TypedExpression
| TypedReturn of TypedExpression

and TypedExpression =
| TypedInt of int
| TypedFloat of float
| TypedAdd of TypedExpression * TypedExpression
| TypedNew of TypedProperty list
| TypedUse of string
| TypedCall of TypedVariable
| TypedFun of string list * TypedStatement list
| TypedGet of TypedVariable
| TypedGreater of TypedExpression * TypedExpression
| TypedGreaterOrEqual of TypedExpression * TypedExpression
| TypedLess of TypedExpression * TypedExpression
| TypedLessOrEqual of TypedExpression * TypedExpression

and TypedProperty = 
| TypedPropertySetter of string * TypedExpression

and TypedVariable = 
| TypedVariable of string
| TypedProperty of TypedVariable * string
