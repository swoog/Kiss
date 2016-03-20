module TypeChecker
open AbstractSyntax

exception ParsingError of string

type EquationType =
| Type of string * (string * EquationType) list
| TypeInt
| TypeFloat
| TypeVoid

let newName = "exp"

exception TypeError of string

let typeToString t = 
    match t with
    | TypeInt -> "int"
    | TypeFloat -> "float"
    | TypeVoid -> "()"
    | Type(name, _) -> name

let rec getType typeAccu name = 
    match typeAccu with
    | [] -> raise(ParsingError("Error to search type of variable" + name))
    | (variableName, valueType)::l -> if variableName = name then valueType else (getType l name)

let rec checkTypeProperties properties =
    match properties with
    | [] -> []
    | PropertySetter(name, e)::l -> (name, (checkTypeExpression e))::(checkTypeProperties l)

and checkTypeExpression (a:Expression) = 
    match a with
    | Int(_) -> TypeInt
    | Float(_) -> TypeFloat
    | New(properties) -> Type(newName, (checkTypeProperties properties))
    | Add(ex1, ex2) -> let type1 = (checkTypeExpression ex1)
                       let type2 = (checkTypeExpression ex2)
                       in if type1 = type2 then 
                           type1 
                          else 
                           raise(TypeError("Expression at the left is " + (typeToString type1) + " and the right is " + (typeToString type2)))

let rec getPropertyTypeFromProperties properties propertyName =
    match properties with
    | [] -> raise(TypeError("property " + propertyName + " not found"))
    | (name, t)::l -> if name = propertyName then t else getPropertyTypeFromProperties l propertyName

let getPropertyType objectType propertyName = 
    match objectType with
    | Type(name, properties) -> getPropertyTypeFromProperties properties propertyName
    | _ -> raise(TypeError((typeToString objectType) + " has no property " + propertyName))

let rec checkTypeVariable (a:Variable) typeAccu propertyTypeAccu = 
    match a with
    | Variable(name) -> (getType typeAccu name)
    | Property(variable, propertyName) -> let objectType = checkTypeVariable variable typeAccu propertyTypeAccu
                                          in getPropertyType objectType propertyName

let checkTypeStatement a typeAccu propertyTypeAccu = 
    match a with 
    | Create(name, e) -> (name, (checkTypeExpression e))::typeAccu
    | Assign(variable, e) -> let expressionType = (checkTypeExpression e)
                             let variableType = (checkTypeVariable variable typeAccu propertyTypeAccu)
                             in if expressionType = variableType then
                                    typeAccu
                                else
                                    raise(TypeError("Variable is not of type " + (typeToString expressionType)))
    | Return(_) -> raise(ParsingError("check statement incorrect"))

let rec checkTypeStatements (a:Statement list) typeAccu propertyTypeAccu = 
    match a with
    | [] -> TypeVoid
    | (Return(a))::l -> checkTypeExpression a
    | a::l -> let typeAccu = checkTypeStatement a typeAccu propertyTypeAccu
                in checkTypeStatements l typeAccu propertyTypeAccu

let checkTypeProg (a:Prog) = 
    match a with
    | Program(x) -> let progType = checkTypeStatements x [] []
                    in progType = TypeVoid
