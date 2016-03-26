module TypeChecker
open AbstractSyntax
open TypedAbstractSyntax

let mutable counter = 0

let newName = 
    counter <- counter + 1
    "obj-" + counter.ToString()

exception TypeError of string

let rec typeToString t = 
    match t with
    | TypeInt -> "int"
    | TypeFloat -> "float"
    | TypeVoid -> "()"
    | Type(name, _) -> name
    | TypeFunc(parameters, returnType) -> "func(" + (parameters |> List.map typeToString |> String.concat ",") + ") : " + (typeToString returnType)
    | TypeGeneric(t) -> t

let rec getType typeAccu name = 
    match typeAccu with
    | [] -> raise(TypeError("Error to search type of variable " + name))
    | (variableName, valueType)::l -> if variableName = name then valueType else (getType l name)

let rec getPropertyTypeFromProperties properties propertyName =
    match properties with
    | [] -> raise(TypeError("property " + propertyName + " not found"))
    | (name, t)::l -> if name = propertyName then t else getPropertyTypeFromProperties l propertyName

let getPropertyType objectType propertyName = 
    match objectType with
    | Type(name, properties) -> getPropertyTypeFromProperties properties propertyName
    | _ -> raise(TypeError((typeToString objectType) + " has no property " + propertyName))

let rec replaceType typeAccu tgName t2 =
    match typeAccu with
    | [] -> []
    | (name, TypeGeneric(t))::l -> (if t = tgName then (name, t2) else (name, TypeGeneric(t)))::(replaceType l tgName t2)
    | a::l -> a::(replaceType l tgName t2)

let compareType typeAccu type1 type2 = 
    let (typeAccu, type1, t1) = type1 
    let (typeAccu, type2, t2) = type2 
    match (type1, type2) with
    | (TypeGeneric(tg1), TypeGeneric(tg2)) -> (replaceType typeAccu tg2 (TypeGeneric(tg1)), TypeGeneric(tg1), TypedAdd(t1, t2))
    | (TypeGeneric(tg1), type2) -> (replaceType typeAccu tg1 type2, type2, TypedAdd(t1, t2))
    | (type1, TypeGeneric(tg2)) -> (replaceType typeAccu tg2 type1, type1, TypedAdd(t1, t2))
    | (type1, type2) -> if type1 = type2 then
                          (typeAccu, type1, TypedAdd(t1, t2))
                        else
                          raise(TypeError("Expression at the left is " + (typeToString type1) + " and the right is " + (typeToString type2)))

let addGenericParameters typeAccu parameters =
    let rec addGenericParameters typeAccu parameters count =
        match parameters with
        | [] -> typeAccu
        | a::l -> (a, TypeGeneric("T" + count.ToString()))::(addGenericParameters typeAccu l (count + 1))
    in addGenericParameters typeAccu parameters 1

let rec checkTypeProperties properties typeAccu=
    match properties with
    | [] -> ([], [])
    | PropertySetter(name, e)::l -> let (typeAccu, typeExpression, typedExpression) = (checkTypeExpression e typeAccu)
                                    let (typeProperties, typedProperties) = (checkTypeProperties l typeAccu)
                                    in ((name, typeExpression)::typeProperties, TypedPropertySetter(name, typedExpression)::typedProperties)

and checkTypeExpression (a:Expression) typeAccu = 
    match a with
    | Int(i) -> (typeAccu, TypeInt, TypedInt(i))
    | Float(f) -> (typeAccu, TypeFloat, TypedFloat(f))
    | New(properties) -> let (typeProperties, typedProperties) = (checkTypeProperties properties typeAccu)
                         in (typeAccu, Type(newName, typeProperties), TypedNew(typedProperties))
    | Add(ex1, ex2) -> compareType 
                            typeAccu
                            (checkTypeExpression ex1 typeAccu)
                            (checkTypeExpression ex2 typeAccu)
    | Fun(parameters, statements) -> let typeAccuWithParameters = addGenericParameters typeAccu parameters
                                     let (subTypeAccu, ty, statements) = (checkTypeStatements statements typeAccuWithParameters)
                                     let paramatersTypes = List.map (getType subTypeAccu) parameters
                                     in (typeAccu, TypeFunc(paramatersTypes, ty), TypedFun(parameters, statements))
    | Get(variable) -> let (variableType, variable) = checkTypeVariable variable typeAccu
                       in (typeAccu, variableType, TypedGet(variable))

and checkTypeVariable (a:Variable) typeAccu = 
    match a with
    | Variable(name) -> ((getType typeAccu name), TypedVariable(name))
    | Property(variable, propertyName) -> let (objectType, variable) = checkTypeVariable variable typeAccu 
                                          in (getPropertyType objectType propertyName, TypedProperty(variable, propertyName))

and checkTypeStatement a typeAccu = 
    match a with 
    | Create(name, e) -> let (typeAccu, typeExpression, t) = (checkTypeExpression e typeAccu)
                         let typeAccu = (name, typeExpression)::typeAccu
                         in (typeAccu, TypedCreate(typeExpression, name, t))
    | Assign(variable, e) -> let (typeAccu, typeExpression, t) = (checkTypeExpression e typeAccu) 
                             let (variableType, variable) = (checkTypeVariable variable typeAccu)
                             in if typeExpression = variableType then
                                   (typeAccu, TypedAssign(variable, t))
                                else
                                   raise(TypeError("Variable is not of type " + (typeToString typeExpression)))
    | Return(_) -> raise(TypeError("check statement incorrect"))

and checkTypeStatements (a:Statement list) typeAccu = 
    match a with
    | [] -> (typeAccu, TypeVoid, [])
    | (Return(a))::l -> let (typeAccu, typeExpression, t) = checkTypeExpression a typeAccu
                        in (typeAccu, typeExpression, [TypedReturn(t)])
    | a::l -> let (typeAccu, statement) = checkTypeStatement a typeAccu
              let (typeAccu, t, s) = checkTypeStatements l typeAccu
              in (typeAccu, t, statement::s)

and checkTypeProg (a:Prog) = 
    match a with
    | Program(x) -> let s = checkTypeStatements x []
                    in match s with 
                       | (_, TypeVoid, s) -> TypedProgram(s)
                       | _ -> raise(TypeError("Program must be of type void"))
