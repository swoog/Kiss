module TypeChecker
open AbstractSyntax
open TypedAbstractSyntax

let mutable counter = 0
let mutable counterGeneric = 0
let mutable counterInterface = 0

let newName = fun() ->
    counter <- counter + 1
    "obj-" + counter.ToString()

let mutable newGenericName = fun() ->
    counterGeneric <- counterGeneric + 1
    "T" + counterGeneric.ToString()

let newInterfaceName = fun() ->
    counterInterface <- counterInterface + 1
    "I" + counterInterface.ToString()

exception TypeError of string

let rec typeToString t = 
    match t with
    | TypeInt -> "int"
    | TypeFloat -> "float"
    | TypeBool -> "bool"
    | TypeVoid -> "()"
    | Type(name, _) -> name
    | TypeInterface(name, _) -> name
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
    | TypeGeneric(name) -> let t = TypeGeneric(newGenericName())
                            in (name, TypeInterface(newInterfaceName(), [(propertyName, t)]), t)
    | Type(name, properties) -> ("", objectType, getPropertyTypeFromProperties properties propertyName)
    | _ -> raise(TypeError((typeToString objectType) + " has no property " + propertyName))

let rec replaceType3 tgName t2 (name, t) = (name, replaceType2 tgName t2 t)
and replaceType2 tgName t2 t=
    match t with
    | TypeGeneric(t) -> if t = tgName then t2 else TypeGeneric(t)
    | TypeFunc(p, r) -> TypeFunc(List.map (replaceType2 tgName t2) p, replaceType2 tgName t2 r)
    | Type(name, p) -> Type(name, List.map (replaceType3 tgName t2) p)
    | TypeInterface(name, p) -> TypeInterface(name, List.map (replaceType3 tgName t2) p)
    | TypeBool -> TypeBool
    | TypeInt -> TypeInt
    | TypeFloat -> TypeFloat
    | TypeVoid -> TypeVoid
and replaceType typeAccu tgName t2 =
    match typeAccu with
    | [] -> []
    | (name, t)::l -> (name, replaceType2 tgName t2 t)::(replaceType l tgName t2)

let compareType typeAccu type1 t1 type2 t2= 
    match (type1, type2) with
    | (TypeGeneric(tg1), TypeGeneric(tg2)) -> (replaceType typeAccu tg2 (TypeGeneric(tg1)), TypeGeneric(tg1), TypedAdd(t1, t2))
    | (TypeGeneric(tg1), type2) -> (replaceType typeAccu tg1 type2, type2, TypedAdd(t1, t2))
    | (type1, TypeGeneric(tg2)) -> (replaceType typeAccu tg2 type1, type1, TypedAdd(t1, t2))
    | (type1, type2) -> if type1 = type2 then
                          (typeAccu, type1, TypedAdd(t1, t2))
                        else
                          raise(TypeError("Expression at the left is " + (typeToString type1) + " and the right is " + (typeToString type2)))

let compareTypeCondition typeAccu type1 type2 R= 
    let (typeAccu, type1, t1) = type1 
    let (typeAccu, type2, t2) = type2 
    match (type1, type2) with
    | (TypeGeneric(tg1), TypeGeneric(tg2)) -> (replaceType typeAccu tg2 (TypeGeneric(tg1)), TypeBool, R(t1, t2))
    | (TypeGeneric(tg1), type2) -> (replaceType typeAccu tg1 type2, TypeBool, R(t1, t2))
    | (type1, TypeGeneric(tg2)) -> (replaceType typeAccu tg2 type1, TypeBool, R(t1, t2))
    | (type1, type2) -> if type1 = type2 then
                          (typeAccu, TypeBool, R(t1, t2))
                        else
                          raise(TypeError("Expression at the left is " + (typeToString type1) + " and the right is " + (typeToString type2)))

let rec addGenericParameters typeAccu parameters =
    match parameters with
    | [] -> typeAccu
    | a::l -> (a, TypeGeneric(newGenericName()))::(addGenericParameters typeAccu l)

let checkTypeUse name = 
    match name with
    | "Console" -> Type("Console", [
                                     ("Write", TypeFunc([TypeGeneric("T1")], TypeVoid))
                                   ]);
    | "Random" -> Type("Random", [
                                 ]);
    | t -> raise(TypeError("Use of " + t + " is not found"))

let rec checkTypeProperties properties typeAccu=
    match properties with
    | [] -> ([], [])
    | PropertySetter(name, e)::l -> let (typeAccu, typeExpression, typedExpression) = (checkTypeExpression e typeAccu)
                                    let (typeProperties, typedProperties) = (checkTypeProperties l typeAccu)
                                    in ((name, typeExpression)::typeProperties, TypedPropertySetter(name, typedExpression)::typedProperties)

and checkTypeExpression (a:Expression) typeAccu = 
    match a with
    | Int(i) -> (typeAccu, TypeInt, TypedInt(i))
    | Bool(i) -> (typeAccu, TypeBool, TypedBool(i))
    | Float(f) -> (typeAccu, TypeFloat, TypedFloat(f))
    | New(properties) -> let (typeProperties, typedProperties) = (checkTypeProperties properties typeAccu)
                         let t = Type(newName(), typeProperties)
                         in (typeAccu, t, TypedNew(t, typedProperties))
    | Add(ex1, ex2) ->  let (typeAccu, type1, t1) = (checkTypeExpression ex1 typeAccu)
                        let (typeAccu, type2, t2) = (checkTypeExpression ex2 typeAccu)
                        in compareType typeAccu type1 t1 type2 t2
    | Fun(parameters, statements) -> let typeAccuWithParameters = addGenericParameters typeAccu parameters
                                     let (subTypeAccu, ty, statements) = (checkTypeStatements statements typeAccuWithParameters)
                                     let paramatersTypes = List.map (getType subTypeAccu) parameters
                                     in (typeAccu, TypeFunc(paramatersTypes, ty), TypedFun(parameters, statements))
    | Get(variable) -> let (typeAccu, variableType, variable) = checkTypeVariable variable typeAccu
                       in (typeAccu, variableType, TypedGet(variable))
    | Call(variable) -> let (typeAccu, variableType, variable) = checkTypeVariable variable typeAccu
                        match variableType with
                        | TypeFunc(_, returnType) -> (typeAccu, returnType, TypedCall(variable))
                        | _ -> raise(TypeError("Variable must be of type Fun()"))
    | Use(name) -> (typeAccu, checkTypeUse name, TypedUse(name))
    | Greater(ex1, ex2) -> compareTypeCondition
                            typeAccu
                            (checkTypeExpression ex1 typeAccu)
                            (checkTypeExpression ex2 typeAccu)
                            TypedGreater
    | GreaterOrEqual(ex1, ex2) -> compareTypeCondition
                                    typeAccu
                                    (checkTypeExpression ex1 typeAccu)
                                    (checkTypeExpression ex2 typeAccu)
                                    TypedGreaterOrEqual
    | Less(ex1, ex2) -> compareTypeCondition
                            typeAccu
                            (checkTypeExpression ex1 typeAccu)
                            (checkTypeExpression ex2 typeAccu)
                            TypedLess
    | LessOrEqual(ex1, ex2) -> compareTypeCondition
                                typeAccu
                                (checkTypeExpression ex1 typeAccu)
                                (checkTypeExpression ex2 typeAccu)
                                TypedLessOrEqual

and checkTypeVariable (a:Variable) typeAccu = 
    match a with
    | Variable(name) -> (typeAccu, (getType typeAccu name), TypedVariable(name))
    | Property(variable, propertyName) -> let (typeAccu, objectType, variable) = checkTypeVariable variable typeAccu 
                                          let (genericTypeName, newObjectType, propertyType) = getPropertyType objectType propertyName
                                          in (replaceType typeAccu genericTypeName newObjectType, propertyType, TypedProperty(variable, propertyName))

and checkTypeStatement a typeAccu = 
    match a with 
    | Create(name, e) -> let (typeAccu, typeExpression, t) = (checkTypeExpression e typeAccu)
                         let typeAccu = (name, typeExpression)::typeAccu
                         in (typeAccu, TypedCreate(typeExpression, name, t))
    | Assign(variable, e) -> let (typeAccu, typeExpression, t) = (checkTypeExpression e typeAccu) 
                             let (typeAccu, variableType, variable) = (checkTypeVariable variable typeAccu)
                             in if typeExpression = variableType then
                                   (typeAccu, TypedAssign(typeExpression, variable, t))
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
                       | (_, TypeInt, s) -> TypedProgram(s)
                       | _ -> raise(TypeError("Program must be of type int or void"))
