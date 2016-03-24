﻿module TypeChecker
open AbstractSyntax
open TypedAbstractSyntax

exception ParsingError of string

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

let rec getType typeAccu name = 
    match typeAccu with
    | [] -> raise(ParsingError("Error to search type of variable" + name))
    | (variableName, valueType)::l -> if variableName = name then valueType else (getType l name)

let rec getPropertyTypeFromProperties properties propertyName =
    match properties with
    | [] -> raise(TypeError("property " + propertyName + " not found"))
    | (name, t)::l -> if name = propertyName then t else getPropertyTypeFromProperties l propertyName

let getPropertyType objectType propertyName = 
    match objectType with
    | Type(name, properties) -> getPropertyTypeFromProperties properties propertyName
    | _ -> raise(TypeError((typeToString objectType) + " has no property " + propertyName))

let compareType type1 type2 = 
    let (type1, t1) = type1 
    let (type2, t2) = type2 
    in if type1 = type2 then
          (type1, TypedAdd(t1, t2))
       else
          raise(TypeError("Expression at the left is " + (typeToString type1) + " and the right is " + (typeToString type2)))

let rec checkTypeProperties properties typeAccu=
    match properties with
    | [] -> ([], [])
    | PropertySetter(name, e)::l -> let (typeExpression, typedExpression) = (checkTypeExpression e typeAccu)
                                    let (typeProperties, typedProperties) = (checkTypeProperties l typeAccu)
                                    in ((name, typeExpression)::typeProperties, TypedPropertySetter(name, typedExpression)::typedProperties)

and checkTypeExpression (a:Expression) typeAccu = 
    match a with
    | Int(i) -> (TypeInt, TypedInt(i))
    | Float(f) -> (TypeFloat, TypedFloat(f))
    | New(properties) -> let (typeProperties, typedProperties) = (checkTypeProperties properties typeAccu)
                         in (Type(newName, typeProperties), TypedNew(typedProperties))
    | Add(ex1, ex2) -> compareType 
                            (checkTypeExpression ex1 typeAccu)
                            (checkTypeExpression ex2 typeAccu)
    | Fun(parameters, statements) -> let returnStatementType = (checkTypeStatements statements [] [])
                                     let (ty, statements) = returnStatementType 
                                     in (TypeFunc([], ty), TypedFun([], statements))
    | Get(variable) -> let (variableType, variable) = checkTypeVariable variable typeAccu []
                       in (variableType, TypedGet(TypedVariable("")))

and checkTypeVariable (a:Variable) typeAccu propertyTypeAccu = 
    match a with
    | Variable(name) -> ((getType typeAccu name), TypedVariable(name))
    | Property(variable, propertyName) -> let (objectType, variable) = checkTypeVariable variable typeAccu propertyTypeAccu
                                          in (getPropertyType objectType propertyName, TypedProperty(variable, propertyName))

and checkTypeStatement a typeAccu propertyTypeAccu = 
    match a with 
    | Create(name, e) -> let (typeExpression, t) = (checkTypeExpression e typeAccu)
                         let typeAccu = (name, typeExpression)::typeAccu
                         in (typeAccu, TypedCreate(typeExpression, name, t))
    | Assign(variable, e) -> let (typeExpression, t) = (checkTypeExpression e typeAccu) 
                             let (variableType, variable) = (checkTypeVariable variable typeAccu propertyTypeAccu)
                             in if typeExpression = variableType then
                                   (typeAccu, TypedAssign(variable, t))
                                else
                                   raise(TypeError("Variable is not of type " + (typeToString typeExpression)))
    | Return(_) -> raise(ParsingError("check statement incorrect"))

and checkTypeStatements (a:Statement list) typeAccu propertyTypeAccu = 
    match a with
    | [] -> (TypeVoid, [])
    | (Return(a))::l -> let (typeExpression, t) = checkTypeExpression a typeAccu
                        in (typeExpression, [TypedReturn(t)])
    | a::l -> let (typeAccu, statement) = checkTypeStatement a typeAccu propertyTypeAccu
              let (t, s) = checkTypeStatements l typeAccu propertyTypeAccu
              in (t, statement::s)

and checkTypeProg (a:Prog) = 
    match a with
    | Program(x) -> let s = checkTypeStatements x [] []
                    in match s with 
                       | (TypeVoid, s) -> TypedProgram(s)
                       | _ -> raise(TypeError("Program must be of type void"))