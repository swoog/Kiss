module ToIl

open System
open System.Reflection
open System.Reflection.Emit
open TypedAbstractSyntax

type IlAssembly =
    Assembly of string * IlType list
and IlType =
    Class of string * IlElement list
and IlParameter = string * int
and IlVariable = string * TypeName * int
and IlElement = 
    | Method of IlParameter list * IlVariable list * string * IlInstruction list
    | EntryPoint of IlParameter list * IlVariable list * string * IlInstruction list
    | Field of string * TypeName
and IlInstruction = 
    | Ldc_I4 of int
    | Stloc of int
    | Ldloc of int
    | Stfld  of string
    | Newobj of string
    | Add
    | Ret
    | Nop


let rec findIndex variables name = 
    match variables with
    | [] -> raise(Exception("Error"))
    | (n, t, i)::l -> if name = n then i else (findIndex l name)

let rec findLocal locals index = 
    match locals with
    | [] -> raise(Exception("Error"))
    | (local, t, i)::l -> if index = i then local else (findLocal l index)

let rec getTypeObject s =
    match s with
    | [] -> []
    | s::l -> List.append (getTypeObjectStatement s) (getTypeObject l)
and getTypeObjectType t = 
    match t with
    | Type(name, p) -> [Class(name, List.map (fun (name, t) -> Field(name, t)) p)]
    | _ -> []
and getTypeObjectStatement s =
    match s with 
    | TypedCreate(t, _, e) -> List.append (getTypeObjectType t) (getTypeObjectExpression e)
    | TypedAssign(t, _, e) -> List.append (getTypeObjectType t) (getTypeObjectExpression e)
    | TypedReturn(e) -> getTypeObjectExpression e
and getTypeObjectProp p = []
and getTypeObjectExpression e = 
    match e with
    | TypedNew(t, props) -> List.append  (getTypeObjectType t) (getTypeObjectProp props)
    | TypedUse(_) -> []
    | TypedInt(_) -> []
    | TypedFloat(_) -> []
    | TypedBool(_) -> []
    | TypedCall(_) -> []
    | TypedGet(_) -> []
    | TypedFun(_, s) -> getTypeObject s
    | TypedGreater(e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)
    | TypedGreaterOrEqual(e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)
    | TypedLess(e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)
    | TypedLessOrEqual(e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)
    | TypedAdd(e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)

let rec toIl assemblyName p =
    match p with 
    | TypedProgram(s) -> let (variables, instructions) = toIlStatements s 0 []
                         let instructions = List.append instructions [Ret]
                         let typeObject = List.distinct (getTypeObject s)
                         let entryPoint = [Class("Program", [EntryPoint([], variables, "main", instructions)])]
                         in Assembly(assemblyName, List.append typeObject entryPoint)
and toIlStatements s i variables=
    match s with
    | [] -> (variables, [])
    | TypedCreate(t, name, e)::l -> 
        let variable = (name, t, i)
        let (i2, instrExpression) = toIlExpression e variables
        let v = List.init (i2 - i) (fun i -> ("", t, i + 1))
        let (variables, instructions) = toIlStatements l (i2 + 1) (variable::(List.append v variables))
        in (variables, List.append instrExpression instructions)
    | TypedAssign(t, name, e)::l -> 
        let (i2, instrExpression) = toIlExpression e variables
        let (variables, instructions) = toIlStatements l (i2 + 1) variables
        let (variables, getVariableInstr, variableToSet) = toIlVariable name variables
        let variables = List.append variables [("", t, i)]
        let i3 = List.append getVariableInstr instrExpression
        let i = List.append i3 (Ldloc(i)::variableToSet)
        in (variables, List.append i instructions)
    | TypedReturn(e)::l -> let (variables, instructions) = toIlStatements l (i + 1) variables
                           let (i, instrExpression) = toIlExpression e variables
                           in (variables, List.append instrExpression instructions)
and toIlVariable t variables = 
    match t with
    | TypedVariable(name) -> (variables, [], [Stloc(findIndex variables name)])
    | TypedProperty(variable, propertyName) -> (variables, [], [])

and toIlExpression e variables = 
    let returnVar = (List.length variables)
    in match e with
        | TypedInt(i) -> 
            (returnVar, [Ldc_I4(i); Stloc(returnVar)])
        | TypedBool(b) -> 
            let v = if b then 1 else 0 
            in (returnVar, [Ldc_I4(v); Stloc(returnVar)])
        | TypedNew(Type(t, p), props) ->
            let variablesIns = [Newobj(t); Stloc(returnVar)]
            let (countVariables, props) = (toIlPropertySet props returnVar returnVar variables)
            in (countVariables, List.append variablesIns props)
        | TypedAdd(e1, e2) -> 
            let var1 = returnVar + 1
            let variables = (("", TypeInt, var1)::variables)
            let (var, instr1) = (toIlExpression e1 variables)
            let var2 = var + 1
            let variables = (("", TypeInt, var2)::variables)
            let (var, instr2) = (toIlExpression e2 variables)
            let instr = List.append instr1 instr2
            in (var, (List.append instr [Ldloc(var1); Ldloc(var2); Add ; Stloc(returnVar)]))
        | _ -> (0, [])
and toIlPropertySet props countVariables varObj variables =
    match props with
    | [] -> (countVariables, [])
    | TypedPropertySetter(name, e)::l -> 
        let (c, exp) = toIlExpression e variables
        in (c, List.append exp [Ldloc(varObj); Ldloc((countVariables + 1)); Stfld(name) ])
