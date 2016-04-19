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
    | Stfld  of string * string
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
    | TypedAdd(_, e1, e2) -> List.append (getTypeObjectExpression e1) (getTypeObjectExpression e2)

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
        let var = (List.length variables)
        let variable = (name, t, var)
        let variables = List.append variables [variable]
        let (variables, instrExpression) = toIlExpression e variables var
        let (variables, instructions) = toIlStatements l ((List.length variables) + 1) variables
        in (variables, List.append instrExpression instructions)
    | TypedAssign(t, name, e)::l -> 
        let var = (List.length variables)
        let variable = ("", t, var)
        let variables = List.append variables [variable]
        let (variables, instrExpression) = toIlExpression e variables var
        let (variables, instructions) = toIlStatements l ((List.length variables) + 1) variables
        let (variables, getVariableInstr, variableToSet) = toIlVariable name variables
        let ins = List.append getVariableInstr instrExpression
        let ins = List.append ins (Ldloc(var)::variableToSet)
        in (variables, List.append ins instructions)
    | TypedReturn(e)::l -> let (variables, instructions) = toIlStatements l (i + 1) variables
                           let (variables, instrExpression) = toIlExpression e variables i
                           in (variables, List.append instrExpression instructions)
and toIlVariable t variables = 
    match t with
    | TypedVariable(name) -> (variables, [], [Stloc(findIndex variables name)])
    | TypedProperty(variable, propertyName) -> (variables, [], [])

and toIlExpression e variables returnVar = 
    match e with
    | TypedInt(i) -> 
        (variables, [Ldc_I4(i); Stloc(returnVar)])
    | TypedBool(b) -> 
        let v = if b then 1 else 0 
        in (variables, [Ldc_I4(v); Stloc(returnVar)])
    | TypedNew(Type(t, p), props) ->
        let variablesIns = [Newobj(t); Stloc(returnVar)]
        let (variables, props) = (toIlPropertySet props returnVar returnVar variables t)
        in (variables, List.append variablesIns props)
    | TypedAdd(t, e1, e2) -> 
        let var1 = returnVar + 1
        let var2 = returnVar + 2
        let variables = List.append variables [("", t, var1);("", t, var2)]
        let (variables, instr1) = (toIlExpression e1 variables var1)
        let (variables, instr2) = (toIlExpression e2 variables var2)
        let instr = List.append instr1 instr2
        in (variables, (List.append instr [Ldloc(var1); Ldloc(var2); Add ; Stloc(returnVar)]))
    | _ -> ([], [])
and toIlPropertySet props countVariables varObj variables className =
    match props with
    | [] -> (variables, [])
    | TypedPropertySetter(t, name, e)::l -> 
        let var = (List.length variables)
        let variables = List.append variables [("", t, var)]
        let (variables, exp) = toIlExpression e variables var
        in (variables, List.append exp [Ldloc(varObj); Ldloc(var); Stfld(className, name) ])
