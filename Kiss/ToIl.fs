module ToIl

open System
open System.Reflection
open System.Reflection.Emit
open TypedAbstractSyntax

type IlAssembly =
    Assembly of string * IlType list
and IlType =
    Class of string * IlMethod list
and IlParameter = string * int
and IlVariable = string * TypeName * int
and IlMethod = 
    | Method of IlParameter list * IlVariable list * string * IlInstruction list
    | EntryPoint of IlParameter list * IlVariable list * string * IlInstruction list
and IlInstruction = 
    | Ldc_I4 of int
    | Stloc of int
    | Ldloc of int
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
    | Type(name, p) -> [Class(name, [])]
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
        let (i2, instrExpression) = toIlExpression e i
        let v = List.init (i2 - i) (fun i -> ("", t, i + 1))
        let (variables, instructions) = toIlStatements l (i2 + 1) (variable::(List.append v variables))
        in (variables, List.append instrExpression instructions)
    | TypedAssign(t, name, e)::l -> 
        let (i2, instrExpression) = toIlExpression e i
        let (variables, instructions) = toIlStatements l (i2 + 1) variables
        let (variables, getVariableInstr, variableToSet) = toIlVariable name variables
        let variables = List.append variables [("", t, i)]
        let i3 = List.append getVariableInstr instrExpression
        let i = List.append i3 (Ldloc(i)::variableToSet)
        in (variables, List.append i instructions)
    | TypedReturn(e)::l -> let (variables, instructions) = toIlStatements l (i + 1) variables
                           let (i, instrExpression) = toIlExpression e i
                           in (variables, List.append instrExpression instructions)
and toIlVariable t variables = 
    match t with
    | TypedVariable(name) -> (variables, [], [Stloc(findIndex variables name)])
    | TypedProperty(variable, propertyName) -> (variables, [], [])

and toIlExpression e countVariables = 
    match e with
    | TypedInt(i) -> (countVariables, [Ldc_I4(i); Stloc(countVariables)])
    | TypedBool(b) -> let v = if b then 1 else 0 in (countVariables, [Ldc_I4(v); Stloc(countVariables)])
    | TypedNew(Type(t, p), props) -> (countVariables, [Newobj(t); Stloc(countVariables)])
    | TypedAdd(e1, e2) -> 
        let var1 = countVariables + 1
        let (var, instr1) = (toIlExpression e1 var1)
        let var2 = var + 1
        let (var, instr2) = (toIlExpression e2 var2)
        let instr = List.append instr1 instr2
        in (var, (List.append instr [Ldloc(var1); Ldloc(var2); Add ; Stloc(countVariables)]))
    | _ -> (0, [])

