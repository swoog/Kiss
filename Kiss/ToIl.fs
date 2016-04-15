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
    and IlVariable = string * int
    and IlMethod = 
        | Method of IlParameter list * IlVariable list * string * IlInstruction list
        | EntryPoint of IlParameter list * IlVariable list * string * IlInstruction list
    and IlInstruction = 
        | Ldc_I4 of int
        | Stloc of int
        | Ldloc of int
        | Add
        | Ret
        | Nop


    let rec findIndex variables name = 
        match variables with
        | [] -> raise(Exception("Error"))
        | (n, i)::l -> if name = n then i else (findIndex l name)

    let rec findLocal locals index = 
        match locals with
        | [] -> raise(Exception("Error"))
        | (local, i)::l -> if index = i then local else (findLocal l index)


    let rec toIl assemblyName p =
        match p with 
        | TypedProgram(s) -> let (variables, instructions) = toIlStatements s 0 []
                             let instructions = List.append instructions [Ret]
                             in Assembly(assemblyName, [Class("Program", [EntryPoint([], variables, "main", instructions)])])
    and toIlStatements s i variables=
        match s with
        | [] -> (variables, [])
        | TypedCreate(t, name, e)::l -> 
            let variable = (name, i)
            let (i2, instrExpression) = toIlExpression e i
            let v = List.init (i2 - i) (fun i -> ("", i + 1))
            let (variables, instructions) = toIlStatements l (i2 + 1) (variable::(List.append v variables))
            in (variables, List.append instrExpression instructions)
        | TypedAssign(t, e)::l -> 
            let (i2, instrExpression) = toIlExpression e i
            let (variables, instructions) = toIlStatements l (i2 + 1) variables
            let (variables, getVariableInstr, variableToSet) = toIlVariable t variables
            let variables = List.append variables [("", i)]
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
        | TypedAdd(e1, e2) -> 
            let var1 = countVariables + 1
            let (var, instr1) = (toIlExpression e1 var1)
            let var2 = var + 1
            let (var, instr2) = (toIlExpression e2 var2)
            let instr = List.append instr1 instr2
            in (var, (List.append instr [Ldloc(var1); Ldloc(var2); Add ; Stloc(countVariables)]))
        | _ -> (0, [])

    let toType t = typedefof<int>

    let rec buildIl assemblyOutput a = 
        (buildAssembly a:AssemblyBuilder).Save(assemblyOutput, PortableExecutableKinds.Preferred32Bit, ImageFileMachine.I386)
    and buildAssembly a = 
        match a with
        | Assembly(name, c) -> 
            let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
            let moduleBuilder = assemblyBuilder.DefineDynamicModule(name, name, true)
            in buildClasses c moduleBuilder assemblyBuilder;assemblyBuilder;
    and buildClasses c moduleBuilder assemblyBuilder= 
        match c with
        | [] -> ()
        | c::classes -> (buildClass c moduleBuilder assemblyBuilder); (buildClasses classes moduleBuilder assemblyBuilder)
    and buildClass c moduleBuilder assemblyBuilder= 
        match c with 
        | Class(name, methods) -> let typeBuilder = moduleBuilder.DefineType("Test." + name)
                                  in buildMethods methods typeBuilder assemblyBuilder; typeBuilder.CreateType() |> ignore
    and buildMethods m typeBuilder assemblyBuilder = 
        match m with
        | [] -> ()
        | m::methods -> buildMethod m typeBuilder assemblyBuilder; buildMethods methods typeBuilder assemblyBuilder
    and buildMethod m typeBuilder assemblyBuilder = 
        match m with
        | Method(parameters, variables, name, emits) -> 
            let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot )
            in buildEmitsIl emits (methodBuilder.GetILGenerator()) []
        | EntryPoint(parameters, variables, name, emits) -> 
            let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
            let ilGenerator = methodBuilder.GetILGenerator()
            let locals = List.map (fun (t, index) -> (ilGenerator.DeclareLocal(toType t), index)) variables 
            in buildEmitsIl emits ilGenerator locals;assemblyBuilder.SetEntryPoint(methodBuilder)
    and buildEmitsIl e ilGenerator locals =
        match e with
        | [] -> ()
        | i::l -> buildEmitIl i ilGenerator locals; buildEmitsIl l ilGenerator locals
    and buildEmitIl e ilGenerator locals =
        match e with
        | Nop -> ilGenerator.Emit(OpCodes.Nop)
        | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | Stloc(index) -> ilGenerator.Emit(OpCodes.Stloc, ((findLocal locals index):LocalBuilder))
        | Ldloc(index) -> ilGenerator.Emit(OpCodes.Ldloc, ((findLocal locals index):LocalBuilder))
        | Add -> ilGenerator.Emit(OpCodes.Add)
        | Ret -> ilGenerator.Emit(OpCodes.Ret)