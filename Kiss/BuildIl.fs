module BuildIl

open System
open System.Reflection
open System.Reflection.Emit
open TypedAbstractSyntax
open ToIl

let rec findTypeConstructor classes name = 
    match classes with
    | [] -> raise(Exception("Not found"))
    | (n, builder)::l -> if name = n then builder else (findTypeConstructor l name)

let toType t classes = 
    match t with
    | TypeInt -> typedefof<int>
    | TypeBool -> typedefof<bool>
    | Type(name, _) -> (findTypeConstructor classes name).GetType()
    | _ -> typedefof<int>

let rec buildIl assemblyOutput a = 
    (buildAssembly a:AssemblyBuilder).Save(assemblyOutput, PortableExecutableKinds.Preferred32Bit, ImageFileMachine.I386)
and buildAssembly a = 
    match a with
    | Assembly(name, c) -> 
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name, name, true)
        let classes = List.map (fun c -> match c with Class(name, _) -> (name, moduleBuilder.DefineType("Test." + name))) c
        in buildClasses c moduleBuilder assemblyBuilder classes;assemblyBuilder;
and buildClasses c moduleBuilder assemblyBuilder classes= 
    match c with
    | [] -> ()
    | c::l -> (buildClass c moduleBuilder assemblyBuilder classes); (buildClasses l moduleBuilder assemblyBuilder classes)
and buildClass c moduleBuilder assemblyBuilder classes= 
    match c with 
    | Class(name, methods) -> let typeBuilder = (findTypeConstructor classes name)
                              in buildMethods methods typeBuilder assemblyBuilder classes; typeBuilder.CreateType() |> ignore
and buildMethods m typeBuilder assemblyBuilder classes = 
    match m with
    | [] -> ()
    | m::methods -> buildMethod m typeBuilder assemblyBuilder classes; buildMethods methods typeBuilder assemblyBuilder classes
and buildMethod m typeBuilder assemblyBuilder classes = 
    match m with
    | Method(parameters, variables, name, emits) -> 
        let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot )
        in buildEmitsIl emits (methodBuilder.GetILGenerator()) [] classes
    | EntryPoint(parameters, variables, name, emits) -> 
        let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
        let ilGenerator = methodBuilder.GetILGenerator()
        let locals = List.map (fun (_, t, index) -> (ilGenerator.DeclareLocal(toType t classes), t, index)) variables 
        in buildEmitsIl emits ilGenerator locals classes;assemblyBuilder.SetEntryPoint(methodBuilder)
and buildEmitsIl e ilGenerator locals classes =
    match e with
    | [] -> ()
    | i::l -> buildEmitIl i ilGenerator locals classes; buildEmitsIl l ilGenerator locals classes
and buildEmitIl e ilGenerator locals classes =
    match e with
    | Nop -> ilGenerator.Emit(OpCodes.Nop)
    | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
    | Stloc(index) -> ilGenerator.Emit(OpCodes.Stloc, ((findLocal locals index):LocalBuilder))
    | Ldloc(index) -> ilGenerator.Emit(OpCodes.Ldloc, ((findLocal locals index):LocalBuilder))
    | Newobj(name) -> ilGenerator.Emit(OpCodes.Newobj, ((findTypeConstructor classes name).GetConstructor([||])))
    | Add -> ilGenerator.Emit(OpCodes.Add)
    | Ret -> ilGenerator.Emit(OpCodes.Ret)
