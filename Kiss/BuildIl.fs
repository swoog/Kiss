module BuildIl

open System
open System.Reflection
open System.Reflection.Emit
open TypedAbstractSyntax
open ToIl


let toType t = 
    match t with
    | TypeInt -> typedefof<int>
    | TypeBool -> typedefof<bool>
    | _ -> typedefof<int>

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
        let locals = List.map (fun (_, t, index) -> (ilGenerator.DeclareLocal(toType t), t, index)) variables 
        in buildEmitsIl emits ilGenerator locals;assemblyBuilder.SetEntryPoint(methodBuilder)
and buildEmitsIl e ilGenerator locals =
    match e with
    | [] -> ()
    | i::l -> buildEmitIl i ilGenerator locals []; buildEmitsIl l ilGenerator locals
and buildEmitIl e ilGenerator locals classes =
    match e with
    | Nop -> ilGenerator.Emit(OpCodes.Nop)
    | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
    | Stloc(index) -> ilGenerator.Emit(OpCodes.Stloc, ((findLocal locals index):LocalBuilder))
    | Ldloc(index) -> ilGenerator.Emit(OpCodes.Ldloc, ((findLocal locals index):LocalBuilder))
    | Newobj(name) -> ilGenerator.Emit(OpCodes.Newobj, ((findTypeConstructor classes name):TypeBuilder))
    | Add -> ilGenerator.Emit(OpCodes.Add)
    | Ret -> ilGenerator.Emit(OpCodes.Ret)
and findTypeConstructor classes name = null