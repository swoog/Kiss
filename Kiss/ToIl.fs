module ToIl

    open System
    open System.Reflection
    open System.Reflection.Emit

    type IlAssembly =
        Assembly of string * IlType list
    and IlType =
        Class of string * IlMethod list
    and IlMethod = 
        | Method of string * IlInstruction list
        | EntryPoint of string * IlInstruction list
    and IlInstruction = 
        Nop

    let toIl assemblyName p = Assembly(assemblyName, [Class("Program", [EntryPoint("main", [])])])

    let rec buildIl assemblyOutput a = 
        (buildAssembly a:AssemblyBuilder).Save(assemblyOutput, PortableExecutableKinds.Preferred32Bit, ImageFileMachine.I386)
    and buildAssembly a = 
        match a with
        | Assembly(name, c) -> let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
                               let moduleBuilder = assemblyBuilder.DefineDynamicModule(name, name, true)
                               in buildClasses c moduleBuilder;assemblyBuilder;
    and buildClasses c moduleBuilder = 
        match c with
        | [] -> ()
        | c::classes -> (buildClass c moduleBuilder); (buildClasses classes moduleBuilder)
    and buildClass c moduleBuilder = 
        match c with 
        | Class(name, methods) -> let typeBuilder = moduleBuilder.DefineType("Test." + name)
                                  in buildMethods methods typeBuilder; typeBuilder.CreateType() |> ignore
    and buildMethods m typeBuilder = 
        match m with
        | [] -> ()
        | m::methods -> buildMethod m typeBuilder; buildMethods methods typeBuilder
    and buildMethod m typeBuilder = 
        match m with
        | Method(name, emits) -> let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot )
                                 in buildEmitsIl emits (methodBuilder.GetILGenerator())
        | EntryPoint(name, emits) -> let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
                                     in buildEmitsIl emits (methodBuilder.GetILGenerator())
    and buildEmitsIl e ilGenerator =
        match e with
        | [] -> ()
        | i::l -> buildEmitIl i ilGenerator; buildEmitsIl e ilGenerator
    and buildEmitIl e ilGenerator =
        match e with
        | Nop -> ilGenerator.Emit(OpCodes.Nop)