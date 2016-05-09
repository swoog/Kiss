module BuildIl

open System
open System.Reflection
open System.Reflection.Emit
open TypedAbstractSyntax
open ToIl

type IlAssemblyBuilder = 
    | IlAssemblyBuilder of AssemblyBuilder * string * IlTypeBuilder list
and IlTypeBuilder =
    | IlClassBuilder of TypeBuilder * string * IlMemberBuilder list
and IlMemberBuilder =
    | IlMethodBuilder of IlParameter list * IlVariable list * MethodBuilder * string * IlInstruction list
    | IlEntryBuilder of IlParameter list * IlVariable list * MethodBuilder * string * IlInstruction list
    | IlFieldBuilder of FieldBuilder * string * TypeName

let rec findTypeConstructor classes name = 
    match classes with
    | [] -> raise(Exception("Not found"))
    | IlClassBuilder(builder, n, _)::l -> if name = n then builder else (findTypeConstructor l name)

let rec findField classes className name = 
    match classes with
    | [] -> raise(Exception("Not found"))
    | IlClassBuilder(_, n, members)::l -> if className  = n then (findFieldMembers members name) else (findField l className name)
and findFieldMembers members name = 
    match members with
    | [] -> raise(Exception("Not found"))
    | IlFieldBuilder(builder, n, _)::l -> if name = n then builder else (findFieldMembers l name)
    | m::l -> (findFieldMembers l name)

let toType t classes = 
    match t with
    | TypeInt -> typedefof<int>
    | TypeBool -> typedefof<bool>
    | Type(name, _) -> ((findTypeConstructor classes name):TypeBuilder).AsType()
    | _ -> typedefof<int>

let rec defineBuilderAssembly a = 
    match a with
    | Assembly(name, c) -> 
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name, name, true)
        in IlAssemblyBuilder(assemblyBuilder, name, defineBuilderTypes c moduleBuilder)
and defineBuilderTypes c moduleBuilder= 
    match c with
    | [] -> []
    | c::l -> (defineBuilderType c moduleBuilder)::(defineBuilderTypes l moduleBuilder)
and defineBuilderType c moduleBuilder= 
    match c with
    | Class(name, methods) -> let typeBuilder = moduleBuilder.DefineType("Test." + name)
                              in IlClassBuilder(typeBuilder, name, defineBuilderMembers typeBuilder methods)
and defineBuilderMembers typeBuilder methods =
    match methods with
    | [] -> []
    | c::l -> (defineBuilderMember typeBuilder c)::(defineBuilderMembers typeBuilder l)
and defineBuilderMember typeBuilder memb = 
    match memb with
    | Method(parameters, variables,name, instructions) -> 
        let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot )
        in IlMethodBuilder(parameters, variables, methodBuilder, name, instructions)
    | EntryPoint(parameters, variables,name, instructions) -> 
        let methodBuilder = typeBuilder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot )
        in IlEntryBuilder(parameters, variables, methodBuilder, name, instructions)
    | Field(name, t) -> 
        let fieldBuilder = typeBuilder.DefineField(name, (toType t []), FieldAttributes.Public)
        IlFieldBuilder(fieldBuilder, name, t)

let rec buildIl assemblyOutput a = 
    let a = defineBuilderAssembly a
    (buildAssembly a:AssemblyBuilder).Save(assemblyOutput, PortableExecutableKinds.Preferred32Bit, ImageFileMachine.I386)
and buildAssembly a = 
    match a with
    | IlAssemblyBuilder(assemblyBuilder, name, c) -> buildClasses c assemblyBuilder c;assemblyBuilder;
and buildClasses c assemblyBuilder classes= 
    match c with
    | [] -> ()
    | c::l -> (buildClass c assemblyBuilder classes); (buildClasses l assemblyBuilder classes)
and buildClass c assemblyBuilder classes= 
    match c with 
    | IlClassBuilder(typeBuilder, name, methods) -> buildMethods methods typeBuilder assemblyBuilder classes methods; typeBuilder.CreateType() |> ignore
and buildMethods m typeBuilder assemblyBuilder classes members = 
    match m with
    | [] -> ()
    | m::methods -> buildMethod m typeBuilder assemblyBuilder classes members; buildMethods methods typeBuilder assemblyBuilder classes members
and buildMethod m typeBuilder assemblyBuilder classes members = 
    match m with
    | IlMethodBuilder(parameters, variables, methodBuilder, name, emits) -> buildEmitsIl emits (methodBuilder.GetILGenerator()) [] classes members
    | IlEntryBuilder(parameters, variables, methodBuilder, name, emits) -> 
        let ilGenerator = methodBuilder.GetILGenerator()
        let locals = List.map (fun (_, t, index) -> (ilGenerator.DeclareLocal(toType t classes), t, index)) variables 
        in buildEmitsIl emits ilGenerator locals classes members;assemblyBuilder.SetEntryPoint(methodBuilder)
    | IlFieldBuilder(_, _, _) -> ()
and buildEmitsIl e ilGenerator locals classes members =
    match e with
    | [] -> ()
    | i::l -> buildEmitIl i ilGenerator locals classes members; buildEmitsIl l ilGenerator locals classes members
and buildEmitIl e ilGenerator locals classes members =
    match e with
    | Nop -> ilGenerator.Emit(OpCodes.Nop)
    | Ldstr(s) -> ilGenerator.Emit(OpCodes.Ldstr, s)
    | Ldc_I4(i) -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
    | Ldc_R4(i) -> ilGenerator.Emit(OpCodes.Ldc_R4, i)
    | Stloc(index) -> ilGenerator.Emit(OpCodes.Stloc, ((findLocal locals index):LocalBuilder))
    | Ldloc(index) -> ilGenerator.Emit(OpCodes.Ldloc, ((findLocal locals index):LocalBuilder))
    | Stfld(className, name) -> ilGenerator.Emit(OpCodes.Stfld, ((findField classes className name):FieldBuilder))
    | Newobj(name) -> ilGenerator.Emit(OpCodes.Newobj, ((findTypeConstructor classes name).GetConstructor([||])))
    | Add -> ilGenerator.Emit(OpCodes.Add)
    | Ret -> ilGenerator.Emit(OpCodes.Ret)
