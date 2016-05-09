module ToIlTests

    open Xunit 
    open Program
    open TypedAbstractSyntax
    open ToIl

    let expected expectedIl prog= 
        let resultats = toIl "First.exe" prog
        Assert.Equal(expectedIl, resultats)

    let expectedError errorMessage prog =
        try
            toIl "First.exe" prog |> ignore
            raise(System.Exception("Expected IlError received no exception"))
        with
        | IlError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected IlError received " + e.ToString()))

    [<Fact>] 
    let ``Should create main method When empty program``() = 
        TypedProgram([])
         |> expected (Assembly("First.exe", [Class("Program", [EntryPoint([], [], "main", [Ret])])]))

    [<Fact>] 
    let ``Should create main method When create string variable``() = 
        TypedProgram([TypedCreate(TypeString, "myVariable", TypedString("Test"))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeString, 0);], "main", [Ldstr("Test"); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create main method When create int variable``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedInt(1))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0);], "main", [Ldc_I4(1); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create main method When create bool variable with true``() = 
        TypedProgram([TypedCreate(TypeBool, "v", TypedBool(true))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("v", TypeBool, 0);], "main", [Ldc_I4(1); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create main method When create bool variable with false``() = 
        TypedProgram([TypedCreate(TypeBool, "v", TypedBool(false))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("v", TypeBool, 0);], "main", [Ldc_I4(0); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create add instruction When add int``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedAdd(TypeInt, TypedInt(1), TypedInt(2)))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1); ("", TypeInt, 2)], "main", [Ldc_I4(1); Stloc(1); Ldc_I4(2); Stloc(2); Ldloc(1); Ldloc(2); Add ; Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should assign value``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedInt(1)); TypedAssign(TypeInt, TypedVariable("myVariable"), TypedInt(2))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1)], "main", [Ldc_I4(1); Stloc(0); Ldc_I4(2); Stloc(1); Ldloc(1); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should assign value when add two int``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedAdd(TypeInt, TypedInt(1), TypedInt(2))); TypedAssign(TypeInt, TypedVariable("myVariable"), TypedInt(4))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1); ("", TypeInt, 2); ("", TypeInt, 3)], "main", [Ldc_I4(1); Stloc(1); Ldc_I4(2); Stloc(2); Ldloc(1); Ldloc(2); Add ; Stloc(0); Ldc_I4(4); Stloc(3); Ldloc(3); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create variable When new type``() = 
        TypedProgram([TypedCreate(Type("obj-1", []), "v", TypedNew(Type("obj-1", []), []))])
         |> expected 
         (Assembly("First.exe", [Class("obj-1", []); Class("Program", [EntryPoint([], [("v", Type("obj-1", []), 0);], "main", [Newobj("obj-1"); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should raise error When new type of bool``() = 
        TypedProgram([TypedCreate(Type("obj-1", []), "v", TypedNew(TypeBool, []))])
         |> expectedError "Error to generate IL for type bool"

    [<Fact>] 
    let ``Should create variable When new type and property``() = 
        TypedProgram([TypedCreate(TypeInt, "v", TypedInt(1)); TypedAssign(TypeInt, TypedVariable("v"), TypedInt(2)); TypedCreate(Type("obj-1", [("prop", TypeInt)]), "v2", TypedNew(Type("obj-1", [("prop", TypeInt)]), [TypedPropertySetter(TypeInt, "prop",TypedInt(3))]))])
         |> expected
         (Assembly("First.exe", [Class("obj-1", [Field("prop", TypeInt)]); Class("Program", [EntryPoint([], [("v", TypeInt, 0);("", TypeInt, 1);("v2", Type("obj-1", [("prop", TypeInt)]), 2);("", TypeInt, 3);], "main", [Ldc_I4(1); Stloc(0); Ldc_I4(2); Stloc(1); Ldloc(1); Stloc(0); Newobj("obj-1") ; Stloc(2); Ldc_I4(3); Stloc(3); Ldloc(2); Ldloc(3); Stfld("obj-1", "prop"); Ret])])]))

    [<Fact>] 
    let ``Should create variable When new type and assign two property``() = 
        TypedProgram([TypedCreate(Type("obj-1", [("prop", TypeInt);("prop2", TypeFloat)]), "v", TypedNew(Type("obj-1", [("prop", TypeInt);("prop2", TypeFloat)]), [TypedPropertySetter(TypeInt, "prop",TypedInt(1));TypedPropertySetter(TypeFloat, "prop2",TypedFloat(1.0))]))])
         |> expected 
         (Assembly("First.exe", [Class("obj-1", [Field("prop", TypeInt);Field("prop2", TypeFloat)]); Class("Program", [EntryPoint([], [("v", Type("obj-1", [("prop", TypeInt);("prop2", TypeFloat)]), 0);("", TypeInt, 1);("", TypeFloat, 2);], "main", [Newobj("obj-1") ; Stloc(0); Ldc_I4(1); Stloc(1); Ldloc(0); Ldloc(1); Stfld("obj-1", "prop"); Ldc_R4(1.0); Stloc(2); Ldloc(0); Ldloc(2); Stfld("obj-1", "prop2"); Ret])])]))

