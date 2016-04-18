module ToIlTests

    open Xunit 
    open Program
    open TypedAbstractSyntax
    open ToIl

    let expected expectedIl prog= 
        let resultats = toIl "First.exe" prog
        Assert.Equal(expectedIl, resultats)

    [<Fact>] 
    let ``Should create main method When empty program``() = 
        TypedProgram([])
         |> expected (Assembly("First.exe", [Class("Program", [EntryPoint([], [], "main", [Ret])])]))


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
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedAdd(TypedInt(1), TypedInt(2)))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1); ("", TypeInt, 2)], "main", [Ldc_I4(1); Stloc(1); Ldc_I4(2); Stloc(2); Ldloc(1); Ldloc(2); Add ; Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should assign value``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedInt(1)); TypedAssign(TypeInt, TypedVariable("myVariable"), TypedInt(2))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1)], "main", [Ldc_I4(1); Stloc(0); Ldc_I4(2); Stloc(1); Ldloc(1); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should assign value when add two int``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedAdd(TypedInt(1), TypedInt(2))); TypedAssign(TypeInt, TypedVariable("myVariable"), TypedInt(4))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", TypeInt, 0); ("", TypeInt, 1); ("", TypeInt, 2); ("", TypeInt, 3)], "main", [Ldc_I4(1); Stloc(1); Ldc_I4(2); Stloc(2); Ldloc(1); Ldloc(2); Add ; Stloc(0); Ldc_I4(4); Stloc(3); Ldloc(3); Stloc(0); Ret])])]))

    [<Fact>] 
    let ``Should create variable When new type``() = 
        TypedProgram([TypedCreate(Type("obj-1", []), "v", TypedNew(Type("obj-1", []), []))])
         |> expected 
         (Assembly("First.exe", [Class("obj-1", []); Class("Program", [EntryPoint([], [("v", Type("obj-1", []), 0);], "main", [Newobj("obj-1"); Stloc(0); Ret])])]))

