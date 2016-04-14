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
         |> expected (Assembly("First.exe", [Class("Program", [EntryPoint([], [], "main", [])])]))


    [<Fact>] 
    let ``Should create main method When create int variable``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedInt(1))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", 0);], "main", [Ldc_I4(1); Stloc(0)])])]))

    [<Fact>] 
    let ``Should create add instruction When add int``() = 
        TypedProgram([TypedCreate(TypeInt, "myVariable", TypedAdd(TypedInt(1), TypedInt(2)))])
         |> expected 
         (Assembly("First.exe", [Class("Program", [EntryPoint([], [("myVariable", 0); ("", 1); ("", 2)], "main", [Ldc_I4(1); Stloc(1); Ldc_I4(2); Stloc(2); Ldloc(1); Ldloc(2); Add ; Stloc(0)])])]))

