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
         |> expected (Assembly("First.exe", [Class("Program", [EntryPoint("main", [])])]))


//    [<Fact>] 
//    let ``Should create main method When assign int``() = 
//        TypedProgram([TypedAssign(TypedVariable("myVariable"), TypedInt(1))])
//         |> expected (Assembly("First.exe", [Class("Program", [EntryPoint("main", [Ldc_I4_s(1); Stloc_0; Ldloc_0; ])])]))

