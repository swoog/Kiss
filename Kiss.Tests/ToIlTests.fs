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

