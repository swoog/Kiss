module ToIlTests

    open Xunit 
    open Program
    open TypedAbstractSyntax
    open ToIl

    let expected expectedIl prog= 
        let resultats = toIl prog
        Assert.Equal(expectedIl, resultats)

    [<Fact>] 
    let ``Should type is correct when check create variable``() = 
        TypedProgram([
        ]) |> expected [
            TypedCreate(TypeInt, "variableName", TypedInt(1))
        ]
