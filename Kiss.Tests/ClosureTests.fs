module ClosureTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypeChecker
    open Closure

    let statement a = Program(a)

    let expectedTypeError errorMessage lines =
        try
            closureProgram (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    let expectedCorrect expectedLines lines= 
        let resultats = closureProgram (statement lines)
        Assert.Equal(Program(expectedLines), resultats)

    [<Fact>] 
    let ``Should return same variable when make closure of one variable``() = 
        [
            Create("variableName", Int(1))
        ] |> expectedCorrect [
            Create("variableName", Int(1))
        ]

    [<Fact>] 
    let ``Should return new name when have two create variable with same name and get variable``() = 
        [
            Create("v", Int(1));
            Create("v", Get(Variable("v")))
        ] |> expectedCorrect [
            Create("v", Int(1));
            Create("v-1", Get(Variable("v")))
        ]


    [<Fact>] 
    let ``Should return new name when have two create variable with same name``() = 
        [
            Create("variableName", Int(1));
            Create("variableName", Int(2))
        ] |> expectedCorrect [
            Create("variableName", Int(1));
            Create("variableName-1", Int(2))
        ]

    [<Fact>] 
    let ``Should return new name when have three create variable with same name``() = 
        [
            Create("variableName", Int(1));
            Create("variableName", Int(2));
            Create("variableName", Int(2))
        ] |> expectedCorrect [
            Create("variableName", Int(1));
            Create("variableName-1", Int(2));
            Create("variableName-1-1", Int(2))
        ]


    [<Fact>] 
    let ``Should return new name when have two create variable with same name and asign``() = 
        [
            Create("variableName", Int(1));
            Create("variableName", Int(2));
            Assign(Variable("variableName"), Int(3))
        ] |> expectedCorrect [
            Create("variableName", Int(1));
            Create("variableName-1", Int(2));
            Assign(Variable("variableName-1"), Int(3))
        ]

    [<Fact>] 
    let ``Should return new name when have three create variable with same name and asign``() = 
        [
            Create("variableName", Int(1));
            Create("variableName", Int(2));
            Assign(Variable("variableName"), Int(3));
            Create("variableName", Int(2));
            Assign(Variable("variableName"), Int(3))
        ] |> expectedCorrect [
            Create("variableName", Int(1));
            Create("variableName-1", Int(2));
            Assign(Variable("variableName-1"), Int(3));
            Create("variableName-1-1", Int(2));
            Assign(Variable("variableName-1-1"), Int(3))
        ]

    [<Fact>] 
    let ``Should return new name when have two create variable with same name in func``() = 
        [
            Create("v", Int(1));
            Create("v", Fun([], [Assign(Variable("v"), Int(2)); Return(Get(Variable("v")))]));
            Assign(Variable("v"), Fun([], []))
        ] |> expectedCorrect [
            Create("v", Int(1));
            Create("v-1", Fun([], [Assign(Variable("v"), Int(2)); Return(Get(Variable("v")))]));
            Assign(Variable("v-1"), Fun([], []))
        ]

    [<Fact>] 
    let ``Should return new name when have three create variable with same name in func``() = 
        [
            Create("v", Int(1));
            Create("v", Fun([], [Create("v", Get(Variable("v")))]));
            Assign(Variable("v"), Fun([], []))
        ] |> expectedCorrect [
            Create("v", Int(1));
            Create("v-1", Fun([], [Create("v-1", Get(Variable("v")))]));
            Assign(Variable("v-1"), Fun([], []))
        ]

    [<Fact>] 
    let ``Should return new name when have three create variable with same name in func and return``() = 
        [
            Create("v", Int(1));
            Create("v", Fun([], [Assign(Variable("v"), Int(2)); Create("v", Get(Variable("v"))); Return(Get(Variable("v")))]));
            Assign(Variable("v"), Fun([], []))
        ] |> expectedCorrect [
            Create("v", Int(1));
            Create("v-1", Fun([], [Assign(Variable("v"), Int(2)); Create("v-1", Get(Variable("v"))); Return(Get(Variable("v-1")))]));
            Assign(Variable("v-1"), Fun([], []))
        ]