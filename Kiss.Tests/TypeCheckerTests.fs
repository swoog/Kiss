module TypeCheckerTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypeChecker

    let statement a = Program(a)

    let expectedTypeError errorMessage lines =
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    let expectedCorrect lines = 
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)


    [<Fact>] 
    let ``Should type is correct when check create variable``() = 
        let line = Create("variableName", Int(1))
        let isCheck = checkTypeProg (statement [line])
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is incorrect when check create variable and assign``() = 
        [
            Create("variableName", Int(1));
            Assign(Variable("variableName"), Float(1.0))
        ] |> expectedTypeError "Variable is not of type float"

    [<Fact>] 
    let ``Should type is correct when check create variable with object``() = 
        let line = Create("variableName", New([]))
        let isCheck = checkTypeProg (statement [line])
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is correct when check assign variable with object``() = 
        [
            Create("variableName", New([]));
            Assign(Variable("variableName"), New([]))
        ] |> expectedCorrect

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property``() = 
        let lines = [
            Create("variableName", New([PropertySetter("Prop1", Int(1))]));
            Assign(Property(Variable("variableName"), "Prop1"), Int(2))
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property float``() = 
        let lines = [
            Create("variableName", New([PropertySetter("Prop1", Float(1.0))]));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is incorrect when check assign variable with object initialized with property``() = 
        [
            Create("variableName", New([PropertySetter("Prop1", Int(1))]));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ] |> expectedTypeError "Variable is not of type float"

    [<Fact>] 
    let ``Should type is incorrect when check assign value to property of int``() = 
        [
            Create("variableName", Int(1));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ] |> expectedTypeError "int has no property Prop1"

    [<Fact>] 
    let ``Should type is correct when check add with int``() = 
        let lines = [
            Create("variableName", Add(Int(1), Int(2)));
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is incorrect when check add int and float``() = 
        [
            Create("variableName", Add(Int(1), Float(2.0)));
        ] |> expectedTypeError "Expression at the left is int and the right is float"

    [<Fact>] 
    let ``Should type is correct when check func``() = 
        let lines = [
            Create("variableName", Fun([], []));
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is correct when check func return int``() = 
        let lines = [
            Create("variableName", Fun([], [Return(Int(1))]));
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is correct when check func and assign new func``() = 
        [
            Create("variableName", Fun([], [Return(Int(1))]));
            Assign(Variable("variableName"), Fun([], [Return(Int(2))]))
        ] |> expectedCorrect

    [<Fact>] 
    let ``Should type is incorrect when check func and assign new func of float``() = 
        [
            Create("variableName", Fun([], [Return(Int(1))]));
            Assign(Variable("variableName"), Fun([], [Return(Float(2.0))]))
        ] |> expectedTypeError "Variable is not of type func() : float"

    [<Fact(Skip="WIP")>] 
    let ``Should type is correct when check func add``() = 
        [
            Create("variableName", Fun(["x"; "y"], [Return(Add(Get(Variable("x")), Get(Variable("y"))))]));
            Assign(Variable("variableName"), Fun([], [Return(Int(2))]))
        ] |> expectedCorrect
