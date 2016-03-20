module TypeCheckerTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypeChecker

    let statement a = Program(a)

    [<Fact>] 
    let ``Should type is correct when check create variable``() = 
        let line = Create("variableName", Int(1))
        let isCheck = checkTypeProg (statement [line])
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is incorrect when check create variable and assign``() = 
        let lines = [
            Create("variableName", Int(1));
            Assign(Variable("variableName"), Float(1.0))
        ]
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal("Variable is not of type float", x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    [<Fact>] 
    let ``Should type is correct when check create variable with object``() = 
        let line = Create("variableName", New([]))
        let isCheck = checkTypeProg (statement [line])
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is correct when check assign variable with object``() = 
        let lines = [
            Create("variableName", New([]));
            Assign(Variable("variableName"), New([]))
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

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
        let lines = [
            Create("variableName", New([PropertySetter("Prop1", Int(1))]));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ]
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal("Variable is not of type float", x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    [<Fact>] 
    let ``Should type is incorrect when check assign value to property of int``() = 
        let lines = [
            Create("variableName", Int(1));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ]
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal("int has no property Prop1", x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))


    [<Fact>] 
    let ``Should type is correct when check add with int``() = 
        let lines = [
            Create("variableName", Add(Int(1), Int(2)));
        ]
        let isCheck = checkTypeProg (statement lines)
        Assert.True(isCheck)

    [<Fact>] 
    let ``Should type is incorrect when check add int and float``() = 
        let lines = [
            Create("variableName", Add(Int(1), Float(2.0)));
        ]
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal("Expression at the left is int and the right is float", x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))
