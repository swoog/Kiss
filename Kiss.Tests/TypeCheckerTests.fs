module TypeCheckerTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypedAbstractSyntax
    open TypeChecker

    let statement a = Program(a)

    let expectedTypeError errorMessage lines =
        try
            checkTypeProg (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    let expectedCorrect expectedLines lines= 
        let resultats = checkTypeProg (statement lines)
        Assert.Equal(TypedProgram(expectedLines), resultats)

    [<Fact>] 
    let ``Should type is correct when check create variable``() = 
        [
            Create("variableName", Int(1))
        ] |> expectedCorrect [
            TypedCreate(TypeInt, "variableName", TypedInt(1))
        ]

    [<Fact>] 
    let ``Should type is incorrect when check create variable and assign``() = 
        [
            Create("variableName", Int(1));
            Assign(Variable("variableName"), Float(1.0))
        ] |> expectedTypeError "Variable is not of type float"

    [<Fact>] 
    let ``Should type is correct when check create variable with object``() = 
        [
            Create("variableName", New([]))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1",[]), "variableName", TypedNew([]))
        ]
    [<Fact>] 
    let ``Should type is correct when check assign variable with object``() = 
        [
            Create("variableName", New([]));
            Assign(Variable("variableName"), New([]))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1", []), "variableName", TypedNew([]));
            TypedAssign(TypedVariable("variableName"), TypedNew([]))
        ]

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property``() = 
        [
            Create("variableName", New([PropertySetter("Prop1", Int(1))]));
            Assign(Property(Variable("variableName"), "Prop1"), Int(2))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1", [("Prop1", TypeInt)]), "variableName", TypedNew([TypedPropertySetter("Prop1", TypedInt(1))]));
            TypedAssign(TypedProperty(TypedVariable("variableName"), "Prop1"), TypedInt(2))
        ]

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property float``() = 
        [
            Create("variableName", New([PropertySetter("Prop1", Float(1.0))]));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1", [("Prop1", TypeFloat)]), "variableName", TypedNew([TypedPropertySetter("Prop1", TypedFloat(1.0))]));
            TypedAssign(TypedProperty(TypedVariable("variableName"), "Prop1"), TypedFloat(2.0))
        ]

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
        [
            Create("variableName", Add(Int(1), Int(2)));
        ] |> expectedCorrect [
            TypedCreate(TypeInt, "variableName", TypedAdd(TypedInt(1), TypedInt(2)));
        ]

    [<Fact>] 
    let ``Should type is incorrect when check add int and float``() = 
        [
            Create("variableName", Add(Int(1), Float(2.0)));
        ] |> expectedTypeError "Expression at the left is int and the right is float"

    [<Fact>] 
    let ``Should type is correct when check func``() = 
        [
            Create("variableName", Fun([], []));
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([], TypeVoid), "variableName", TypedFun([], []));
        ]

    [<Fact>] 
    let ``Should type is correct when check func return int``() = 
        [
            Create("variableName", Fun([], [Return(Int(1))]));
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([], TypeInt), "variableName", TypedFun([], [TypedReturn(TypedInt(1))]));
        ]

    [<Fact>] 
    let ``Should type is correct when check func and assign new func``() = 
        [
            Create("variableName", Fun([], [Return(Int(1))]));
            Assign(Variable("variableName"), Fun([], [Return(Int(2))]))
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([], TypeInt), "variableName", TypedFun([], [TypedReturn(TypedInt(1))]));
            TypedAssign(TypedVariable("variableName"), TypedFun([], [TypedReturn(TypedInt(2))]))
        ]

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
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([], TypeVoid), "variableName", TypedFun([], []));
        ]
