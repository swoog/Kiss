module TypeCheckerTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypedAbstractSyntax
    open TypeChecker

    let statement a = Program(a)

    let expectedTypeError errorMessage lines =
        try
            TypeChecker.counter <- 0;
            TypeChecker.counterGeneric <- 0;
            TypeChecker.counterInterface <- 0;
            checkType (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    let expectedCorrect expectedLines lines= 
        TypeChecker.counter <- 0;
        TypeChecker.counterGeneric <- 0;
        TypeChecker.counterInterface <- 0;
        let resultats = checkType (statement lines)
        Assert.Equal(TypedProgram(expectedLines), resultats)

    [<Fact>] 
    let ``Should type is correct when check create variable``() = 
        [
            Create("variableName", Int(1))
        ] |> expectedCorrect [
            TypedCreate(TypeInt, "variableName", TypedInt(1))
        ]

    [<Fact>] 
    let ``Should type is correct when check create variable with string type``() = 
        [
            Create("variableName", String("my value"))
        ] |> expectedCorrect [
            TypedCreate(TypeString, "variableName", TypedString("my value"))
        ]

    [<Fact>] 
    let ``Should type is incorrect when check create variable and assign``() = 
        [
            Create("variableName", Int(1));
            Assign(Variable("variableName"), Float(1.0))
        ] |> expectedTypeError "Variable is not of type float"

    [<Fact>] 
    let ``Should type is incorrect when check create variable of type int and assign string value``() = 
        [
            Create("variableName", Int(1));
            Assign(Variable("variableName"), String("Test"))
        ] |> expectedTypeError "Variable is not of type string"

    [<Fact>] 
    let ``Should type is correct when assign variable to create variable``() = 
        [
            Create("v1", Int(1));
            Create("v2", Get(Variable("v1")));
        ] |> expectedCorrect [
            TypedCreate(TypeInt, "v1", TypedInt(1));
            TypedCreate(TypeInt, "v2", TypedGet(TypedVariable("v1")));
        ]

    [<Fact>] 
    let ``Should type is correct when check create variable with object``() = 
        [
            Create("variableName", New([]))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1",[]), "variableName", TypedNew(Type("obj-1", []), []))
        ]
    [<Fact>] 
    let ``Should type is incorrect when check assign variable with object``() = 
        [
            Create("variableName", New([]));
            Assign(Variable("variableName"), New([]))
        ] |> expectedTypeError "Variable is not of type obj-2"

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property``() = 
        [
            Create("variableName", New([PropertySetter("Prop1", Int(1))]));
            Assign(Property(Variable("variableName"), "Prop1"), Int(2))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1", [("Prop1", TypeInt)]), "variableName", TypedNew(Type("obj-1", [("Prop1", TypeInt)]), [TypedPropertySetter(TypeInt, "Prop1", TypedInt(1))]));
            TypedAssign(TypeInt, TypedProperty(TypedVariable("variableName"), "Prop1"), TypedInt(2))
        ]

    [<Fact>] 
    let ``Should type is correct when check assign variable with object initialized with property float``() = 
        [
            Create("variableName", New([PropertySetter("Prop1", Float(1.0))]));
            Assign(Property(Variable("variableName"), "Prop1"), Float(2.0))
        ] |> expectedCorrect [
            TypedCreate(Type("obj-1", [("Prop1", TypeFloat)]), "variableName", TypedNew(Type("obj-1", [("Prop1", TypeFloat)]), [TypedPropertySetter(TypeFloat, "Prop1", TypedFloat(1.0))]));
            TypedAssign(TypeFloat, TypedProperty(TypedVariable("variableName"), "Prop1"), TypedFloat(2.0))
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
            TypedCreate(TypeInt, "variableName", TypedAdd(TypeInt, TypedInt(1), TypedInt(2)));
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
            TypedAssign(TypeFunc([], TypeInt), TypedVariable("variableName"), TypedFun([], [TypedReturn(TypedInt(2))]))
        ]

    [<Fact>] 
    let ``Should type is incorrect when check func and assign new func of float``() = 
        [
            Create("variableName", Fun([], [Return(Int(1))]));
            Assign(Variable("variableName"), Fun([], [Return(Float(2.0))]))
        ] |> expectedTypeError "Variable is not of type func() : float"

    [<Fact>] 
    let ``Should type is correct when check func add``() = 
        [
            Create("variableName", Fun(["x"; "y"], [Return(Add(Get(Variable("x")), Get(Variable("y"))))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeGeneric("T1"); TypeGeneric("T1")], TypeGeneric("T1")), 
                "variableName", 
                TypedFun(["x";"y"], [TypedReturn(TypedAdd(TypeGeneric("T1"), TypedGet(TypedVariable("x")),TypedGet(TypedVariable("y"))))]));
        ]

    [<Fact>] 
    let ``Should type is correct when check func add and seconde instruction of type int``() = 
        [
            Create("variableName", Fun(["x"; "y"], [Create("z", Add(Get(Variable("x")), Get(Variable("y")))); Return(Add(Get(Variable("z")), Int(1)))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeInt; TypeInt], TypeInt), 
                "variableName", 
                TypedFun(["x";"y"], [TypedCreate(TypeInt, "z", TypedAdd(TypeInt, TypedGet(TypedVariable("x")),TypedGet(TypedVariable("y")))); TypedReturn(TypedAdd(TypeInt, TypedGet(TypedVariable("z")), TypedInt(1)))]));
        ]

    [<Fact>] 
    let ``Should type is correct when check func add and seconde instruction of type string``() = 
        [
            Create("variableName", Fun(["x"; "y"], [Create("z", Add(Get(Variable("x")), Get(Variable("y")))); Return(Add(Get(Variable("z")), String("Test")))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeString; TypeString], TypeString), 
                "variableName", 
                TypedFun(["x";"y"], [TypedCreate(TypeString, "z", TypedAdd(TypeString, TypedGet(TypedVariable("x")),TypedGet(TypedVariable("y")))); TypedReturn(TypedAdd(TypeString, TypedGet(TypedVariable("z")), TypedString("Test")))]));
        ]

    [<Fact>] 
    let ``Should type is correct when check func add with int``() = 
        [
            Create("variableName", Fun(["x"], [Return(Add(Get(Variable("x")), Int(1)))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeInt], TypeInt), 
                "variableName", 
                TypedFun(["x"], [TypedReturn(TypedAdd(TypeInt, TypedGet(TypedVariable("x")),TypedInt(1)))]));
        ]

    [<Fact>] 
    let ``Should type is incorrect when check parameter out off scope of func``() = 
        [
            Create("variableName", Fun(["x"; "y"], [Return(Add(Get(Variable("x")), Get(Variable("y"))))]))
            Create("variableName2", Get(Variable("x")))
        ] |> expectedTypeError "Error to search type of variable x"

    let equalities : obj array seq = 
        seq { 
            yield [| Greater; TypedGreater |] 
            yield [| GreaterOrEqual; TypedGreaterOrEqual |] 
            yield [| Less; TypedLess |] 
            yield [| LessOrEqual; TypedLessOrEqual |] 
        }

    [<Theory>]
    [<MemberDataAttribute("equalities")>]
    let ``Should type is correct when check func and comparaison expression``(F:(Expression * Expression -> Expression), TF:(TypedExpression * TypedExpression -> TypedExpression)) = 
        [
            Create("f", Fun(["x"; "y"], [Return(F(Get(Variable("x")), Get(Variable("y"))))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeGeneric("T1"); TypeGeneric("T1")], TypeBool), 
                "f", 
                TypedFun(["x";"y"], [TypedReturn(TF(TypedGet(TypedVariable("x")),TypedGet(TypedVariable("y"))))]));
        ]

    [<Theory>]
    [<MemberDataAttribute("equalities")>]
    let ``Should type is correct when check func and comparaison expression with int``(F:(Expression * Expression -> Expression), TF:(TypedExpression * TypedExpression -> TypedExpression)) = 
        [
            Create("f", Fun(["x"], [Return(F(Get(Variable("x")), Int(1)))]))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeInt], TypeBool), 
                "f", 
                TypedFun(["x"], [TypedReturn(TF(TypedGet(TypedVariable("x")),TypedInt(1)))]));
        ]

    [<Fact>] 
    let ``Should type is correct when check call of func``() = 
        [
            Create("variableName", Fun(["x"], [Return(Add(Get(Variable("x")), Int(1)))]));
            Create("result", Call(Variable("variableName")))
        ] |> expectedCorrect [
            TypedCreate(
                TypeFunc([TypeInt], TypeInt), 
                "variableName", 
                TypedFun(["x"], [TypedReturn(TypedAdd(TypeInt, TypedGet(TypedVariable("x")),TypedInt(1)))]));
            TypedCreate(TypeInt, "result", TypedCall(TypedVariable("variableName")))
        ]

    [<Fact>] 
    let ``Should type is correct when program return int``() = 
        [
            Return(Int(1))
        ] |> expectedCorrect [
            TypedReturn(TypedInt(1))
        ]

    [<Fact>] 
    let ``Should type is incorrect when program return other from int and void``() = 
        [
            Return(Float(1.0))
        ] |> expectedTypeError "Program must be of type int or void"

    [<Fact>] 
    let ``Should type is correct when check use console``() = 
        [
            Create("variableName", Use("Console"))
        ] |> expectedCorrect [
            TypedCreate(
                Type("Console", [("Write", TypeFunc([TypeGeneric("T1")], TypeVoid))]), 
                "variableName", 
                TypedUse("Console"))
        ]

    [<Fact>] 
    let ``Should type is correct when check use random``() = 
        [
            Create("variableName", Use("Random"))
        ] |> expectedCorrect [
            TypedCreate(
                Type("Random", []), 
                "variableName", 
                TypedUse("Random"))
        ]

    [<Fact>] 
    let ``Should type is incorrect when check use of undefinedtype``() = 
        [
            Create("variableName", Use("undefinedtype"))
        ] |> expectedTypeError "Use of undefinedtype is not found"


    [<Fact>] 
    let ``Should type is correct when check func with call to a property``() = 
        [
            Create("f", Fun(["x"], [ Return(Get(Property(Variable("x"),"P")))]))
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([TypeInterface("I1", [("P", TypeGeneric("T2"))])], TypeGeneric("T2")), "f", TypedFun(["x"], [ TypedReturn(TypedGet(TypedProperty(TypedVariable("x"),"P")))]))
        ]

    [<Fact>] 
    let ``Should type is correct when check func with call to a property and add one``() = 
        [
            Create("f", Fun(["x"], [ Return(Add(Get(Property(Variable("x"),"P")), Int(1)))]))
        ] |> expectedCorrect [
            TypedCreate(TypeFunc([TypeInterface("I1", [("P", TypeInt)])], TypeInt), "f", TypedFun(["x"], [ TypedReturn(TypedAdd(TypeInt, TypedGet(TypedProperty(TypedVariable("x"),"P")), TypedInt(1)))]))
        ]
