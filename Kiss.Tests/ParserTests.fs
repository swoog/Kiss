module ParserTests
    open Xunit 
    open Program
    open AbstractSyntax

    [<Fact>] 
    let ``Should parse one statement create variable when initialized by constant``() = 
        let line = "var variableName = 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = Program([Create("variableName", Int(1))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>] 
    let ``Should parse one statement create variable when initialized by constant true``() = 
        let line = "var variableName = true;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = Program([Create("variableName", Bool(true))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>] 
    let ``Should parse one statement create variable when initialized by constant false``() = 
        let line = "var variableName = false;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = Program([Create("variableName", Bool(false))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>] 
    let ``Should parse two statements when assign new value``() = 
        let line = "var variableName = 1;\nvariableName = 2;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = Program([Create("variableName", Int(1));Assign(Variable("variableName"), Int(2))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>] 
    let ``Should parse one statement create variable when initialized by expression 1 add 1``() = 
        let line = "var variableName = 1 + 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", Add(Int(1), Int(1)))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression use``() = 
        let line = "var console = use(Console);"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("console", Use("Console"))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method``() = 
        let line = "var returnOne = fun() -> 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("returnOne", Fun([], [Return(Int(1))]))])
        Assert.Equal(expected, abstractsyntax)

    let expected expect line = 
        let abstractsyntax = Program.LexParseOfString line
        Assert.Equal(expect, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method when have parameter``() = 
        "var f = fun(x) -> x;"
        |> expected (Program([Create("f", Fun(["x"], [Return(Get(Variable("x")))]))]))


    [<Fact>]
    let ``Should parse expression method When have multiple line``() = 
        let line = "var returnOne = fun() -> {\n var v = 1; \nreturn v;\n};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("returnOne", Fun([], [Create("v", Int(1)); Return(Get(Variable("v")))]))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method When have recursion``() = 
        "var f1 = fun() -> {var f2 = fun() -> {return 1;};};"
        |> expected (Program([Create("f1", Fun([], [Create("f2",Fun([], [Return(Int(1))]))]))]))

    [<Fact>]
    let ``Should parse expression method When is property``() = 
        "var c = { P = fun() -> {return 1;}};"
        |> expected (Program([Create("c", New([PropertySetter("P", Fun([], [Return(Int(1))]))]))]))

    [<Fact>]
    let ``Should parse call method``() = 
        "var returnOne = fun() -> 1; \nreturn returnOne();"
        |> expected (Program([Create("returnOne", Fun([], [Return(Int(1))]));Return(Call(Variable("returnOne")))]))

    [<Fact>]
    let ``Should throw error when parsing is false``() = 
        let line = "var returnOne = \nfun() -> 1 \nreturn returnOne();"
        try 
            Program.LexParseOfString line |> ignore
            raise(System.Exception("Expected ParseError"))
        with
        | ParsingError(x) -> Assert.Equal("Line 2", x)
        | _ -> raise(System.Exception("Expected ParseError"))


    [<Fact>]
    let ``Should parse one statement when initialized by empty object``() = 
        let line = "var variableName = {};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New([]))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should initialized object when set one property``() = 
        let line = "var variableName = { Prop1 = 1};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New([PropertySetter("Prop1", Int(1))]))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should initialized object when set two property``() = 
        let line = "var variableName = { Prop1 = 1, Prop2 = 2};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New([PropertySetter("Prop1", Int(1));PropertySetter("Prop2", Int(2))]))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should set property of object``() = 
        let line = "var variableName = { }; variableName.Prop1 = 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New([]));Assign(Property(Variable("variableName"), "Prop1"), Int(1))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should set sub property of object``() = 
        let line = "var variableName = { Prop1 = {} }; variableName.Prop1.SubProp = 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New([PropertySetter("Prop1", New([]))]));Assign(Property(Property(Variable("variableName"), "Prop1"), "SubProp"), Int(1))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should greater than``() = 
        let line = "var v = 1 > 0;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", Greater(Int(1), Int(0)))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should less than``() = 
        let line = "var v = 1 < 0;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", Less(Int(1), Int(0)))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should greate or equal than``() = 
        let line = "var v = 1 >= 0;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", GreaterOrEqual(Int(1), Int(0)))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should less or equal than``() = 
        let line = "var v = 1 <= 0;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", LessOrEqual(Int(1), Int(0)))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse call method of property``() = 
        let line = "return c.Method();"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Return(Call(Property(Variable("c"),"Method")))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse string constante``() = 
        let line = "var v = \"test of string\";"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", String("test of string"))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse string constante when have ignore quote``() = 
        let line = "var v = \"test of string with \\\" charactere\";"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("v", String("test of string with \" charactere"))])
        Assert.Equal(expected, abstractsyntax)
