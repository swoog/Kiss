module Parser
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
    let ``Should parse two statements when assign new value``() = 
        let line = "var variableName = 1;\nvariableName = 2;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = Program([Create("variableName", Int(1));Assign("variableName", Int(2))])
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
    let ``Should parse one statement when initialized by empty object``() = 
        let line = "var variableName = {};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("variableName", New)])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method``() = 
        let line = "var returnOne = fun() -> 1;"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("returnOne", Fun([], [Return(Int(1))]))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method When have multiple line``() = 
        let line = "var returnOne = fun() -> {\n var v = 1; \nreturn v;\n};"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            Program([Create("returnOne", Fun([], [Create("v", Int(1)); Return(Variable("v"))]))])
        Assert.Equal(expected, abstractsyntax)