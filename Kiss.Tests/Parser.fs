module Parser
    open Xunit 
    open Program
    open AbstractSyntax

    [<Fact>] 
    let ``Should parse one statement assign variable when initialized by constant``() = 
        let line = "var variableName = 1"
        let abstractsyntax = Program.LexParseOfString line
        let expected = AbstractSyntax.Program([AbstractSyntax.Assign("variableName", AbstractSyntax.Int(1))])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>] 
    let ``Should parse one statement assign variable when initialized by expression 1 add 1``() = 
        let line = "var variableName = 1 + 1"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            AbstractSyntax.Program([
                                    AbstractSyntax.Assign("variableName", 
                                        AbstractSyntax.Add(AbstractSyntax.Int(1), AbstractSyntax.Int(1)))
                                    ])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression use``() = 
        let line = "var console = use(Console)"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            AbstractSyntax.Program([
                                    AbstractSyntax.Assign("console", 
                                        AbstractSyntax.Use("Console"))
                                    ])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse one statement when initialized by empty object``() = 
        let line = "var variableName = {}"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            AbstractSyntax.Program([
                                    AbstractSyntax.Assign("variableName", 
                                        AbstractSyntax.New)
                                    ])
        Assert.Equal(expected, abstractsyntax)

    [<Fact>]
    let ``Should parse expression method``() = 
        let line = "var returnOne = fun() -> 1"
        let abstractsyntax = Program.LexParseOfString line
        let expected = 
            AbstractSyntax.Program([
                                    AbstractSyntax.Assign("returnOne", 
                                        AbstractSyntax.Fun([], AbstractSyntax.Int(1)))
                                    ])
        Assert.Equal(expected, abstractsyntax)