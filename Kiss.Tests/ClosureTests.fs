module ClosureTests
    open Xunit 
    open Program
    open AbstractSyntax
    open TypeChecker

    exception NotFoundVariable

    let statement a = Program(a)

    let rec createVariable name variables = 
        match variables with
        | [] -> ([(name, name)], name)
        | (oldName, newName)::l -> if oldName = name then
                                        let newName2 = newName + "-1"
                                        ((oldName, newName2)::((oldName, newName)::l), newName2)
                                    else
                                        let (l, newName) = createVariable name l
                                        in ((oldName, newName)::l, newName)
    let rec getVariable v variables =
        match variables with
        | [] -> raise(NotFoundVariable)
        | (oldName, newName)::l -> if oldName = v then
                                     newName
                                   else
                                     getVariable v l

    let rec closureVariable v variables =
        match v with
        | Variable(v) -> Variable(getVariable v variables)
        | Property(v, p) -> Property(closureVariable v variables, p)
    and closureExpression e variables = 
        match e with
        | Get(v) -> Get(closureVariable v variables)
        | Fun(p, ss) -> Fun(p, closureStatements ss variables)
        | e -> e
    and closureStatement s variables = 
        match s with
        | Create(name, e) -> let e = closureExpression e variables
                             let (variables, newName) = createVariable name variables
                             in (variables, Create(newName, e))
        | Assign(v, e) -> (variables, Assign(closureVariable v variables, closureExpression e variables))
        | Return(e) -> (variables, Return(closureExpression e variables))
    
    and closureStatements ss variables = 
        match ss with
        | [] -> []
        | s::l -> let (variables, s) = closureStatement s variables
                  in s::(closureStatements l variables)

    and closure p = 
        match p with
        | Program(t) -> Program(closureStatements t [])

    let expectedTypeError errorMessage lines =
        try
            closure (statement lines) |> ignore
            raise(System.Exception("Expected TypeError received no exception"))
        with
        | TypeError(x) -> Assert.Equal(errorMessage, x)
        | e -> raise(System.Exception("Expected TypeError received " + e.ToString()))

    let expectedCorrect expectedLines lines= 
        let resultats = closure (statement lines)
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