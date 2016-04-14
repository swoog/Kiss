module Closure
    open AbstractSyntax

    exception NotFoundVariable of string

    let rec createVariable name variables = 
        match variables with
        | [] -> ([(name, name)], name)
        | (oldName, newName)::l -> if oldName = name then
                                        let newName2 = newName + "-1"
                                        ((oldName, newName2)::((oldName, newName)::l), newName2)
                                    else
                                        let (l, newName) = createVariable name l
                                        in ((oldName, newName)::l, newName)
    let rec createVariables names variables =
        match names with
        | [] -> (variables, [])
        | a::l -> let (variables, newName) = createVariable a variables
                  let (variables, newNames) = createVariables l variables
                  in (variables, newName::newNames)

    let rec getVariable v variables =
        match variables with
        | [] -> raise(NotFoundVariable("Variable " + v + " was not found"))
        | (oldName, newName)::l -> if oldName = v then
                                     newName
                                   else
                                     getVariable v l

    let rec closureVariable v variables =
        match v with
        | Variable(v) -> Variable(getVariable v variables)
        | Property(v, p) -> Property(closureVariable v variables, p)
    and closurePropertie variables p =
        match p with
        | PropertySetter(name, e) -> PropertySetter(name, closureExpression e variables)
    and closureExpression e variables = 
        match e with
        | Int(i) -> Int(i)
        | Float(f) -> Float(f)
        | Get(v) -> Get(closureVariable v variables)
        | Fun(p, ss) -> let (variables, newNames) = (createVariables p [])
                        in Fun(newNames, closureStatements ss variables)
        | Add(e1, e2) -> Add(closureExpression e1 variables, closureExpression e2 variables)
        | Call(v) -> Call(closureVariable v variables)
        | New(ps) -> New(List.map (closurePropertie variables) ps)
        | Use(name) -> Use(name)
        | Greater(ex1, ex2) -> Greater(closureExpression ex1 variables, closureExpression ex2 variables)
        | GreaterOrEqual(ex1, ex2) -> GreaterOrEqual(closureExpression ex1 variables, closureExpression ex2 variables)
        | Less(ex1, ex2) -> Less(closureExpression ex1 variables, closureExpression ex2 variables)
        | LessOrEqual(ex1, ex2) -> LessOrEqual(closureExpression ex1 variables, closureExpression ex2 variables)

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

    and closureProgram p = 
        match p with
        | Program(t) -> Program(closureStatements t [])

