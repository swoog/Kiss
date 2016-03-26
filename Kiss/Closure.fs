module Closure
    open AbstractSyntax

    exception NotFoundVariable

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
    and closurePropertie variables p =
        match p with
        | PropertySetter(name, e) -> PropertySetter(name, closureExpression e variables)
    and closureExpression e variables = 
        match e with
        | Int(i) -> Int(i)
        | Float(f) -> Float(f)
        | Get(v) -> Get(closureVariable v variables)
        | Fun(p, ss) -> Fun(p, closureStatements ss variables)
        | Add(e1, e2) -> Add(closureExpression e1 variables, closureExpression e2 variables)
        | Call(v) -> Call(closureVariable v variables)
        | New(ps) -> New(List.map (closurePropertie variables) ps)

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

