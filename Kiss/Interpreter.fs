module Interpreter

open TypedAbstractSyntax

let Run x = 
    match x with 
    | TypedProgram(_) -> ();;