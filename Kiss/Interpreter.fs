module Interpreter

open AbstractSyntax

let Run x = 
    match x with 
    | Program(_) -> ();;