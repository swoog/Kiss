module IlOptimizer

open System
open System.Reflection
open System.Reflection.Emit
open ToIl

let rec optimize p = 
    match p with
    | ToIl.Assembly(name, classes) -> Assembly(name, optimizeClasses classes)
and optimizeClasses classes = 
    match classes with
    | [] -> []
    | Class(name, elements)::l -> Class(name, optimizeElements elements)::(optimizeClasses l)
and optimizeElements elements =
    match elements with
    | [] -> []
    | Method(p, v, name, ins)::l -> Method(p, v, name, optimizedInstructions ins)::(optimizeElements l)
    | EntryPoint(p, v, name, ins)::l -> EntryPoint(p, v, name, optimizedInstructions ins)::(optimizeElements l)
    | Field(name, t)::l -> Field(name, t)::(optimizeElements l)
and optimizedInstructions ins = ins