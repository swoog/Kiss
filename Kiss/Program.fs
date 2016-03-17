// En savoir plus sur F# sur le site http://fsharp.org
// Voir le projet 'Didacticiel F#' pour obtenir de l'aide.

// Learn more about F# at http://fsharp.net

open System.IO
open Microsoft.FSharp.Text.Lexing

exception ParsingError of string

let LexParseOfTextReader textReader =
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    try Parser.start Lexer.token lexbuf
    with e ->  let lex = (Lexer.token lexbuf)
               raise (ParsingError("Line " + lexbuf.EndPos.Line.ToString()))


let LexParseOfString (code:string)= 
    use textReader = new System.IO.StringReader(code)
    LexParseOfTextReader textReader

let LexParse (fileName:string)= 
    use textReader = new System.IO.StreamReader(fileName)
    let abstractSyntax = LexParseOfTextReader textReader
    Interpreter.Run abstractSyntax






[<EntryPoint>]
let main argv = 
    let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.kiss")
    LexParse testFile

    printfn "Press any key to continue..."
    System.Console.ReadLine() |> ignore    

    0 // retourne du code de sortie entier
