// En savoir plus sur F# sur le site http://fsharp.org
// Voir le projet 'Didacticiel F#' pour obtenir de l'aide.

// Learn more about F# at http://fsharp.net

open System.IO
open Microsoft.FSharp.Text.Lexing

let LexParseOfString (code:string)= 
    use textReader = new System.IO.StringReader(code)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    Parser.start Lexer.token lexbuf

let LexParse (fileName:string)= 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    let countFromParser = Parser.start Lexer.token lexbuf

    Interpreter.Run countFromParser






[<EntryPoint>]
let main argv = 
    let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.kiss")
    LexParse testFile

    printfn "Press any key to continue..."
    System.Console.ReadLine() |> ignore    

    0 // retourne du code de sortie entier
