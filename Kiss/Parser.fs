// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open AbstractSyntax

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | DESINCR
  | AND
  | DOT
  | EQUAL
  | STRING
  | SEMI
  | COMMA
  | GREATER
  | LESS
  | GREATEREQUAL
  | LESSEQUAL
  | FUN
  | ARROW
  | RETURN
  | VAR
  | IF
  | ELSE
  | INCR
  | USE
  | PLUS
  | MINUS
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | INT of (int)
  | NAME of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_DESINCR
    | TOKEN_AND
    | TOKEN_DOT
    | TOKEN_EQUAL
    | TOKEN_STRING
    | TOKEN_SEMI
    | TOKEN_COMMA
    | TOKEN_GREATER
    | TOKEN_LESS
    | TOKEN_GREATEREQUAL
    | TOKEN_LESSEQUAL
    | TOKEN_FUN
    | TOKEN_ARROW
    | TOKEN_RETURN
    | TOKEN_VAR
    | TOKEN_IF
    | TOKEN_ELSE
    | TOKEN_INCR
    | TOKEN_USE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_INT
    | TOKEN_NAME
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_File
    | NONTERM_PropertyInitializer
    | NONTERM_PropertiesInitializer
    | NONTERM_Variable
    | NONTERM_Parameters
    | NONTERM_Expression
    | NONTERM_Statement
    | NONTERM_StatementList

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | DESINCR  -> 1 
  | AND  -> 2 
  | DOT  -> 3 
  | EQUAL  -> 4 
  | STRING  -> 5 
  | SEMI  -> 6 
  | COMMA  -> 7 
  | GREATER  -> 8 
  | LESS  -> 9 
  | GREATEREQUAL  -> 10 
  | LESSEQUAL  -> 11 
  | FUN  -> 12 
  | ARROW  -> 13 
  | RETURN  -> 14 
  | VAR  -> 15 
  | IF  -> 16 
  | ELSE  -> 17 
  | INCR  -> 18 
  | USE  -> 19 
  | PLUS  -> 20 
  | MINUS  -> 21 
  | LPAREN  -> 22 
  | RPAREN  -> 23 
  | LBRACE  -> 24 
  | RBRACE  -> 25 
  | INT _ -> 26 
  | NAME _ -> 27 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_DESINCR 
  | 2 -> TOKEN_AND 
  | 3 -> TOKEN_DOT 
  | 4 -> TOKEN_EQUAL 
  | 5 -> TOKEN_STRING 
  | 6 -> TOKEN_SEMI 
  | 7 -> TOKEN_COMMA 
  | 8 -> TOKEN_GREATER 
  | 9 -> TOKEN_LESS 
  | 10 -> TOKEN_GREATEREQUAL 
  | 11 -> TOKEN_LESSEQUAL 
  | 12 -> TOKEN_FUN 
  | 13 -> TOKEN_ARROW 
  | 14 -> TOKEN_RETURN 
  | 15 -> TOKEN_VAR 
  | 16 -> TOKEN_IF 
  | 17 -> TOKEN_ELSE 
  | 18 -> TOKEN_INCR 
  | 19 -> TOKEN_USE 
  | 20 -> TOKEN_PLUS 
  | 21 -> TOKEN_MINUS 
  | 22 -> TOKEN_LPAREN 
  | 23 -> TOKEN_RPAREN 
  | 24 -> TOKEN_LBRACE 
  | 25 -> TOKEN_RBRACE 
  | 26 -> TOKEN_INT 
  | 27 -> TOKEN_NAME 
  | 30 -> TOKEN_end_of_input
  | 28 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_File 
    | 3 -> NONTERM_PropertyInitializer 
    | 4 -> NONTERM_PropertiesInitializer 
    | 5 -> NONTERM_PropertiesInitializer 
    | 6 -> NONTERM_Variable 
    | 7 -> NONTERM_Variable 
    | 8 -> NONTERM_Parameters 
    | 9 -> NONTERM_Parameters 
    | 10 -> NONTERM_Expression 
    | 11 -> NONTERM_Expression 
    | 12 -> NONTERM_Expression 
    | 13 -> NONTERM_Expression 
    | 14 -> NONTERM_Expression 
    | 15 -> NONTERM_Expression 
    | 16 -> NONTERM_Expression 
    | 17 -> NONTERM_Expression 
    | 18 -> NONTERM_Expression 
    | 19 -> NONTERM_Expression 
    | 20 -> NONTERM_Expression 
    | 21 -> NONTERM_Expression 
    | 22 -> NONTERM_Expression 
    | 23 -> NONTERM_Expression 
    | 24 -> NONTERM_Expression 
    | 25 -> NONTERM_Statement 
    | 26 -> NONTERM_Statement 
    | 27 -> NONTERM_Statement 
    | 28 -> NONTERM_StatementList 
    | 29 -> NONTERM_StatementList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 30 
let _fsyacc_tagOfErrorTerminal = 28

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | DESINCR  -> "DESINCR" 
  | AND  -> "AND" 
  | DOT  -> "DOT" 
  | EQUAL  -> "EQUAL" 
  | STRING  -> "STRING" 
  | SEMI  -> "SEMI" 
  | COMMA  -> "COMMA" 
  | GREATER  -> "GREATER" 
  | LESS  -> "LESS" 
  | GREATEREQUAL  -> "GREATEREQUAL" 
  | LESSEQUAL  -> "LESSEQUAL" 
  | FUN  -> "FUN" 
  | ARROW  -> "ARROW" 
  | RETURN  -> "RETURN" 
  | VAR  -> "VAR" 
  | IF  -> "IF" 
  | ELSE  -> "ELSE" 
  | INCR  -> "INCR" 
  | USE  -> "USE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | LBRACE  -> "LBRACE" 
  | RBRACE  -> "RBRACE" 
  | INT _ -> "INT" 
  | NAME _ -> "NAME" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | DESINCR  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | STRING  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | GREATER  -> (null : System.Object) 
  | LESS  -> (null : System.Object) 
  | GREATEREQUAL  -> (null : System.Object) 
  | LESSEQUAL  -> (null : System.Object) 
  | FUN  -> (null : System.Object) 
  | ARROW  -> (null : System.Object) 
  | RETURN  -> (null : System.Object) 
  | VAR  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | INCR  -> (null : System.Object) 
  | USE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | LBRACE  -> (null : System.Object) 
  | RBRACE  -> (null : System.Object) 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 4us; 65535us; 11us; 12us; 25us; 9us; 26us; 9us; 27us; 9us; 3us; 65535us; 25us; 10us; 26us; 10us; 27us; 10us; 17us; 65535us; 0us; 15us; 4us; 15us; 7us; 14us; 26us; 15us; 27us; 15us; 37us; 14us; 39us; 15us; 42us; 14us; 44us; 15us; 54us; 14us; 55us; 14us; 56us; 14us; 57us; 14us; 58us; 14us; 61us; 14us; 62us; 14us; 63us; 14us; 1us; 65535us; 35us; 19us; 11us; 65535us; 7us; 8us; 37us; 38us; 42us; 43us; 54us; 46us; 55us; 47us; 56us; 48us; 57us; 49us; 58us; 50us; 61us; 51us; 62us; 52us; 63us; 53us; 6us; 65535us; 0us; 64us; 4us; 66us; 26us; 64us; 27us; 64us; 39us; 66us; 44us; 66us; 3us; 65535us; 0us; 4us; 26us; 39us; 27us; 44us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 10us; 14us; 32us; 34us; 46us; 53us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 29us; 1us; 3us; 2us; 3us; 6us; 1us; 3us; 6us; 3us; 20us; 21us; 22us; 23us; 24us; 1us; 4us; 2us; 5us; 14us; 1us; 5us; 1us; 5us; 1us; 6us; 3us; 7us; 10us; 11us; 2us; 7us; 26us; 1us; 7us; 1us; 7us; 1us; 8us; 3us; 9us; 18us; 19us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 12us; 2us; 13us; 14us; 3us; 13us; 14us; 17us; 3us; 13us; 14us; 19us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 4us; 16us; 17us; 18us; 19us; 4us; 16us; 17us; 18us; 19us; 2us; 16us; 17us; 2us; 16us; 17us; 6us; 16us; 20us; 21us; 22us; 23us; 24us; 2us; 17us; 29us; 1us; 17us; 2us; 18us; 19us; 2us; 18us; 19us; 6us; 18us; 20us; 21us; 22us; 23us; 24us; 2us; 19us; 29us; 1us; 19us; 6us; 20us; 20us; 21us; 22us; 23us; 24us; 6us; 20us; 21us; 21us; 22us; 23us; 24us; 6us; 20us; 21us; 22us; 22us; 23us; 24us; 6us; 20us; 21us; 22us; 23us; 23us; 24us; 6us; 20us; 21us; 22us; 23us; 24us; 24us; 6us; 20us; 21us; 22us; 23us; 24us; 25us; 6us; 20us; 21us; 22us; 23us; 24us; 26us; 6us; 20us; 21us; 22us; 23us; 24us; 27us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 25us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 28us; 1us; 29us; 1us; 29us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 13us; 16us; 18us; 25us; 27us; 30us; 32us; 34us; 36us; 40us; 43us; 45us; 47us; 49us; 53us; 55us; 57us; 59us; 61us; 63us; 66us; 70us; 74us; 76us; 78us; 80us; 82us; 84us; 86us; 91us; 96us; 99us; 102us; 109us; 112us; 114us; 117us; 120us; 127us; 130us; 132us; 139us; 146us; 153us; 160us; 167us; 174us; 181us; 188us; 190us; 192us; 194us; 196us; 198us; 200us; 202us; 204us; 206us; 208us; 210us; 212us; 214us; |]
let _fsyacc_action_rows = 68
let _fsyacc_actionTableElements = [|3us; 32768us; 14us; 63us; 15us; 59us; 27us; 13us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 3us; 16386us; 14us; 63us; 15us; 59us; 27us; 13us; 1us; 32768us; 4us; 7us; 1us; 16390us; 4us; 7us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 16387us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 0us; 16388us; 2us; 32768us; 7us; 11us; 25us; 29us; 1us; 32768us; 27us; 5us; 0us; 16389us; 0us; 16390us; 2us; 16395us; 3us; 16us; 22us; 22us; 2us; 32768us; 3us; 16us; 4us; 62us; 1us; 32768us; 27us; 17us; 0us; 16391us; 0us; 16392us; 2us; 32768us; 7us; 20us; 23us; 41us; 1us; 32768us; 27us; 21us; 0us; 16393us; 1us; 32768us; 23us; 23us; 0us; 16394us; 0us; 16396us; 2us; 32768us; 25us; 28us; 27us; 5us; 4us; 32768us; 14us; 63us; 15us; 59us; 25us; 28us; 27us; 6us; 4us; 32768us; 14us; 63us; 15us; 59us; 25us; 28us; 27us; 6us; 0us; 16397us; 0us; 16398us; 1us; 32768us; 22us; 31us; 1us; 32768us; 27us; 32us; 1us; 32768us; 23us; 33us; 0us; 16399us; 1us; 32768us; 22us; 35us; 2us; 32768us; 23us; 36us; 27us; 18us; 1us; 32768us; 13us; 37us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 26us; 26us; 24us; 27us; 13us; 5us; 16400us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 4us; 32768us; 14us; 63us; 15us; 59us; 25us; 40us; 27us; 13us; 0us; 16401us; 1us; 32768us; 13us; 42us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 27us; 26us; 24us; 27us; 13us; 5us; 16402us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 4us; 32768us; 14us; 63us; 15us; 59us; 25us; 45us; 27us; 13us; 0us; 16403us; 5us; 16404us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16405us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16406us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16407us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16408us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16409us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16410us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 16411us; 8us; 54us; 9us; 55us; 10us; 56us; 11us; 57us; 20us; 58us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 1us; 32768us; 27us; 60us; 1us; 32768us; 4us; 61us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 5us; 32768us; 12us; 34us; 19us; 30us; 24us; 25us; 26us; 24us; 27us; 13us; 1us; 32768us; 6us; 65us; 0us; 16412us; 1us; 32768us; 6us; 67us; 0us; 16413us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 4us; 5us; 7us; 8us; 12us; 14us; 16us; 22us; 28us; 29us; 32us; 34us; 35us; 36us; 39us; 42us; 44us; 45us; 46us; 49us; 51us; 52us; 54us; 55us; 56us; 59us; 64us; 69us; 70us; 71us; 73us; 75us; 77us; 78us; 80us; 83us; 85us; 91us; 97us; 102us; 103us; 105us; 111us; 117us; 122us; 123us; 129us; 135us; 141us; 147us; 153us; 159us; 165us; 171us; 177us; 183us; 189us; 195us; 201us; 203us; 205us; 211us; 217us; 223us; 225us; 226us; 228us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 1us; 3us; 1us; 3us; 1us; 3us; 3us; 1us; 1us; 2us; 3us; 4us; 5us; 7us; 6us; 8us; 3us; 3us; 3us; 3us; 3us; 4us; 3us; 2us; 2us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 8us; 8us; 8us; 9us; 9us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 16388us; 65535us; 65535us; 16389us; 16390us; 65535us; 65535us; 65535us; 16391us; 16392us; 65535us; 65535us; 16393us; 65535us; 16394us; 16396us; 65535us; 65535us; 65535us; 16397us; 16398us; 65535us; 65535us; 65535us; 16399us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16401us; 65535us; 65535us; 65535us; 65535us; 16403us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16412us; 65535us; 16413us; |]
let _fsyacc_reductions ()  =    [| 
# 265 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  AbstractSyntax.Prog )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 274 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'File)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                       _1 
                   )
# 23 "Parser.fsy"
                 :  AbstractSyntax.Prog ));
# 285 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                           Program(List.rev(_1)) 
                   )
# 26 "Parser.fsy"
                 : 'File));
# 296 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                                                                                  PropertySetter(_1, _3) 
                   )
# 28 "Parser.fsy"
                 : 'PropertyInitializer));
# 308 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'PropertyInitializer)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                                                                                  [_1] 
                   )
# 30 "Parser.fsy"
                 : 'PropertiesInitializer));
# 319 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'PropertiesInitializer)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'PropertyInitializer)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                                                                                  _3::_1 
                   )
# 31 "Parser.fsy"
                 : 'PropertiesInitializer));
# 331 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                                                                                  Variable(_1) 
                   )
# 33 "Parser.fsy"
                 : 'Variable));
# 342 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Variable)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                                                                                  Property(_1, _3) 
                   )
# 34 "Parser.fsy"
                 : 'Variable));
# 354 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                                                                                                  [_1] 
                   )
# 36 "Parser.fsy"
                 : 'Parameters));
# 365 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Parameters)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                                                                  _3::_1 
                   )
# 37 "Parser.fsy"
                 : 'Parameters));
# 377 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Variable)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                                                  Call(_1) 
                   )
# 39 "Parser.fsy"
                 : 'Expression));
# 388 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Variable)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                                                                                  Get(_1) 
                   )
# 40 "Parser.fsy"
                 : 'Expression));
# 399 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                                                                  Int(_1) 
                   )
# 41 "Parser.fsy"
                 : 'Expression));
# 410 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                                                                  New([]) 
                   )
# 42 "Parser.fsy"
                 : 'Expression));
# 420 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'PropertiesInitializer)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                                                                  New(List.rev(_2)) 
                   )
# 43 "Parser.fsy"
                 : 'Expression));
# 431 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                                                                  Use(_3) 
                   )
# 44 "Parser.fsy"
                 : 'Expression));
# 442 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                                                                  Fun([], [Return(_5)]) 
                   )
# 45 "Parser.fsy"
                 : 'Expression));
# 453 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                                                  Fun([], List.rev(_6)) 
                   )
# 46 "Parser.fsy"
                 : 'Expression));
# 464 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Parameters)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                                                                  Fun(List.rev(_3), [Return(_6)]) 
                   )
# 47 "Parser.fsy"
                 : 'Expression));
# 476 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Parameters)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                                                                                  Fun(List.rev(_3), List.rev(_7)) 
                   )
# 48 "Parser.fsy"
                 : 'Expression));
# 488 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                                                                  Greater(_1, _3) 
                   )
# 49 "Parser.fsy"
                 : 'Expression));
# 500 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                                                                  Less(_1, _3) 
                   )
# 50 "Parser.fsy"
                 : 'Expression));
# 512 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                                                                                  GreaterOrEqual(_1, _3) 
                   )
# 51 "Parser.fsy"
                 : 'Expression));
# 524 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                                                                  LessOrEqual(_1, _3) 
                   )
# 52 "Parser.fsy"
                 : 'Expression));
# 536 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                                                                  Add(_1, _3) 
                   )
# 53 "Parser.fsy"
                 : 'Expression));
# 548 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "Parser.fsy"
                                                                                                  Create(_2, _4) 
                   )
# 55 "Parser.fsy"
                 : 'Statement));
# 560 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Variable)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                                                                  Assign(_1, _3) 
                   )
# 56 "Parser.fsy"
                 : 'Statement));
# 572 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                                                  Return(_2) 
                   )
# 57 "Parser.fsy"
                 : 'Statement));
# 583 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Statement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                                                                  [_1] 
                   )
# 59 "Parser.fsy"
                 : 'StatementList));
# 594 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Statement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                                                                  _2::_1 
                   )
# 60 "Parser.fsy"
                 : 'StatementList));
|]
# 607 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 31;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  AbstractSyntax.Prog  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
