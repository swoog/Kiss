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
  | FUN  -> 7 
  | ARROW  -> 8 
  | RETURN  -> 9 
  | VAR  -> 10 
  | IF  -> 11 
  | ELSE  -> 12 
  | INCR  -> 13 
  | USE  -> 14 
  | PLUS  -> 15 
  | MINUS  -> 16 
  | LPAREN  -> 17 
  | RPAREN  -> 18 
  | LBRACE  -> 19 
  | RBRACE  -> 20 
  | INT _ -> 21 
  | NAME _ -> 22 

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
  | 7 -> TOKEN_FUN 
  | 8 -> TOKEN_ARROW 
  | 9 -> TOKEN_RETURN 
  | 10 -> TOKEN_VAR 
  | 11 -> TOKEN_IF 
  | 12 -> TOKEN_ELSE 
  | 13 -> TOKEN_INCR 
  | 14 -> TOKEN_USE 
  | 15 -> TOKEN_PLUS 
  | 16 -> TOKEN_MINUS 
  | 17 -> TOKEN_LPAREN 
  | 18 -> TOKEN_RPAREN 
  | 19 -> TOKEN_LBRACE 
  | 20 -> TOKEN_RBRACE 
  | 21 -> TOKEN_INT 
  | 22 -> TOKEN_NAME 
  | 25 -> TOKEN_end_of_input
  | 23 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_File 
    | 3 -> NONTERM_Expression 
    | 4 -> NONTERM_Expression 
    | 5 -> NONTERM_Expression 
    | 6 -> NONTERM_Expression 
    | 7 -> NONTERM_Expression 
    | 8 -> NONTERM_Expression 
    | 9 -> NONTERM_Expression 
    | 10 -> NONTERM_Expression 
    | 11 -> NONTERM_Statement 
    | 12 -> NONTERM_Statement 
    | 13 -> NONTERM_Statement 
    | 14 -> NONTERM_StatementList 
    | 15 -> NONTERM_StatementList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 25 
let _fsyacc_tagOfErrorTerminal = 23

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
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 5us; 65535us; 19us; 22us; 27us; 23us; 30us; 24us; 32us; 25us; 33us; 26us; 4us; 65535us; 0us; 34us; 4us; 36us; 10us; 34us; 20us; 36us; 2us; 65535us; 0us; 4us; 10us; 20us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 11us; 16us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 15us; 1us; 3us; 2us; 4us; 5us; 1us; 4us; 1us; 4us; 1us; 6us; 2us; 6us; 8us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 7us; 2us; 8us; 9us; 2us; 8us; 9us; 2us; 8us; 9us; 2us; 8us; 9us; 2us; 8us; 15us; 1us; 8us; 2us; 9us; 10us; 2us; 10us; 10us; 2us; 10us; 11us; 2us; 10us; 12us; 2us; 10us; 13us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 15us; 1us; 15us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 13us; 16us; 18us; 20us; 22us; 25us; 27us; 29us; 31us; 33us; 35us; 38us; 41us; 44us; 47us; 50us; 52us; 55us; 58us; 61us; 64us; 67us; 69us; 71us; 73us; 75us; 77us; 79us; 81us; 83us; 85us; 87us; |]
let _fsyacc_action_rows = 38
let _fsyacc_actionTableElements = [|3us; 32768us; 9us; 33us; 10us; 28us; 22us; 31us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 3us; 16386us; 9us; 33us; 10us; 28us; 22us; 31us; 0us; 16387us; 1us; 16389us; 17us; 7us; 1us; 32768us; 18us; 8us; 0us; 16388us; 1us; 32768us; 20us; 11us; 4us; 32768us; 9us; 33us; 10us; 28us; 20us; 11us; 22us; 31us; 0us; 16390us; 1us; 32768us; 17us; 13us; 1us; 32768us; 22us; 14us; 1us; 32768us; 18us; 15us; 0us; 16391us; 1us; 32768us; 17us; 17us; 1us; 32768us; 18us; 18us; 1us; 32768us; 8us; 19us; 5us; 32768us; 7us; 16us; 14us; 12us; 19us; 10us; 21us; 5us; 22us; 6us; 4us; 32768us; 9us; 33us; 10us; 28us; 20us; 21us; 22us; 31us; 0us; 16392us; 1us; 16393us; 15us; 27us; 1us; 16394us; 15us; 27us; 1us; 16395us; 15us; 27us; 1us; 16396us; 15us; 27us; 1us; 16397us; 15us; 27us; 5us; 32768us; 7us; 16us; 14us; 12us; 19us; 9us; 21us; 5us; 22us; 6us; 1us; 32768us; 22us; 29us; 1us; 32768us; 4us; 30us; 5us; 32768us; 7us; 16us; 14us; 12us; 19us; 9us; 21us; 5us; 22us; 6us; 1us; 32768us; 4us; 32us; 5us; 32768us; 7us; 16us; 14us; 12us; 19us; 9us; 21us; 5us; 22us; 6us; 5us; 32768us; 7us; 16us; 14us; 12us; 19us; 9us; 21us; 5us; 22us; 6us; 1us; 32768us; 6us; 35us; 0us; 16398us; 1us; 32768us; 6us; 37us; 0us; 16399us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 4us; 5us; 7us; 8us; 12us; 13us; 15us; 17us; 18us; 20us; 25us; 26us; 28us; 30us; 32us; 33us; 35us; 37us; 39us; 45us; 50us; 51us; 53us; 55us; 57us; 59us; 61us; 67us; 69us; 71us; 77us; 79us; 85us; 91us; 93us; 94us; 96us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 3us; 1us; 2us; 4us; 7us; 5us; 3us; 4us; 3us; 2us; 2us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 16387us; 65535us; 65535us; 16388us; 65535us; 65535us; 16390us; 65535us; 65535us; 65535us; 16391us; 65535us; 65535us; 65535us; 65535us; 65535us; 16392us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16398us; 65535us; 16399us; |]
let _fsyacc_reductions ()  =    [| 
# 217 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  AbstractSyntax.Prog )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 226 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'File)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 "Parser.fsy"
                                       _1 
                   )
# 21 "Parser.fsy"
                 :  AbstractSyntax.Prog ));
# 237 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "Parser.fsy"
                                           Program(List.rev(_1)) 
                   )
# 24 "Parser.fsy"
                 : 'File));
# 248 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                                                                         Int(_1) 
                   )
# 26 "Parser.fsy"
                 : 'Expression));
# 259 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                                                                         Call(_1) 
                   )
# 27 "Parser.fsy"
                 : 'Expression));
# 270 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                                                                         Variable(_1) 
                   )
# 28 "Parser.fsy"
                 : 'Expression));
# 281 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                                                                         New 
                   )
# 29 "Parser.fsy"
                 : 'Expression));
# 291 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                                                                         Use(_3) 
                   )
# 30 "Parser.fsy"
                 : 'Expression));
# 302 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                                                                         Fun([], List.rev(_6)) 
                   )
# 31 "Parser.fsy"
                 : 'Expression));
# 313 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                                                                         Fun([], [Return(_5)]) 
                   )
# 32 "Parser.fsy"
                 : 'Expression));
# 324 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                                                                         Add(_1, _3) 
                   )
# 33 "Parser.fsy"
                 : 'Expression));
# 336 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                                                                         Create(_2, _4) 
                   )
# 35 "Parser.fsy"
                 : 'Statement));
# 348 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                                                                                         Assign(_1, _3) 
                   )
# 36 "Parser.fsy"
                 : 'Statement));
# 360 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                                                         Return(_2) 
                   )
# 37 "Parser.fsy"
                 : 'Statement));
# 371 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Statement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                                         [_1] 
                   )
# 39 "Parser.fsy"
                 : 'StatementList));
# 382 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StatementList)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Statement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                                                                         _2::_1 
                   )
# 40 "Parser.fsy"
                 : 'StatementList));
|]
# 395 "Parser.fs"
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
    numTerminals = 26;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  AbstractSyntax.Prog  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
