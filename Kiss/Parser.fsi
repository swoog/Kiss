// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | DESINCR
  | AND
  | DOT
  | EQUAL
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
  | TRUE
  | FALSE
  | STRING of (string)
  | INT of (int)
  | NAME of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_DESINCR
    | TOKEN_AND
    | TOKEN_DOT
    | TOKEN_EQUAL
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
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_STRING
    | TOKEN_INT
    | TOKEN_NAME
    | TOKEN_end_of_input
    | TOKEN_error
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
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( AbstractSyntax.Prog ) 
