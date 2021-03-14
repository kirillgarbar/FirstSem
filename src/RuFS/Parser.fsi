// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | ABS
  | RBR
  | LBR
  | EQ
  | KW_PRINT
  | KW_LET
  | VNAME of (string)
  | BIN
  | POW
  | REM
  | DIV
  | MUL
  | SUB
  | SUM
  | NUM of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_ABS
    | TOKEN_RBR
    | TOKEN_LBR
    | TOKEN_EQ
    | TOKEN_KW_PRINT
    | TOKEN_KW_LET
    | TOKEN_VNAME
    | TOKEN_BIN
    | TOKEN_POW
    | TOKEN_REM
    | TOKEN_DIV
    | TOKEN_MUL
    | TOKEN_SUB
    | TOKEN_SUM
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_stmt
    | NONTERM_expr
    | NONTERM_rexpr
    | NONTERM_baseAndPow
    | NONTERM_power
    | NONTERM_vname
    | NONTERM_eof
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> ( AST.Program ) 
