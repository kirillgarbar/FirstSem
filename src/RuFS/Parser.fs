// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 0 " "

# 8 "Parser.fs"
// This type is the type of tokens accepted by the parser
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
// This type is used to give symbolic names to token indexes, useful for error messages
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
// This type is used to give symbolic names to token indexes, useful for error messages
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

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | ABS  -> 1 
  | RBR  -> 2 
  | LBR  -> 3 
  | EQ  -> 4 
  | KW_PRINT  -> 5 
  | KW_LET  -> 6 
  | VNAME _ -> 7 
  | BIN  -> 8 
  | POW  -> 9 
  | REM  -> 10 
  | DIV  -> 11 
  | MUL  -> 12 
  | SUB  -> 13 
  | SUM  -> 14 
  | NUM _ -> 15 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_ABS 
  | 2 -> TOKEN_RBR 
  | 3 -> TOKEN_LBR 
  | 4 -> TOKEN_EQ 
  | 5 -> TOKEN_KW_PRINT 
  | 6 -> TOKEN_KW_LET 
  | 7 -> TOKEN_VNAME 
  | 8 -> TOKEN_BIN 
  | 9 -> TOKEN_POW 
  | 10 -> TOKEN_REM 
  | 11 -> TOKEN_DIV 
  | 12 -> TOKEN_MUL 
  | 13 -> TOKEN_SUB 
  | 14 -> TOKEN_SUM 
  | 15 -> TOKEN_NUM 
  | 18 -> TOKEN_end_of_input
  | 16 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_start 
    | 3 -> NONTERM_stmt 
    | 4 -> NONTERM_stmt 
    | 5 -> NONTERM_expr 
    | 6 -> NONTERM_expr 
    | 7 -> NONTERM_expr 
    | 8 -> NONTERM_rexpr 
    | 9 -> NONTERM_rexpr 
    | 10 -> NONTERM_rexpr 
    | 11 -> NONTERM_rexpr 
    | 12 -> NONTERM_baseAndPow 
    | 13 -> NONTERM_baseAndPow 
    | 14 -> NONTERM_power 
    | 15 -> NONTERM_power 
    | 16 -> NONTERM_power 
    | 17 -> NONTERM_power 
    | 18 -> NONTERM_power 
    | 19 -> NONTERM_vname 
    | 20 -> NONTERM_eof 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 18 
let _fsyacc_tagOfErrorTerminal = 16

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | ABS  -> "ABS" 
  | RBR  -> "RBR" 
  | LBR  -> "LBR" 
  | EQ  -> "EQ" 
  | KW_PRINT  -> "KW_PRINT" 
  | KW_LET  -> "KW_LET" 
  | VNAME _ -> "VNAME" 
  | BIN  -> "BIN" 
  | POW  -> "POW" 
  | REM  -> "REM" 
  | DIV  -> "DIV" 
  | MUL  -> "MUL" 
  | SUB  -> "SUB" 
  | SUM  -> "SUM" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | ABS  -> (null : System.Object) 
  | RBR  -> (null : System.Object) 
  | LBR  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | KW_PRINT  -> (null : System.Object) 
  | KW_LET  -> (null : System.Object) 
  | VNAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BIN  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | REM  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MUL  -> (null : System.Object) 
  | SUB  -> (null : System.Object) 
  | SUM  -> (null : System.Object) 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 2us; 65535us; 0us; 1us; 2us; 3us; 2us; 65535us; 0us; 2us; 2us; 2us; 4us; 65535us; 8us; 9us; 28us; 11us; 30us; 12us; 34us; 13us; 6us; 65535us; 8us; 10us; 14us; 15us; 16us; 17us; 28us; 10us; 30us; 10us; 34us; 10us; 9us; 65535us; 8us; 18us; 14us; 18us; 16us; 18us; 19us; 20us; 21us; 22us; 23us; 24us; 28us; 18us; 30us; 18us; 34us; 18us; 10us; 65535us; 8us; 25us; 14us; 25us; 16us; 25us; 19us; 25us; 21us; 25us; 23us; 25us; 26us; 27us; 28us; 25us; 30us; 25us; 34us; 25us; 12us; 65535us; 4us; 5us; 6us; 7us; 8us; 33us; 14us; 33us; 16us; 33us; 19us; 33us; 21us; 33us; 23us; 33us; 26us; 33us; 28us; 33us; 30us; 33us; 34us; 33us; 0us; 65535us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 4us; 7us; 12us; 19us; 29us; 40us; 53us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 4us; 3us; 4us; 6us; 7us; 4us; 5us; 9us; 10us; 11us; 3us; 6us; 7us; 14us; 3us; 6us; 7us; 15us; 3us; 6us; 7us; 18us; 1us; 6us; 4us; 6us; 9us; 10us; 11us; 1us; 7us; 4us; 7us; 9us; 10us; 11us; 2us; 8us; 13us; 1us; 9us; 2us; 9us; 13us; 1us; 10us; 2us; 10us; 13us; 1us; 11us; 2us; 11us; 13us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 15us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 11us; 13us; 15us; 17us; 19us; 23us; 28us; 32us; 36us; 40us; 42us; 47us; 49us; 54us; 57us; 59us; 62us; 64us; 67us; 69us; 72us; 74us; 76us; 78us; 80us; 82us; 84us; 86us; 88us; 90us; 92us; |]
let _fsyacc_action_rows = 36
let _fsyacc_actionTableElements = [|2us; 32768us; 5us; 4us; 6us; 6us; 0us; 49152us; 2us; 16385us; 5us; 4us; 6us; 6us; 0us; 16386us; 1us; 32768us; 7us; 35us; 0us; 16387us; 1us; 32768us; 7us; 35us; 1us; 32768us; 4us; 8us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 2us; 16388us; 13us; 16us; 14us; 14us; 3us; 16389us; 10us; 23us; 11us; 21us; 12us; 19us; 3us; 32768us; 2us; 29us; 13us; 16us; 14us; 14us; 3us; 32768us; 1us; 31us; 13us; 16us; 14us; 14us; 2us; 16402us; 13us; 16us; 14us; 14us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 3us; 16390us; 10us; 23us; 11us; 21us; 12us; 19us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 3us; 16391us; 10us; 23us; 11us; 21us; 12us; 19us; 1us; 16392us; 9us; 26us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 1us; 16393us; 9us; 26us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 1us; 16394us; 9us; 26us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 1us; 16395us; 9us; 26us; 0us; 16396us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 0us; 16397us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 0us; 16398us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 0us; 16399us; 0us; 16400us; 0us; 16401us; 5us; 32768us; 1us; 30us; 3us; 28us; 7us; 35us; 8us; 34us; 15us; 32us; 0us; 16403us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 3us; 4us; 7us; 8us; 10us; 11us; 13us; 15us; 21us; 24us; 28us; 32us; 36us; 39us; 45us; 49us; 55us; 59us; 61us; 67us; 69us; 75us; 77us; 83us; 85us; 86us; 92us; 93us; 99us; 100us; 106us; 107us; 108us; 109us; 115us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 2us; 2us; 4us; 1us; 3us; 3us; 1us; 3us; 3us; 3us; 1us; 3us; 3us; 3us; 1us; 1us; 2us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 2us; 2us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 5us; 5us; 6us; 6us; 6us; 6us; 6us; 7us; 8us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16386us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16396us; 65535us; 16397us; 65535us; 16398us; 65535us; 16399us; 16400us; 16401us; 65535us; 16403us; |]
let _fsyacc_reductions ()  =    [| 
# 181 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  AST.Program )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 190 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "Parser.fsy"
                                  [_1]
                   )
# 25 "Parser.fsy"
                 :  AST.Program ));
# 201 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'stmt)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data :  AST.Program )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                        _1 :: _2
                   )
# 26 "Parser.fsy"
                 :  AST.Program ));
# 213 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'vname)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                           AST.Print(_2)
                   )
# 28 "Parser.fsy"
                 : 'stmt));
# 224 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'vname)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                                 AST.VDecl(_2,_4)
                   )
# 29 "Parser.fsy"
                 : 'stmt));
# 236 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                  _1
                   )
# 31 "Parser.fsy"
                 : 'expr));
# 247 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                           AST.Sum(_1, _3)
                   )
# 32 "Parser.fsy"
                 : 'expr));
# 259 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                           AST.Sub(_1, _3)
                   )
# 33 "Parser.fsy"
                 : 'expr));
# 271 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'baseAndPow)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                        _1
                   )
# 35 "Parser.fsy"
                 : 'rexpr));
# 282 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'baseAndPow)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                                                 AST.Mul(_1, _3)
                   )
# 36 "Parser.fsy"
                 : 'rexpr));
# 294 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'baseAndPow)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                 AST.Div(_1, _3)
                   )
# 37 "Parser.fsy"
                 : 'rexpr));
# 306 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'baseAndPow)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                 AST.Rem(_1, _3)
                   )
# 38 "Parser.fsy"
                 : 'rexpr));
# 318 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'power)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                        _1
                   )
# 40 "Parser.fsy"
                 : 'baseAndPow));
# 329 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'baseAndPow)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'power)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                 AST.Pow(_1, _3)
                   )
# 41 "Parser.fsy"
                 : 'baseAndPow));
# 341 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                          _2
                   )
# 43 "Parser.fsy"
                 : 'power));
# 352 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                         AST.Abs _2
                   )
# 44 "Parser.fsy"
                 : 'power));
# 363 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                AST.Num(BigInt.stringToBigInt _1)
                   )
# 45 "Parser.fsy"
                 : 'power));
# 374 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'vname)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                  AST.NVar(_1)
                   )
# 46 "Parser.fsy"
                 : 'power));
# 385 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                     AST.Bin _2
                   )
# 47 "Parser.fsy"
                 : 'power));
# 396 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                   AST.Var(_1)
                   )
# 49 "Parser.fsy"
                 : 'vname));
# 407 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                               1
                   )
# 51 "Parser.fsy"
                 : 'eof));
|]
# 418 "Parser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
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
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 19;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  AST.Program  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
