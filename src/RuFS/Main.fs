module Main

open Argu

type CLIArguments =
    | Input of file:string
    | Calculate of expr:string
    | GetTree of expr:string * output:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "File to interpretate"
            | Calculate _ -> "Arithmetic expression to calculate and return the result"
            | GetTree _ -> "Generates dot code which draws a tree of given arithmetic expression"

open FSharp.Text.Lexing

let parse text =
    let lexbuf = LexBuffer<char>.FromString text
    let parsed = Parser.start Lexer.tokenStream lexbuf
    parsed

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Arithmetics interpreter")
    let results = parser.Parse(argv)
    match parser.ParseCommandLine argv with
    | p when p.Contains(Input) ->
        let file = results.GetResult Input
        let ast = parse (System.IO.File.ReadAllText file)
        Interpreter.run ast
    | p when p.Contains(Calculate) ->
        let expr = results.GetResult Calculate
        let full_expr = parse ("let [x] = " + expr)
        printfn "%s" (Interpreter.calculate full_expr |> BigInt.bigIntToString)
    | p when p.Contains(GetTree) ->
        let expr, output = results.GetResult GetTree
        let full_expr = parse ("let [x] = " + expr)
        Interpreter.drawTree full_expr.[0] output
    | _ -> parser.PrintUsage() |> printfn "%s"
    0
    
