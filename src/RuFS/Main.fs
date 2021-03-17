module Main

open Argu
open DrawTree

type CLIArguments =
    | ComputeFile of file:string
    | ComputeString of code:string
    | ToDotFile of file:string * output:string
    | ToDotString of code:string * output:string
    | ComputeAndDotFile of file:string * output:string
    | ComputeAndDotString of code:string * output:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ComputeFile _ -> "File to interpretate"
            | ComputeString _ -> "Calculate and return the result of given code" 
            | ToDotFile _ -> "Generates dot code from file which draws a tree of given arithmetic expression"
            | ToDotString _ -> "Generates dot code from given code which draws a tree of given arithmetic expression"
            | ComputeAndDotFile _ -> "Computes and generates dot code from file"
            | ComputeAndDotString _ -> "Computes and generates dot code from given code"

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
    | p when p.Contains(ComputeFile) ->
        let file = results.GetResult ComputeFile
        let ast = parse (System.IO.File.ReadAllText file)
        Interpreter.run ast
    | p when p.Contains(ComputeString) ->
        let expr = results.GetResult ComputeString
        let full_expr = parse expr
        printfn "%s" (Interpreter.calculate full_expr |> BigInt.bigIntToString)
    | p when p.Contains(ToDotFile) ->
        let file, output = results.GetResult ToDotFile
        let ast = parse (System.IO.File.ReadAllText file)
        drawTree ast output
    | p when p.Contains(ToDotString) ->
        let code, output = results.GetResult ToDotString
        drawTree (parse code) output
    | p when p.Contains(ComputeAndDotFile) ->
        let file, output = results.GetResult ComputeAndDotFile
        let ast = parse (System.IO.File.ReadAllText file)
        Interpreter.run ast
        drawTree ast output
    | p when p.Contains(ComputeAndDotString) ->
        let code, output = results.GetResult ComputeAndDotString
        let ast = parse code
        drawTree ast output
        Interpreter.run ast
    | _ -> parser.PrintUsage() |> printfn "%s"
    0
    
