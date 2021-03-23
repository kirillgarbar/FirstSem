module Main

open Argu
open DrawTree

type CLIArguments =
    | InputFile of file:string
    | InputString of code:string
    | Compute
    | ToDot of output:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputFile _ -> "File with code"
            | InputString _ -> "String of code" 
            | Compute -> "Return the result of interptetation of given code"
            | ToDot _ -> "Generates dot code of syntax tree to the given file"

open FSharp.Text.Lexing

let parse text =
    let lexbuf = LexBuffer<char>.FromString text
    let parsed = Parser.start Lexer.tokenStream lexbuf
    parsed

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Arithmetics interpreter")
    let results = parser.Parse(argv)
    let p = parser.ParseCommandLine argv
    if argv.Length = 0 || results.IsUsageRequested then parser.PrintUsage() |> printfn "%s"
    else 
        let input =
            if p.Contains(InputFile) then System.IO.File.ReadAllText (results.GetResult InputFile)
            elif p.Contains(InputString) then results.GetResult InputString
            else failwith "No input code given"
        let ast = parse input
        if p.Contains(Compute) then Interpreter.run ast
        if p.Contains(ToDot) then drawTree ast (results.GetResult ToDot)
    0
    
