module Main

open Argu

type CLIArguments =
    | Input of file:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "File to interpret."

open System.Collections.Generic
open FSharp.Text.Lexing

let parse text =
    let lexbuf = LexBuffer<char>.FromString text
    let parsed = Parser.start Lexer.tokenStream lexbuf
    parsed

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Arithmetics interpreter")
    let results = parser.Parse(argv)
    if results.Contains Input
    then
        let file = results.GetResult Input
        let ast = parse (System.IO.File.ReadAllText file)
        Interpreter.run ast
    else
        parser.PrintUsage() |> printfn "%s"
    0
    
