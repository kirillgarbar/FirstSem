namespace FirstSem

open Argu
open Generator
open System.IO

module Main =
    type CLIArguments =
        | Rows of int
        | Cols of int
        | Number of int 
        | Sparcity of int
        | Path of string
        | Int
        | Bool
        | Float
        interface IArgParserTemplate with 
            member s.Usage = "Generates matrices of a given type. Sparcity - a number from 0 to 100 which means percentage of zero elements."
    [<EntryPoint>]
    let main argv =
        try
            let p = ArgumentParser.Create<CLIArguments>(programName = "FirstSem").ParseCommandLine argv
            let generator = if p.Contains(Int) then intGenerator elif p.Contains(Bool) then boolGenerator elif p.Contains(Float) then floatGenerator else failwith "You must enter a type"
            for i in 1..p.GetResult(Number) do
                let path = p.GetResult(Path) + string Path.DirectorySeparatorChar + string i + ".txt"
                let m = generateMatrix (p.GetResult(Rows)) (p.GetResult(Cols)) (p.GetResult(Sparcity)) generator
                writeMatrix path m
        with
        | ex -> printfn "%s" ex.Message
        0
