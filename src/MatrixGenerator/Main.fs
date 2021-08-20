namespace FirstSem

open Argu
open Generator
open System.IO

module Main =
    type CLIArguments =
        | Rows of int
        | Cols of int
        | Number of int 
        | Sparsity of int
        | Path of string
        | Type of GeneratorType
        interface IArgParserTemplate with 
            member s.Usage = "Generates matrices of a given type. Sparcity - a number from 0 to 100 which means percentage of zero elements."
    [<EntryPoint>]
    let main argv =
        try
            let p = ArgumentParser.Create<CLIArguments>(programName = "FirstSem").ParseCommandLine argv
            let generator =
                match p.GetResult(Type) with
                    | Int -> intGenerator
                    | Bool -> boolGenerator
                    | Float -> floatGenerator
            for i in 1..p.GetResult(Number) do
                let path = p.GetResult(Path) + string Path.DirectorySeparatorChar + string i + ".txt"
                let m = generateMatrix (p.GetResult(Rows)) (p.GetResult(Cols)) (p.GetResult(Sparsity)) generator
                writeMatrix path m
        with
        | ex -> printfn "%s" ex.Message
        0
