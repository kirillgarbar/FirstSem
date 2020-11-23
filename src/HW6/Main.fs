namespace FirstSem

open HW6
open Argu

module Main =
    open Argu
    type CLIArguments =
        | MultiplyBoolMatrices of input1:string * input2:string * output:string
        interface IArgParserTemplate with 
            member s.Usage =
                match s with
                | MultiplyBoolMatrices _ -> "Multiplies two bool matrices from input1 and input2 files"
    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "FirstSem")
            match parser.ParseCommandLine argv with
            | p when p.Contains(MultiplyBoolMatrices) ->
                let i1, i2, o = p.GetResult(MultiplyBoolMatrices)
                let m1 = readBMat i1
                let m2 = readBMat i2
                writeBMat o (mulBMat m1 m2)
            | _ ->
                printfn "%s" (parser.PrintUsage())
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1


