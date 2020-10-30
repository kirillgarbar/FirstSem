namespace FirstSem

open HW4

module Main =
    open Argu
    type CLIArguments =
        | ArrayBubbleSort of input:string * output:string
        | ListBubbleSort of input:string * output:string
        | ArrayQuickSort of input:string * output:string
        | ListQuickSort of input:string * output:string
        | Pack32to64 of f:int * s:int
        | Unpack64to32 of i:int64
        | Pack16to64 of a:int16 * b:int16 * c:int16 * d:int16
        | Unpack64to16 of i:int64
        interface IArgParserTemplate with 
            member s.Usage =
                match s with
                | ArrayBubbleSort _ -> "Sorts the array in file with given path by bubble sorting and writes result in another given file"
                | ListBubbleSort _ -> "Sorts the list in file with given path by bubble sorting and writes result in another given file"
                | ArrayQuickSort _ -> "Sorts the array in file with given path by quick sorting and writes result in another given file"
                | ListQuickSort _ -> "Sorts the list in file with given path by bubble sorting and writes result in another given file"
                | Pack32to64 _ -> "Generates random array of two elements and swaps their positions"
                | Unpack64to32 _ -> "Generates random array of given length (<len>) and swaps two elements with indices i and j"
                | Pack16to64 _ -> "Calculates fib number with index N by recursion"
                | Unpack64to16 _ -> "Calculates fib number with index N by iterations"
    [<EntryPoint>]
    let main (argv: string array) =
        let sort sortFun readFun writeFun ioFiles =
            let i, o = fst ioFiles, snd ioFiles
            writeFun o (sortFun (readFun i))
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "FirstSem")
            let results = parser.Parse(argv)
            match parser.ParseCommandLine argv with
            | p when p.Contains(ArrayBubbleSort) ->
                sort arrayBubbleSort readArray writeArray (p.GetResult ArrayBubbleSort)
            | p when p.Contains(ListBubbleSort) ->
                sort listBubbleSort readList writeList (p.GetResult ListBubbleSort)
            | p when p.Contains(ArrayQuickSort) ->
                sort arrayQuickSort readArray writeArray (p.GetResult ArrayQuickSort)
            | p when p.Contains(ListQuickSort) ->
                sort listBubbleSort readList writeList (p.GetResult ListQuickSort)
            | p when p.Contains(Pack32to64) ->
                printfn "%A" (pack32To64 (p.GetResult Pack32to64))
            | p when p.Contains(Unpack64to32) ->
                printfn "%A" (unpack64To32 (p.GetResult Unpack64to32))
            | p when p.Contains(Pack16to64) ->
                printfn "%A" (pack16To64 (p.GetResult Pack16to64))
            | p when p.Contains(Unpack64to16) ->
                printfn "%A" (unpack64To16 (p.GetResult Unpack64to16))
            | _ ->
                printfn "%s" (parser.PrintUsage())
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
