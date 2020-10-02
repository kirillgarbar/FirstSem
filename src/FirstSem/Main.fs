namespace FirstSem

open System

module Main =
    open Argu
    type CLIArguments =
        | CalculateNaively of x:int
        | CalculateShortly of x:int
        | IndicesLesserX of len:int
        | IndicesNotFromRangeXY of len:int
        | SwapFS
        | SwapTwoByIndex of len:int
        | FibRec of n:int
        | FibIt of n:int
        | FibTailRec of n:int
        | NaiveFibMatrix of n:int
        | LogFibMatrix of n:int
        | AllFib of n:int
        interface IArgParserTemplate with 
            member s.Usage =
                match s with
                | CalculateNaively _ -> "Calculates x^4+x^3+x^2+x^1+1 (Task 1)"
                | CalculateShortly _ -> "Calculates x^4+x^3+x^2+x^1+1 (Task 2)"
                | IndicesLesserX _ -> "Displays indices of array elements that are lesser than X, <len> stands for the length of the generated array"
                | IndicesNotFromRangeXY _ -> "Displays indices of array elements that are lesser than X or greater than Y, <len> stands for the length of the generated array"
                | SwapFS _ -> "Generates random array of two elements and swaps their positions"
                | SwapTwoByIndex _ -> "Generates random array of given length (<len>) and swaps two elements with indices i and j"
                | FibRec _ -> "Calculates fib number with index N by recursion"
                | FibIt _ -> "Calculates fib number with index N by iterations"
                | FibTailRec _ -> "Calculates fib number with index N by tail recursion"
                | NaiveFibMatrix _ -> "Calculates fib number with index N by naively multiplying matrices"
                | LogFibMatrix _ -> "Calculates fib number with index N by multiplying matrices for log2(N) operations"
                | AllFib _ -> "Displays all fib numbers with indices from 0 to N"
    [<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "FirstSem")
            let results = parser.Parse(argv)
            match parser.ParseCommandLine argv with
            | p when p.Contains(CalculateNaively) -> printfn "%i" (HW2.calculateNaively (p.GetResult(CalculateNaively)))
            | p when p.Contains(CalculateShortly) -> printfn "%i" (HW2.calculateShortly (p.GetResult(CalculateShortly)))
            | p when p.Contains(IndicesLesserX) ->
                let a = HW2.genRandomArray(p.GetResult(IndicesLesserX))
                printfn "%A" a
                printfn "Enter X: "
                let x = Console.ReadLine() |> int
                printfn "%A" (HW2.indicesLesserX(x, a))
            | p when p.Contains(IndicesNotFromRangeXY) ->
                let a = HW2.genRandomArray(p.GetResult(IndicesNotFromRangeXY))
                printfn "%A" a
                printfn "Enter X: "
                let x = Console.ReadLine() |> int
                printfn "Enter Y: "
                let y = Console.ReadLine() |> int
                printfn "%A" (HW2.indicesNotFromRangeXY(x, y, a))
            | p when p.Contains(SwapFS) ->
                let a = HW2.genRandomArray(2)
                printfn "%A" a
                printfn "%A" (HW2.swapFS(a))
            | p when p.Contains(SwapTwoByIndex) ->
                let a = HW2.genRandomArray(p.GetResult(SwapTwoByIndex))
                printfn "%A" a
                printfn "Enter i: "
                let i = Console.ReadLine() |> int
                printfn "Enter j: "
                let j = Console.ReadLine() |> int
                printfn "%A" (HW2.swapTwoByIndex(i, j, a))
            | p when p.Contains(FibRec) ->
                printfn "%A" (HW3.fibRec(p.GetResult(FibRec)))
            | p when p.Contains(FibIt) ->
                printfn "%A" (HW3.fibIt(p.GetResult(FibIt)))
            | p when p.Contains(FibTailRec) ->
                printfn "%A" (HW3.fibTailRec(p.GetResult(FibTailRec)))
            | p when p.Contains(NaiveFibMatrix) ->
                printfn "%A" (HW3.naiveFibMatrix(p.GetResult(NaiveFibMatrix)))
            | p when p.Contains(LogFibMatrix) ->
                printfn "%A" (HW3.logFibMatrix(p.GetResult(LogFibMatrix)))
            | p when p.Contains(AllFib) ->
                printfn "%A" (HW3.allFib(p.GetResult(AllFib)))
            | _ ->
                printfn "%s" (parser.PrintUsage())
            0
        with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
