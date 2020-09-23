module HW2

open System

let genRandomArray count =
    let rnd = System.Random()
    Array.init count (fun _ -> rnd.Next(100))

let calculateNaively x =
    x*x*x*x + x*x*x + x*x + x + 1

let calculateShortly x =
    let r = x*x
    let t = r + x
    r*t + t + 1

let indicesLesserX (x, a:array<int>) =
    let mutable c = 0
    for i = 0 to a.Length - 1 do
        if a.[i] < x then c <- c + 1
    let r = Array.zeroCreate c
    c <- 0
    for i = 0 to a.Length - 1 do
        if a.[i] < x then
            r.[c] <- i
            c <- c + 1
    r

let indicesNotFromRangeXY (x, y, a:array<int>) =
    let mutable c = 0
    for i = 0 to a.Length - 1 do
        if a.[i] < x || a.[i] > y then c <- c + 1
    let r = Array.zeroCreate c
    c <- 0
    for i = 0 to a.Length - 1 do
        if a.[i] < x || a.[i] > y then
            r.[c] <- i
            c <- c + 1
    r

let swapFS (a:array<int>) =
    try
        a.[0] <- a.[0] + a.[1]
        a.[1] <- a.[0] - a.[1]
        a.[0] <- a.[0] - a.[1]
        a
    with
    | :? IndexOutOfRangeException as ex ->
        printfn "%s" ex.Message
        a


