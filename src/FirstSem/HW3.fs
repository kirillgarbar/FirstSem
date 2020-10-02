module HW3

let mulMat2x2 (a: int [] []) (b: int [] []) =
    let r = [| [| 0; 0 |]; [| 0; 0 |] |]
    r.[0].[0] <- a.[0].[0] * b.[0].[0] + a.[0].[1] * b.[1].[0]
    r.[0].[1] <- a.[0].[0] * b.[0].[1] + a.[0].[1] * b.[1].[1]
    r.[1].[0] <- a.[1].[0] * b.[0].[0] + a.[1].[1] * b.[1].[0]
    r.[1].[1] <- a.[1].[0] * b.[0].[1] + a.[1].[1] * b.[1].[1]
    r

let powMat2x2 (a: int [] []) p =
    let mutable r = a
    for i = 2 to p do
        r <- mulMat2x2 r a
    r
            
let rec fibRec n =
    if n < 0
    then
        failwith "Number must not be negative"
    elif n = 0 || n = 1
    then
        1
    else
        fibRec(n - 2) + fibRec(n - 1)

let fibIt n =
    if n < 0
    then
        failwith "Number must not be negative"
    elif n = 0 || n = 1
    then
        1
    else
        let mutable x = 0
        let mutable fib1 = 1
        let mutable fib2 = 1
        for i = 2 to n do
            x <- fib1 + fib2
            fib2 <- fib1
            fib1 <- x
        x

let fibTailRec n =
    if n < 0
    then
        failwith "Number must not be negative"
    else
        let rec go n a b =
            if n = 1 || n = 0
            then
                a
            else
                go (n - 1) (a + b) (a)
        go n 1 1

let naiveFibMatrix n =
    if n < 0
    then
        failwith "Number must not be negative"
    elif n = 0 || n = 1
    then
        1
    else
        let mutable m = [| [| 0; 1 |]; [| 1; 1 |] |]
        for i = 2 to n do
            m <- (mulMat2x2 m [| [| 0; 1 |]; [| 1; 1 |] |])
        m.[1].[1]

let logFibMatrix n =
    if n < 0
    then
        failwith "Number must not be negative"
    else
        let rec go n =
            if n = 0 || n = 1
            then
                [| [| 0; 1 |]; [| 1; 1 |] |]
            else
                if n % 2 = 0
                then
                    powMat2x2 (go (n/2)) 2
                else
                    mulMat2x2 ([| [| 0; 1 |]; [| 1; 1 |] |]) (powMat2x2 (go ((n - 1)/2)) 2)
        (go n).[1].[1]

let allFib n =
    if n < 0
    then
        failwith "Number must not be negative"
    else
        let a = Array.zeroCreate (n + 1)
        for i = 0 to n do
            a.[i] <- fibTailRec i
        a
