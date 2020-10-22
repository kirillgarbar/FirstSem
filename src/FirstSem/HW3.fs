module HW3

let mat a b =
    if a < 0 || b < 0
    then
        failwith "Size of matrix must be positive"
    else
        let m = Array.init a (fun _ -> Array.create b 0)
        m

let fibMat =
    let m = mat 2 2
    m.[0].[1] <- 1
    m.[1].[0] <- 1
    m.[1].[1] <- 1
    m

let identMat n =
    let m = mat n n
    for i = 0 to n - 1 do
        m.[i].[i] <- 1
    m

let mulMat (a:int [] []) (b:int [] []) =
        if a.Length = 0 || b.Length = 0 || a.[0].Length = 0 || b.[0].Length = 0
        then
            failwith "Matrices should not be empty"
        elif a.[0].Length <> b.Length
        then
            failwith "The number of rows in a is not equal to the number of columns in b"       
        else
            let m = mat (a.Length) (b.[0].Length)
            for i = 0 to a.Length - 1 do
                for j = 0 to b.[0].Length - 1 do
                    for k = 0 to a.[0].Length - 1 do
                        m.[i].[j] <- m.[i].[j] + a.[i].[k] * b.[k].[j]
            m

let powMat (a:int [] []) p =
    if a.Length <> a.[0].Length
    then
        failwith "The number of rows is not equal to the number of columns"
    elif p < 0
    then
        failwith "Power should be positive"
    elif p = 0
    then
        identMat a.Length
    else
        let mutable r = a
        for i = 2 to p do
            r <- mulMat r a
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
        let m = fibMat
        (powMat m n).[1].[1]

let logFibMatrix n =
    if n < 0
    then
        failwith "Number must not be negative"
    else
        let rec go n =
            if n = 0 || n = 1
            then
                fibMat
            else
                if n % 2 = 0
                then
                    powMat (go (n/2)) 2
                else
                    mulMat (fibMat) (powMat (go ((n - 1)/2)) 2)
        (go n).[1].[1]

let allFib n =
    if n < 0
    then
        failwith "Number must not be negative"
    else
        let a = Array.create (n + 1) 1
        for i = 2 to n do
            a.[i] <- a.[i-1] + a.[i-2]
        a
        
