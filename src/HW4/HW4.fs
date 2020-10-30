module HW4

let readArray file =
    try
        let f = System.IO.File.ReadAllLines file
        let a = Array.zeroCreate (f.Length)
        for i = 0 to f.Length - 1 do
            a.[i] <- int (f.[i].Trim())
        a
    with
        | :? System.IO.FileNotFoundException ->
            failwith "Given file has not been found"
        | :? System.IO.IOException ->
            failwith "Invalid file name"
        | :? System.ArgumentException ->
            failwith "Empty file path given"

let readList file =
    try
        let f = System.IO.File.ReadAllLines file
        let mutable l = []
        for i = 0 to f.Length - 1 do
            l <- l @ [int (f.[i].Trim())]
        l
    with
    | :? System.IO.FileNotFoundException ->
        failwith "Given file has not been found"
    | :? System.IO.IOException ->
        failwith "Invalid file name"
    | :? System.ArgumentException ->
        failwith "Empty file path given"

let write file content =
    try
        let a = System.IO.File.WriteAllText (file, content)
        a
    with
    | :? System.ArgumentException ->
        failwith "Empty file path given"

let writeArray file (content:array<int>) =
    try
        let mutable s = ""
        for i = 0 to content.Length - 1 do
            s <- s + string content.[i] + "\n"
        write file s
    with
    | :? System.ArgumentException ->
        failwith "Empty file path given"

let writeList file (content:list<int>) =
    try
        let rec go (l:list<int>) s =
            match l with
            | [] -> s
            | head::tail -> go tail (s + string head + "\n")
        write file (go content "")
    with
    | :? System.ArgumentException ->
        failwith "Empty file path given"

let arrayBubbleSort (a:array<int>) =
    for i = 0 to a.Length - 2 do
        for j = 0 to a.Length - 2 do
            if a.[j] > a.[j + 1]
            then
                let c = a.[j]
                a.[j] <- a.[j + 1]
                a.[j + 1] <- c
    a

let listBubbleSort (l:list<int>) =
    let mutable r = l
    let rec go (l:list<int>) =
        match l with
        | [] -> []
        | x :: y :: tail ->
            if x > y
            then
                y :: (go(x :: tail))
            else
                x :: (go(y :: tail))
        | x :: tail -> [x]
    for i = 0 to l.Length - 1 do
        r <- go r
    r

let rec arrayQuickSort (a:array<int>) =
    if a.Length <= 1
    then
        a
    else
        let mutable c = 0
        let mutable j = 0
        let p = a.Length / 2
        for i = 0 to a.Length - 1 do
            if a.[i] < a.[p]
            then
                c <- c + 1
        let m1 = Array.zeroCreate (c)
        let m2 = Array.zeroCreate (a.Length - c - 1)
        c <- 0
        for i = 0 to a.Length - 1 do
            if i <> p
            then
                if a.[i] < a.[p]
                then
                    m1.[c] <- a.[i]
                    c <- c + 1
                else
                    m2.[j] <- a.[i]
                    j <- j + 1
        Array.append(Array.append (arrayQuickSort m1) [| a.[p] |]) (arrayQuickSort m2)

let rec listQuickSort (l:list<int>) =
    if l.Length <= 1
    then
        l
    else
        let mutable l1 = []
        let mutable l2 = []
        let p = l.Length / 2
        for i = 0 to l.Length - 1 do
            if i <> p
            then
                if l.[i] < l.[p]
                then
                    l1 <- l1 @ [l.[i]]
                else
                    l2 <- l2 @ [l.[i]]
        (listQuickSort l1) @ [l.[p]] @ (listQuickSort l2)

let pack32To64 (a, b) =
    if b >= 0
    then
        (a |> int64 <<< 32) + (b |> int64)
    else
        (a |> int64 <<< 32) + 4294967296L + (b |> int64)

let pack16To32 (a:int16,b:int16) =
    if b >= 0s
    then
        (a |> int32 <<< 16) + (b |> int32)
    else
        (a |> int32 <<< 16) + 65536 + (b |> int32)

let pack16To64 (a:int16, b:int16, c:int16, d:int16) =
    pack32To64 (pack16To32 (a, b), pack16To32 (c, d))

let unpack64To32 (a:int64) =
    (a >>> 32 |> int, (a <<< 32) >>> 32 |> int)

let unpack32To16 (a:int) =
    (a >>> 16 |> int16, (a <<< 16) >>> 16 |> int16)

let unpack64To16 (a:int64) =
    let abcd = unpack64To32 a
    let ab = unpack32To16 (fst abcd)
    let cd = unpack32To16 (snd abcd)
    (fst ab, snd ab, fst cd, snd cd)


