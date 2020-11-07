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
    readArray file |> Array.toList

let write file content =
    try
        System.IO.File.WriteAllText (file, content)
    with
    | :? System.ArgumentException ->
        failwith "Empty file path given"
    | :? System.IO.IOException ->
        failwith "Invalid file name"

let writeArray file (content:array<int>) =
    let mutable s = ""
    for i = 0 to content.Length - 1 do
        s <- s + string content.[i] + "\n"
    write file s

let writeList file (content: list<int>) =
    writeArray file (List.toArray content)

let arrayBubbleSort (a:array<int>) =
    let m = Array.copy a
    for i = 0 to m.Length - 2 do
        for j = 0 to m.Length - 2 do
            if m.[j] > m.[j + 1]
            then
                let c = m.[j]
                m.[j] <- m.[j + 1]
                m.[j + 1] <- c
    m

let listBubbleSort (l:list<int>) =
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

    let rec go2 (l:list<int>) c =
        if c = l.Length
        then
            l
        else
            go2 (go l) (c + 1)

    go2 l 0

let rec arrayQSort (a:array<int>) =
    if a.Length <= 1
    then
        a
    else
        let pivot = a.[a.Length / 2]
        let pivots, lr = Array.partition(fun i -> i = pivot) a
        let left, right = Array.partition(fun i -> i < pivot) lr
        Array.append (Array.append (arrayQSort left) pivots) (arrayQSort right)

let arrayQuickSort (a:array<int>) =
    let swap (a:array<int>) i j =
        let c = a.[i]
        a.[i] <- a.[j]
        a.[j] <- c

    let partition (a:array<int>) l r =
        let pivot = a.[r]
        let mutable i = l - 1
        for j = l to r - 1 do
            if a.[j] <= pivot
            then
                i <- i + 1
                swap a i j
        swap a (i + 1) r
        i + 1

    let rec go (a:array<int>) l r =
        if l < r
        then
            let pi = partition a l r
            go a l (pi - 1)
            go a (pi + 1) r

    let m = Array.copy a
    go m 0 (a.Length - 1)
    m
    
let rec listQuickSort(l:list<int>) =
    match l with
    | [] -> []
    | pivot :: tail ->
        let left, right = List.partition(fun i -> i < pivot) tail
        (listQuickSort left) @ (pivot :: (listQuickSort right))

let pack32To64 (a, b) =
    if b >= 0
    then
        (a |> int64 <<< 32) + (b |> int64)
    else
        (a |> int64 <<< 32) + 4294967296L + (b |> int64)

let pack16To32 (a:int16, b:int16) =
    if b >= 0s
    then
        (int32 a <<< 16) + (int32 b)
    else
        (int32 a <<< 16) + 65536 + (int32 b)

let pack16To64 (a:int16, b:int16, c:int16, d:int16) =
    pack32To64 (pack16To32 (a, b), pack16To32 (c, d))

let unpack64To32 (a:int64) =
    (a >>> 32 |> int, (a <<< 32) >>> 32 |> int)

let unpack32To16 (a:int) =
    (a >>> 16 |> int16, (a <<< 16) >>> 16 |> int16)

let unpack64To16 (a:int64) =
    let ab, cd = unpack64To32 a
    let a, b = unpack32To16 ab
    let c, d = unpack32To16 cd
    (a, b, c, d)

