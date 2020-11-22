module HW6

open System
open HW3

[<Measure>] type row
[<Measure>] type col

[<Struct>]
type rc =
    val R:int<row>
    val C:int<col>
    new(x, y) = {R = x; C = y}

[<Struct>]
type bMat =
    val R:int
    val C:int
    val L:list<rc>
    new (l, r, c) = {L = l; R = r; C = c}
 
let readBoolMat file =
    let r = System.IO.File.ReadAllLines file
    let m = Array.zeroCreate r.Length
    for i = 0 to r.Length - 1 do
        m.[i] <- (r.[i].Trim ' ').Split ' '

    for i in m do
        for j in i do
            if j <> "1" && j <> "0"
            then
                failwith "Matrix should contain only 0 or 1 values"

    Array.map (fun i -> Array.map int i) m

let writeMat file (mat: int [] []) =
    let mutable s = ""
    for r in mat do
        for c in r do
            s <- s + string c + " "
        s <- (s.Trim ' ') + "\n"
        
    System.IO.File.WriteAllText (file, s)

let boolMatToBMat (m:int [] []) =
    for i in m do
        for j in i do
            if j <> 1 && j <> 0
            then
                failwith "Matrix should contain only 0 or 1 values"
    if Array.isEmpty m
    then
        bMat([], 0, 0)
    else
        let l = [
            for i = 0 to m.Length - 1 do
                for j = 0 to m.[0].Length - 1 do
                    if m.[i].[j] = 1 then rc(i * 1<row>, j * 1<col>)
        ]
        bMat(l, m.Length, m.[0].Length)
    
let bMatToMat (bM:bMat) =
    let m = mat bM.R bM.C
    for rc in bM.L do
        m.[int rc.R].[int rc.C] <- 1
    m

let mulBMat (a1:bMat) (b1:bMat) =
    if a1.C <> b1.R
    then
        failwith "Wrong sized matrices given"
    else
        let l = [
            for f in a1.L do
                for g in b1.L do
                    if int g.R = int f.C then rc(f.R, g.C)
        ]
        bMat(List.distinct l, a1.R, b1.C)




