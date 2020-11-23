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
 
let readBMat file =
    let m = Array.map (fun (i:string) -> (i.Trim ' ').Split ' ') (System.IO.File.ReadAllLines file)

    if Array.isEmpty m
    then
        bMat([], 0, 0)
    else
        let len = m.[0].Length
        for i = 0 to m.Length - 1 do
            if m.[i].Length <> len
            then
                failwith "Matrix should me rectangular"
            for j in m.[i] do
                if j <> "1" && j <> "0"
                then
                    failwith "Matrix should contain only 0 or 1 values"

        let l = [
            for i = 0 to m.Length - 1 do
                for j = 0 to m.[0].Length - 1 do
                    if m.[i].[j] = "1" then rc(i * 1<row>, j * 1<col>)
        ]
        bMat(l, m.Length, m.[0].Length)

let writeBMat file (m: bMat) =
    let bMatToMat (bM:bMat) =
        let m = Array.init bM.R (fun _ -> Array.create bM.C "0")
        for rc in bM.L do
            m.[int rc.R].[int rc.C] <- "1"
        m

    let m = Array.fold (fun s i -> s + (Array.fold (fun s1 j -> s1 + string j + " ") "" i) + "\n") "" (bMatToMat m)
    System.IO.File.WriteAllText (file, m)

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




