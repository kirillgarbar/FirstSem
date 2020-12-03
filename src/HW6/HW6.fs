module HW6

[<Measure>] type row
[<Measure>] type col

[<Struct>]
type row_col =
    val Row:int<row>
    val Col:int<col>
    new(x, y) = {Row = x; Col = y}

[<Struct>]
type boolMat =
    val Rows:int
    val Cols:int
    val Coordinates:list<row_col>
    new (l, r, c) = {Coordinates = l; Rows = r; Cols = c}
 
let readBoolMat file =
    let m = Array.map (fun (i:string) -> (i.Trim ' ').Split ' ') (System.IO.File.ReadAllLines file)

    if Array.isEmpty m
    then
        boolMat([], 0, 0)
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
                    if m.[i].[j] = "1" then row_col(i * 1<row>, j * 1<col>)
        ]
        boolMat(l, m.Length, m.[0].Length)

let writeBoolMat file (m: boolMat) =
    let boolMatToMat (bM: boolMat) =
        let m = Array.init bM.Rows (fun _ -> Array.create bM.Cols "0")
        for rc in bM.Coordinates do
            m.[int rc.Row].[int rc.Col] <- "1"
        m

    let m = Array.fold (fun s i -> s + (Array.fold (fun s1 j -> s1 + string j + " ") "" i) + "\n") "" (boolMatToMat m)
    System.IO.File.WriteAllText (file, m)
    
let mulBoolMat (a1:boolMat) (b1:boolMat) =
    if a1.Cols <> b1.Rows
    then
        failwith "Wrong sized matrices given"
    else
        let l = [
            for f in a1.Coordinates do
                for g in b1.Coordinates do
                    if int g.Row = int f.Col then row_col(f.Row, g.Col)
        ]
        boolMat(List.distinct l, a1.Rows, b1.Cols)




