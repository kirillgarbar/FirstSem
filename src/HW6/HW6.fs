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
    let m = Array.fold (fun (bm, row, len) (str:string) ->
        let line = ((str.Trim ' ').Split ' ')
        let l = [
            for i = 0 to line.Length - 1 do
                if line.Length <> len && row > 0 then failwith "Matrix should me rectangular"
                elif line.[i] = "1" then row_col(row * 1<row>, i * 1<col>)
                elif line.[i] <> "0" then failwith "Matrix should contain only 0 or 1 values"
        ]
        (bm @ l, row + 1, line.Length)) ([], 0, 0) (System.IO.File.ReadAllLines file)
    match m with
    | list, rows, cols -> boolMat(list, rows, cols)

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




