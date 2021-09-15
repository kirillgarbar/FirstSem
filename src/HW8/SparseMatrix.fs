module SparseMatrix

type CRV<'t> =
    val Col:int
    val Row:int
    val Value:'t

    new(c, r, v) = {
        Col = if r < 0 then failwith "Positive number expected" else c
        Row = if c < 0 then failwith "Positive number expected" else r
        Value = v }

type SparseMatrix<'t> =
    val Cols:int
    val Rows:int
    val List:list<CRV<'t>>

    new(c, r, l) = {
        Cols = c
        Rows = r
        List = if (List.fold (fun _ (x:CRV<'t>) -> x.Row < r && x.Col < c) true l) then l else failwith "Index is out of bounds" }
