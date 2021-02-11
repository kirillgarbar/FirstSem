module SparseMatrix

type RCV<'t> =
    val Row:int
    val Col:int
    val Value:'t

    new(r, c, v) = {
        Row = if r < 0 then failwith "Positive number expected" else r
        Col = if c < 0 then failwith "Positive number expected" else c
        Value = v }

type SparseMatrix<'t> =
    val Size:int * int
    val List:list<RCV<'t>>

    new(s, l) = {
        Size = s
        List = if (List.fold (fun _ (x:RCV<'t>) -> x.Row < fst s && x.Col < snd s) true l) then l else failwith "Index is outside the bounds" }
