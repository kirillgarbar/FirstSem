module SparseMatrix

let roundToPowerOfTwo x =
    let rec go r = if pown 2 r < x then go (r + 1) else r

    pown 2 (go 0)

type CRV<'t> =
    val Col:int
    val Row:int
    val Value:'t

    new(c, r, v) = {
        Col = if r < 0 then failwith "Positive number expected" else c
        Row = if c < 0 then failwith "Positive number expected" else r
        Value = v }

type SparseMatrix<'t> =
    val Size:int
    val List:list<CRV<'t>>

    new(s, l) = {
        Size = roundToPowerOfTwo s
        List = if (List.fold (fun _ (x:CRV<'t>) -> x.Row < (roundToPowerOfTwo s) && x.Col < (roundToPowerOfTwo s)) true l) then l else failwith "Index is out of bounds" }
