module QuadTreeMatrix

open SparseMatrix
open AlgebraicStruct

let roundToPowerOfTwo x =
    let rec go r = if pown 2 r < x then go (r + 1) else r

    pown 2 (go 0)

let getSquaredSize cols rows = roundToPowerOfTwo (max cols rows)

type Direction =
    | NW
    | NE
    | SE
    | SW

type QuadTree<'t when 't : equality> =
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

    member this.placeTree (tree:QuadTree<'t>) (dir:Direction) =
        match this with
        | Node(nw, ne, se, sw) ->
            match dir with
            | NW -> Node(tree, ne, se, sw)
            | NE -> Node(nw, tree, se, sw)
            | SE -> Node(nw, ne, tree, sw)
            | SW -> Node(nw, ne, se, tree)
        | None ->
            match dir with
            | NW -> Node(tree, None, None, None)
            | NE -> Node(None, tree, None, None)
            | SE -> Node(None, None, tree, None)
            | SW -> Node(None, None, None, tree)
        | _ -> failwith "Node or None expected"

let private getTree (node:QuadTree<'t>) (dir:Direction) =
        match node with
        | Node(nw, ne, se, sw) ->
            match dir with
            | NW -> nw
            | NE -> ne
            | SE -> se
            | SW -> sw
        | None -> None 
        | _ -> failwith "Node or None expected"

let private getDirection center colRow =
    match center with
    | xNode, yNode ->
        match colRow with
        | x, y when xNode > x && yNode > y -> NW
        | x, y when xNode <= x && yNode > y -> NE
        | x, y when xNode <= x && yNode <= y -> SE
        | x, y when xNode > x && yNode <= y -> SW
        | _, _ -> failwith "Impossible case"

let private getNewCenter center (dir:Direction) squaredSize =
    match center with
    | x, y ->
        match dir with
        | NW -> (x - squaredSize / 4, y - squaredSize / 4)
        | NE -> (x + squaredSize / 4, y - squaredSize / 4)
        | SE -> (x + squaredSize / 4, y + squaredSize / 4)
        | SW -> (x - squaredSize / 4, y  + squaredSize / 4)

let private matchTrees nw ne se sw =
    match nw, ne, se, sw with
    | None, None, None, None -> None
    | _, _, _, _ -> Node(nw, ne, se, sw)

type QuadTreeMatrix<'t when 't : equality> =
    val Cols:int
    val Rows:int
    val SquaredSize:int
    val AlgebraicStruct:AlgebraicStruct<'t>
    val mutable Tree:QuadTree<'t>

    new(c, r, a, t) = { Cols = c; Rows = r; SquaredSize = getSquaredSize c r ; AlgebraicStruct = a; Tree = t }

    override this.GetHashCode() =
        hash (this.Cols, this.Rows, this.Tree)

    override this.Equals(t) =
        match t with
        | :? QuadTreeMatrix<'t> as t -> this.Tree = t.Tree && this.Cols = t.Cols && this.Rows = t.Rows
        | _ -> false

    member this.Item
        with get (col, row) =
            let rec go center m squaredSize =
                match m with
                | None -> None
                | Leaf _ -> m
                | Node _ ->
                    let dir = getDirection center (col, row)
                    go (getNewCenter center dir squaredSize) (getTree m dir) (squaredSize / 2)

            if col >= this.Cols || row >= this.Rows then failwith "Index is out of bounds"
            else go (this.SquaredSize/2, this.SquaredSize/2) this.Tree this.SquaredSize
        and set (col, row) value =
            let rec go center m currSize =           
                match currSize with
                | 1 -> Leaf value
                | _ ->
                    let dir = getDirection center (col, row)
                    let newCenter = getNewCenter center dir currSize
                    let next = getTree m dir
                    m.placeTree (go newCenter next (currSize / 2)) dir
                   
            if col >= this.Cols || row >= this.Rows then failwith "Index is out of bounds"
            else this.Tree <- go (this.SquaredSize/2, this.SquaredSize/2) this.Tree this.SquaredSize

let initQTM algStr (m:SparseMatrix<'t>) =
    if m.List.Length = 0 then QuadTreeMatrix(m.Cols, m.Rows, algStr, None)
    else
        let matrix = QuadTreeMatrix(m.Cols, m.Rows, algStr, Node(None, None, None, None))
        List.iter (fun (crv:CRV<'t>) -> matrix.[crv.Col, crv.Row] <- crv.Value) m.List
        matrix

let sum (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) =
    let sumOp, neutral = m1.AlgebraicStruct.getSumOp, m1.AlgebraicStruct.getNeutral
    let rec go m1 m2 = 
        match m1, m2 with
        | Leaf x , Leaf y ->
            let r = sumOp x y
            if r = neutral then None else Leaf r
        | x, None -> x
        | None, x -> x
        | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
            let nw = go nw1 nw2 
            let ne = go ne1 ne2 
            let se = go se1 se2 
            let sw = go sw1 sw2 
            matchTrees nw ne se sw
        | _, _ -> failwith "Different sizes of matrices"

    if (m1.Cols, m1.Rows) <> (m2.Cols, m2.Rows)
    then
        failwith "Wrong size of matrices"
    else if m1.AlgebraicStruct <> m2.AlgebraicStruct
    then
        failwith "Algebraic structures must be the same"
    else
        QuadTreeMatrix(m1.Cols, m1.Rows, m1.AlgebraicStruct, go m1.Tree m2.Tree)

let mul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) =
    let reduceToSize tree neededSize currentSize =
        let rec go tree currentSize =
            if neededSize = currentSize
            then
                tree
            else
                go (getTree tree NW) (currentSize / 2)
    
        if neededSize >= currentSize then tree
        else go tree currentSize
    
    let increaseToSize tree neededSize currentSize =
        let rec go tree currentSize =
            if neededSize = currentSize
            then
                tree
            else
                go (Node(tree, None, None, None)) (currentSize * 2)
    
        if neededSize <= currentSize then tree
        else go tree currentSize

    let mulOp, neutral, alSt = m1.AlgebraicStruct.getMulOp, m1.AlgebraicStruct.getNeutral, m1.AlgebraicStruct
    let rec go (m1:QuadTree<'t>) (m2:QuadTree<'t>) currSize =
        match m1, m2 with
        | Leaf x, Leaf y ->
            let r = mulOp x y
            if r = neutral then None else Leaf r
        | None, _ -> None
        | _, None -> None
        | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
            let nw = sum (QuadTreeMatrix(currSize, currSize, alSt, go nw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, alSt, go ne1 sw2 (currSize/2)))
            let ne = sum (QuadTreeMatrix(currSize, currSize, alSt, go nw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, alSt, go ne1 se2 (currSize/2)))
            let se = sum (QuadTreeMatrix(currSize, currSize, alSt, go sw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, alSt, go se1 se2 (currSize/2)))
            let sw = sum (QuadTreeMatrix(currSize, currSize, alSt, go sw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, alSt, go se1 sw2 (currSize/2)))
            matchTrees nw.Tree ne.Tree se.Tree sw.Tree
        | _, _ -> failwith "Different sizes of matrices"

    if m1.Cols <> m2.Rows
    then
        failwith "Wrong size of matrices"
    else if m1.AlgebraicStruct <> m2.AlgebraicStruct
    then
        failwith "Algebraic structures must be the same"
    else
        let currentSquaredSize = max m1.SquaredSize m2.SquaredSize
        let oversizedTree = go (increaseToSize m1.Tree currentSquaredSize m1.SquaredSize) (increaseToSize m2.Tree currentSquaredSize m2.SquaredSize) (currentSquaredSize / 2)
        let tree = reduceToSize oversizedTree (getSquaredSize m2.Cols m1.Rows) currentSquaredSize
        QuadTreeMatrix(m2.Cols, m1.Rows, m1.AlgebraicStruct, tree)

let scalarMul (m:QuadTreeMatrix<'t>) (s:'t) =
     let mulOp, neutral = m.AlgebraicStruct.getMulOp, m.AlgebraicStruct.getNeutral
     let rec go m =
         match m with
         | Leaf x -> Leaf (mulOp s x)
         | None -> None
         | Node(nw, ne, se, sw) ->
             let nw = go nw 
             let ne = go ne 
             let se = go se 
             let sw = go sw 
             matchTrees nw ne se sw

     QuadTreeMatrix(m.Cols, m.Rows, m.AlgebraicStruct, if s = neutral then None else go m.Tree)

let tensorMul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) =
    let rec go m1 =
        match m1 with
        | Leaf x -> (scalarMul m2 x).Tree
        | None -> None
        | Node(nw, ne, se, sw) ->
            let nw = go nw
            let ne = go ne
            let se = go se
            let sw = go sw
            matchTrees nw ne se sw

    if m1.Rows = m1.Cols && m1.SquaredSize = roundToPowerOfTwo m1.SquaredSize
        && m2.Rows = m2.Cols && m2.SquaredSize = roundToPowerOfTwo m2.SquaredSize
    then
        let t =
            match m1.Tree, m2.Tree with
            | None, _ | _, None -> None
            | _, _ -> go m1.Tree
        QuadTreeMatrix(m1.SquaredSize * m2.SquaredSize, m1.SquaredSize * m2.SquaredSize, m1.AlgebraicStruct, t)
    else
        failwith "Size of matrices should be the power of two"
