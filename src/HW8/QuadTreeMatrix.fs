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

    static member getTree (node:QuadTree<'t>) (dir:Direction) =
        match node with
        | Node(nw, ne, se, sw) ->
            match dir with
            | NW -> nw
            | NE -> ne
            | SE -> se
            | SW -> sw
        | None -> None 
        | _ -> failwith "Node or None expected"

    static member getDirection center (crv:CRV<'t>) =
        match center with
        | xNode, yNode ->
            match crv.Col, crv.Row with
            | x, y when xNode > x && yNode > y -> NW
            | x, y when xNode <= x && yNode > y -> NE
            | x, y when xNode <= x && yNode <= y -> SE
            | x, y when xNode > x && yNode <= y -> SW
            | _, _ -> failwith "Impossible case"

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

    static member getNewCenter center (dir:Direction) squaredSize =
        match center with
        | x, y ->
            match dir with
            | NW -> (x - squaredSize / 4, y - squaredSize / 4)
            | NE -> (x + squaredSize / 4, y - squaredSize / 4)
            | SE -> (x + squaredSize / 4, y + squaredSize / 4)
            | SW -> (x - squaredSize / 4, y  + squaredSize / 4)

    static member matchTrees nw ne se sw =
        match nw, ne, se, sw with
        | None, None, None, None -> None
        | _, _, _, _ -> Node(nw, ne, se, sw)

type QuadTreeMatrix<'t when 't : equality> =
    val Cols:int
    val Rows:int
    val SquaredSize:int
    val mutable Tree:QuadTree<'t>

    new(c, r, t) = { Cols = c; Rows = r; SquaredSize = getSquaredSize c r ; Tree = t }

    override this.GetHashCode() =
        hash (this.Cols, this.Rows, this.Tree)

    override this.Equals(t) =
        match t with
        | :? QuadTreeMatrix<'t> as t -> this.Tree = t.Tree && this.Cols = t.Cols && this.Rows = t.Rows
        | _ -> false

    member this.set (crv:CRV<'t>) =          
        let rec go center m currSize =           
            match currSize with
            | 1 -> Leaf crv.Value
            | _ ->
                let dir = QuadTree.getDirection center crv
                let newCenter = QuadTree<_>.getNewCenter center dir currSize
                let next = QuadTree.getTree m dir
                m.placeTree (go newCenter next (currSize / 2)) dir
               
        if crv.Col >= this.Cols || crv.Row >= this.Rows then failwith "Index is out of bounds"
        else this.Tree <- go (this.SquaredSize/2, this.SquaredSize/2) this.Tree this.SquaredSize

    member this.get coord =
        let rec go center m squaredSize =
            match m with
            | None -> None
            | Leaf _ -> m
            | Node _ ->
                let dir = QuadTree.getDirection center (CRV(fst coord, snd coord, 1))
                go (QuadTree<_>.getNewCenter center dir squaredSize) (QuadTree.getTree m dir) (squaredSize / 2)

        if fst coord >= this.Cols || snd coord >= this.Rows then failwith "Index is out of bounds"
        else go (this.SquaredSize/2, this.SquaredSize/2) this.Tree this.SquaredSize

let initQTM (m:SparseMatrix<'t>) =
    if m.List.Length = 0 then QuadTreeMatrix(m.Cols, m.Rows, None)
    else
        let matrix = QuadTreeMatrix(m.Cols, m.Rows, Node(None, None, None, None))
        List.iter (matrix.set) m.List
        matrix

let sum (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) (astruct:AlgebraicStruct<'t>) =
    let sumOp, neutral = astruct.getSumOp, astruct.getNeutral
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
            QuadTree<_>.matchTrees nw ne se sw
        | _, _ -> failwith "Different sizes of matrices"

    QuadTreeMatrix(m1.Cols, m1.Rows, go m1.Tree m2.Tree)

let mul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) (astruct:AlgebraicStruct<'t>) =
    let reduceToSize tree neededSize currentSize =
        let rec go tree currentSize =
            if neededSize = currentSize
            then
                tree
            else
                go (QuadTree.getTree tree NW) (currentSize / 2)
    
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

    let mulOp, neutral = astruct.getMulOp, astruct.getNeutral
    let rec go (m1:QuadTree<'t>) (m2:QuadTree<'t>) currSize =
        match m1, m2 with
        | Leaf x, Leaf y ->
            let r = mulOp x y
            if r = neutral then None else Leaf r
        | None, _ -> None
        | _, None -> None
        | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
            let nw = sum (QuadTreeMatrix(currSize, currSize, go nw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, go ne1 sw2 (currSize/2))) astruct
            let ne = sum (QuadTreeMatrix(currSize, currSize, go nw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, go ne1 se2 (currSize/2))) astruct
            let se = sum (QuadTreeMatrix(currSize, currSize, go sw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, go se1 se2 (currSize/2))) astruct
            let sw = sum (QuadTreeMatrix(currSize, currSize, go sw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, currSize, go se1 sw2 (currSize/2))) astruct
            QuadTree<_>.matchTrees nw.Tree ne.Tree se.Tree sw.Tree
        | _, _ -> failwith "Different sizes of matrices"

    if m1.Cols <> m2.Rows
    then
        failwith "Wrong size of matrices"
    else
        let currentSquaredSize = max m1.SquaredSize m2.SquaredSize
        let oversizedTree = go (increaseToSize m1.Tree currentSquaredSize m1.SquaredSize) (increaseToSize m2.Tree currentSquaredSize m2.SquaredSize) (currentSquaredSize / 2)
        let tree = reduceToSize oversizedTree (getSquaredSize m2.Cols m1.Rows) currentSquaredSize
        QuadTreeMatrix(m2.Cols, m1.Rows, tree)

let scalarMul (m:QuadTreeMatrix<'t>) s (astruct:AlgebraicStruct<'t>) =
     let mulOp, neutral = astruct.getMulOp, astruct.getNeutral
     let rec go m =
         match m with
         | Leaf x -> Leaf (mulOp s x)
         | None -> None
         | Node(nw, ne, se, sw) ->
             let nw = go nw 
             let ne = go ne 
             let se = go se 
             let sw = go sw 
             QuadTree<_>.matchTrees nw ne se sw

     QuadTreeMatrix(m.Cols, m.Rows, if s = neutral then None else go m.Tree)

let tensorMul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) (astruct:AlgebraicStruct<'t>) =
    let rec go m1 =
        match m1 with
        | Leaf x -> (scalarMul m2 x astruct).Tree
        | None -> None
        | Node(nw, ne, se, sw) ->
            let nw = go nw
            let ne = go ne
            let se = go se
            let sw = go sw
            QuadTree<_>.matchTrees nw ne se sw

    let t =
        match m1.Tree, m2.Tree with
        | None, _ | _, None -> None
        | _, _ -> go m1.Tree
    QuadTreeMatrix(m1.SquaredSize * m2.SquaredSize, m1.SquaredSize * m2.SquaredSize, t)
