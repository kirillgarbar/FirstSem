module QuadTreeMatrix

open SparseMatrix
open AlgebraicStruct

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

    static member getNewCenter center (dir:Direction) currSize =
        match center with
        | x, y ->
            match dir with
            | NW -> (x - currSize / 4, y - currSize / 4)
            | NE -> (x + currSize / 4, y - currSize / 4)
            | SE -> (x + currSize / 4, y + currSize / 4)
            | SW -> (x - currSize / 4, y  + currSize / 4)

    static member matchTrees nw ne se sw =
        match nw, ne, se, sw with
        | None, None, None, None -> None
        | _, _, _, _ -> Node(nw, ne, se, sw)

type QuadTreeMatrix<'t when 't : equality> =
    val Size:int
    val Tree:QuadTree<'t>

    new(s, t) = { Size = s; Tree = t }

    override this.GetHashCode() =
        hash (this.Size, this.Tree)

    override this.Equals(t) =
        match t with
        | :? QuadTreeMatrix<'t> as t -> this.Tree = t.Tree && this.Size = t.Size
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
               
        if crv.Col >= this.Size || crv.Row >= this.Size then failwith "Index is out of bounds"
        else  QuadTreeMatrix(this.Size, go (this.Size/2, this.Size/2) this.Tree this.Size)

    member this.get coord =
        let rec go center m currSize =
            match m with
            | None -> None
            | Leaf _ -> m
            | Node _ ->
                let dir = QuadTree.getDirection center (CRV(fst coord, snd coord, 1))
                go (QuadTree<_>.getNewCenter center dir currSize) (QuadTree.getTree m dir) (currSize / 2)

        if fst coord >= this.Size || snd coord >= this.Size then failwith "Index is out of bounds"
        else go (this.Size/2, this.Size/2) this.Tree this.Size

    static member initQTM (m:SparseMatrix<'t>) =
        let rec go (l:list<CRV<'t>>) (result:QuadTreeMatrix<'t>) =
            match l with
            | [] -> result
            | head :: tail -> go tail (result.set head)
    
        if m.List.Length = 0 then QuadTreeMatrix(m.Size, None)
        else
            let emptyM = QuadTreeMatrix(m.Size, Node(None, None, None, None))
            go m.List emptyM

    static member sum (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) (astruct:AlgebraicStruct<'t>) =
        let sumOp, neutral =
            match astruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Monoid.Sum, x.Monoid.Neutral
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

        QuadTreeMatrix(m1.Size, go m1.Tree m2.Tree) 

    static member mul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) (astruct:AlgebraicStruct<'t>) =
        let mulOp, neutral =
            match astruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Mul, x.Monoid.Neutral
        let rec go (m1:QuadTree<'t>) (m2:QuadTree<'t>) currSize =
            match m1, m2 with
            | Leaf x, Leaf y ->
                let r = mulOp x y
                if r = neutral then None else Leaf r
            | None, _ -> None
            | _, None -> None
            | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
                let nw = QuadTreeMatrix.sum (QuadTreeMatrix(currSize, go nw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, go ne1 sw2 (currSize/2))) astruct
                let ne = QuadTreeMatrix.sum (QuadTreeMatrix(currSize, go nw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, go ne1 se2 (currSize/2))) astruct
                let se = QuadTreeMatrix.sum (QuadTreeMatrix(currSize, go sw1 ne2 (currSize/2))) (QuadTreeMatrix(currSize, go se1 se2 (currSize/2))) astruct
                let sw = QuadTreeMatrix.sum (QuadTreeMatrix(currSize, go sw1 nw2 (currSize/2))) (QuadTreeMatrix(currSize, go se1 sw2 (currSize/2))) astruct
                QuadTree<_>.matchTrees nw.Tree ne.Tree se.Tree sw.Tree
            | _, _ -> failwith "Different sizes of matrices"

        QuadTreeMatrix(m1.Size, go m1.Tree m2.Tree (m1.Size/2))

    member this.scalarMul s (astruct:AlgebraicStruct<'t>) =
        let mulOp, neutral =
            match astruct with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Mul, x.Monoid.Neutral
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

        if neutral = s then QuadTreeMatrix(this.Size, None)
        else QuadTreeMatrix(this.Size, go this.Tree)
        
    static member tensorMul (m1:QuadTreeMatrix<'t>) (m2:QuadTreeMatrix<'t>) group =
        let rec go m1 =
            match m1 with
            | Leaf x -> (m2.scalarMul x group).Tree
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
        QuadTreeMatrix(m1.Size * m2.Size, t)
        
