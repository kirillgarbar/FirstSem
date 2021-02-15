module QuadTreeMatrix

open SparseMatrix
open Group

type Direction =
    | NW
    | NE
    | SE
    | SW

type QuadTree<'t when 't: equality> =
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

    member this.set (crv:CRV<'t>) size =          
        let rec go center m currSize =           
            match currSize with
            | 1 -> Leaf crv.Value
            | _ ->
                let dir = QuadTree.getDirection center crv
                let newCenter = QuadTree<_>.getNewCenter center dir currSize
                let next = QuadTree.getTree m dir
                m.placeTree (go newCenter next (currSize / 2)) dir
               
        if crv.Col >= size || crv.Row >= size then failwith "Index is out of bounds"
        else go (size/2, size/2) this size

    member this.get coord size =
        let rec go center m currSize =
            match m with
            | None -> None
            | Leaf _ -> m
            | Node _ ->
                let dir = QuadTree.getDirection center (CRV(fst coord, snd coord, 1))
                go (QuadTree<_>.getNewCenter center dir currSize) (QuadTree.getTree m dir) (currSize / 2)

        if fst coord >= size || snd coord >= size then failwith "Index is out of bounds"
        else go (size/2, size/2) this size

    static member initQT (m:SparseMatrix<'t>) =
        let rec go (l:list<CRV<'t>>) (result:QuadTree<'t>) =
            match l with
            | [] -> result
            | head :: tail -> go tail (result.set head m.Size)
    
        if m.List.Length = 0 then None
        else
            let emptyM = Node(None, None, None, None)
            go m.List emptyM

    static member sum (m1:QuadTree<'t>) (m2:QuadTree<'t>) (group:Group<'t>) =
        let sumOp, neutral =
            match group with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Monoid.Sum, x.Monoid.Neutral
        let rec go m1 m2 = 
            match m1, m2 with
            | Leaf x , Leaf y -> if sumOp x y = neutral then None else Leaf (sumOp x y)
            | x, None -> x
            | None, x -> x
            | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
                let nw = go nw1 nw2 
                let ne = go ne1 ne2 
                let se = go se1 se2 
                let sw = go sw1 sw2 
                QuadTree<_>.matchTrees nw ne se sw
            | _, _ -> failwith "Different sizes of matrices"

        go m1 m2 

    static member mul (m1:QuadTree<'t>) (m2:QuadTree<'t>) (group:Group<'t>) =
        let mulOp, neutral =
            match group with
            | Monoid x -> x.Sum, x.Neutral
            | SemiRing x -> x.Mul, x.Monoid.Neutral
        let rec go m1 m2 =
            match m1, m2 with
            | Leaf x, Leaf y -> if mulOp x y = neutral then None else Leaf (mulOp x y)
            | None, _ -> None
            | _, None -> None
            | Node(nw1, ne1, se1, sw1), Node(nw2, ne2, se2, sw2) ->
                let nw = QuadTree.sum (go nw1 nw2) (go ne1 sw2) group
                let ne = QuadTree.sum (go nw1 ne2) (go ne1 se2) group
                let se = QuadTree.sum (go sw1 ne2) (go se1 se2) group
                let sw = QuadTree.sum (go sw1 nw2) (go se1 sw2) group
                QuadTree<_>.matchTrees nw ne se sw
            | _, _ -> failwith "Different sizes of matrices"

        go m1 m2

    member this.scalarMul s (group:Group<'t>) =
        let mulOp, neutral =
            match group with
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

        if neutral = s then None
        else go this
        
    static member tensorMul (m1:QuadTree<'t>) (m2:QuadTree<'t>) group =
        let rec go m1 =
            match m1 with
            | Leaf x -> m2.scalarMul x group
            | None -> None
            | Node(nw, ne, se, sw) ->
                let nw = go nw
                let ne = go ne
                let se = go se
                let sw = go sw
                QuadTree<_>.matchTrees nw ne se sw

        match m1, m2 with
        | None, _ -> None
        | _, None -> None
        | _, _ -> go m1
        
