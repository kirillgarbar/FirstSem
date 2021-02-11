module QuadTreeMatrix

open SparseMatrix

type Direction =
    | NW
    | NE
    | SE
    | SW

type QuadTree<'t> =
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

type QuadTreeMatrix<'t> =
    val Size:int
    val Tree:QuadTree<'t>

    new(s, t) = { Size = s; Tree = t }

let getDirection center (rcv:RCV<'t>) =
    match center with
    | xNode, yNode ->
        match rcv.Row, rcv.Col with
        | x, y when xNode > x && yNode <= y -> NW
        | x, y when xNode <= x && yNode <= y -> NE
        | x, y when xNode <= x && yNode > y -> SE
        | x, y when xNode > x && yNode > y -> SW
        | _, _ -> failwith "Impossible case"

let getTree (node:QuadTree<'t>) (dir:Direction) =
    match node with
    | Node(nw, ne, se, sw) ->
        match dir with
        | NW -> nw
        | NE -> ne
        | SE -> se
        | SW -> sw
    | _ -> failwith "Node expected"

let roundToPowerOfTwo x =
    let rec go x r =
        if pown 2 r < x then go x (r + 1) else r

    pown 2 (go x 0)

let placeTree (node:QuadTree<'t>) (tree:QuadTree<'t>) (dir:Direction) =
    match node with
    | Node(nw, ne, se, sw) ->
        match dir with
        | NW -> Node(tree, ne, se, sw)
        | NE -> Node(nw, tree, se, sw)
        | SE -> Node(nw, ne, tree, sw)
        | SW -> Node(nw, ne, se, tree)
    | _ -> failwith "Node expected"

let getNewCenter center (dir:Direction) size =
    match center with
    | x, y ->
        match dir with
        | NW -> (x / 2, y + size / 4)
        | NE -> (x + size / 4, y + size / 4)
        | SE -> (x + size / 4, y / 2)
        | SW -> (x / 2, y / 2)

let set (m:QuadTreeMatrix<'t>) (rcv:RCV<'t>) =
    let rec go center (n:QuadTree<'t>) c =
        match c with
        | 0 -> None
        | 1 -> Leaf rcv.Value
        | 2 ->
            match n with
            | Node _ -> placeTree n (Leaf rcv.Value) (getDirection center rcv)
            | _ -> failwith "Node expexted"
        | _ ->
            match n with
            | Node _ ->
                let dir = getDirection center rcv
                let next = getTree n dir
                let newCenter = getNewCenter center dir c
                match next with
                | Node _ -> placeTree n (go newCenter next (c / 2)) dir
                | None ->
                    let newNode = Node(None, None, None, None)
                    placeTree n (go newCenter newNode (c / 2)) dir
                | _ -> failwith "Impossible case"
            | _ -> failwith "Impossible case"

    if m.Size <= rcv.Row || m.Size <= rcv.Col then failwith "Index is out of bounds"
    else QuadTreeMatrix(roundToPowerOfTwo m.Size, go (m.Size/2, m.Size/2) m.Tree m.Size)

let get (m:QuadTreeMatrix<'t>) coord =
    let rec go center (n:QuadTree<'t>) rcv =
        match n with
        | Leaf x -> x
        | Node _ ->
            let dir = getDirection center rcv
            let next = getTree n dir
            let newCenter = getNewCenter center dir m.Size
            go newCenter next rcv
        | None -> failwith "There is no value by given coordinates"

    let rcv = RCV(fst coord, snd coord, 0)
    go (m.Size/2, m.Size/2) m.Tree rcv

let initQTM (m:SparseMatrix<'t>) =
    let rec go (l:list<RCV<'t>>) (result:QuadTreeMatrix<'t>) =
        match l with
        | [] -> result
        | head :: tail -> go tail (set result head)

    let size = roundToPowerOfTwo (max (fst m.Size) (snd m.Size))
    let emptyM = QuadTreeMatrix(size, Node(None, None, None, None))
    go m.List emptyM
