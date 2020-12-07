module MyList

type MyList<'t> =
    | One of 't
    | Cons of 't * MyList<'t>

type MyString = MyList<char>

let len l =
    let rec go l c =
        match l with
            | One _ -> c + 1
            | Cons(_, tail) -> go tail (c + 1)

    go l 0

let rec fold folder acc l  =
    match l with
    | One x -> folder acc x
    | Cons(x, tail) -> fold folder (folder acc x) tail

let rec concat l1 l2 =
    match l1 with
        | One last -> Cons(last, l2)
        | Cons(head, tail) -> Cons(head, concat tail l2)

let rec concatMyString (s1:MyString) (s2:MyString):MyString =
    concat s1 s2

let rec map mapping l =
    match l with
    | One el -> One (mapping el)
    | Cons(head, tail) -> Cons(mapping head, map mapping tail)

let rec iter action l =
    match l with
    | One el -> action el
    | Cons(head, tail) ->
        action head
        iter action tail    

let bubbleSort l =
    let rec go l =
        match l with
        | One el -> One el
        | Cons(head, One el) -> if head <= el then l else Cons(el, One head)
        | Cons(a, Cons(b, tail)) -> if a <= b then Cons(a, (go (Cons(b, tail)))) else Cons(b, (go (Cons(a, tail))))

    let rec go2 l c lenL =
        if c = lenL then l
        else go2 (go l) (c + 1) lenL

    go2 l 0 (len l)

let listToMyList l =
    let rec go l ml =
        match l with
        | [] -> ml
        | head :: tail -> go tail (Cons(head, ml))

    if List.isEmpty l
    then
        failwith "List should not be empty"
    else
        go (List.rev l).Tail (One l.[l.Length - 1])

let myListToList l =
    fold (fun list x -> list @ [x]) [] l

let stringToMyString s:MyString =
    listToMyList (Seq.toList s)

let myStringToString (s:MyString) =
    myListToList s |> List.toArray |> System.String |> string


