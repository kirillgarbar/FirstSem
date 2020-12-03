module MyTree

open MyList
open System

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>

let rec fold folder acc tree =
    match tree with
    | Leaf x -> folder x acc
    | Node (x, tail) -> MyList.fold (fun t acc -> fold folder acc t) (folder x acc) tail

let max tree =
    fold (fun x max -> if x > max then x else max) Int32.MinValue tree

let average tree =
    let x, y = fold (fun x (sum, count) -> (sum + x, count + 1)) (0, 0) tree
    (float x) / (float y)
