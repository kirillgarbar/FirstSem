module TestsHW7

open MyList
open MyTree
open Expecto

let genRandomList count =
    let rnd = System.Random()
    let c = if count = 0 then rnd.Next(1, 10) else abs count
    List.init c (fun _ -> rnd.Next(100))

[<Tests>]
let tests =
    testList "Tests for MyList and MyString" [
        testProperty "bubbleSort test" <| fun a ->
            let list = genRandomList a
            Expect.sequenceEqual (List.sort list) (myListToList (bubbleSort (listToMyList list))) "bubbleList =/= List.sort"
        testProperty "fold test" <| fun a ->
            let list = genRandomList a |> List.map (fun i -> string i) 
            let str = List.fold (fun s i -> s + i) "" list
            Expect.sequenceEqual (MyList.fold (fun s i -> s + i) "" (listToMyList list)) (str) "fold =/= List.fold"
        testProperty "len test" <| fun a ->
            let list = genRandomList a
            Expect.equal (list.Length) (len (listToMyList list)) ".Length =/= length"
        testProperty "concat test" <| fun (a, b) ->
            let list1 = genRandomList a
            let list2 = genRandomList b
            Expect.sequenceEqual (list1 @ list2) (myListToList (concat (listToMyList list1) (listToMyList list2))) "@ =/= concat"
        testProperty "map test" <| fun a ->
            let list = genRandomList a
            Expect.sequenceEqual (List.map (fun i -> i * 2) list) (myListToList (map (fun i -> i * 2) (listToMyList list))) "@ =/= concat"
        testProperty "iter" <| fun a ->
            let list = genRandomList a  
            let a1 = Array.zeroCreate list.Length
            let a2 = Array.ofList list
            let mutable c = 0
            iter (fun x ->
                a1.[c] <- x
                c <- c + 1) (listToMyList list) 
            Expect.sequenceEqual a1 a2 "iter is incorrect"

        testProperty "myListToList and listToMyList test" <| fun a ->
            let list = genRandomList a
            Expect.sequenceEqual list (myListToList (listToMyList list)) "myListToList =/= listToMyList"
        testCase "listToMyList test. Empty list given" <| fun _ ->
            let list = []
            Expect.throws (fun _ -> listToMyList list |> ignore) "Exception should be raised"

        testProperty "stringToMyString and myStringToString test" <| fun a ->
            let s = if a = null || a = "" then "a" else a
            Expect.equal s (myStringToString (stringToMyString s)) "stringToMyString =/= myStringToString"
        testProperty "concatMyString test" <| fun (a, b) ->
            let s1 = if a = null || a = "" then "a" else a
            let s2 = if b = null || b = "" then "b" else b
            Expect.equal (s1 + s2) (myStringToString (concatMyString (stringToMyString s1) (stringToMyString s2))) "concatMyString =/= +"

        testProperty "reverse test" <| fun a ->
            let l = genRandomList a
            let rl = List.rev l
            let ml = l |> listToMyList |> reverse |> myListToList
            Expect.sequenceEqual ml rl "List.rev =/= reverse"

        testProperty "map2 test" <| fun a ->
            let l1 = genRandomList a
            let l2 = genRandomList (List.length l1)
            let mappedl = List.map2 (fun i j -> i + j) l1 l2
            let mappedml = map2 (fun i j -> i + j) (listToMyList l1) (listToMyList l2) |> myListToList
            Expect.sequenceEqual mappedml mappedl "List.map2 =/= map2"

        testProperty "equalize test" <| fun (a, b) ->
            let l1 = genRandomList a |> listToMyList
            let l2 = genRandomList b |> listToMyList
            let eq1, eq2 = equalize l1 l2
            Expect.equal (len eq1) (len eq2) "len eq1 =/= len eq2"

        testCase "delZeroHead test" <| fun _ ->
            let l = genRandomList 5
            let lc = concat (Cons(0, Cons(0, One 0))) (l |> listToMyList)
            let r = delZeroHead lc |> myListToList
            Expect.sequenceEqual l r "delZeroHead is wrong"

        testProperty "intToMyList test" <| fun _ ->
            let i = System.Random().Next(10000, 99999)
            let a = i |> string |> Array.ofSeq
            let l = Array.fold (fun l i -> (i |> string |> int) :: l) ([]) a |> List.rev
            let ml = i |> intToMyList |> myListToList
            Expect.sequenceEqual l ml "intToMyList is wrong"

        testCase "addZeroes test" <| fun _ ->
            let l = genRandomList 5 |> listToMyList |> addZeroes 3 |> myListToList
            Expect.sequenceEqual l.[0..2] [0;0;0] "addZeroes is wrong"
            
    ]

[<Tests>]
let tests1 =
    testList "Tests for Mytree" [
        testCase "fold test" <| fun _ ->
            let tree = Node("3", Cons(Node("0", One(Leaf "0")), One(Leaf "$")))
            Expect.sequenceEqual (fold (fun s i -> s + i) "" tree) ("300$") "Wrong price for fisting"
        testCase "max test" <| fun _ ->
            let tree = Node(15, Cons(Node(5, One(Leaf 56)), One(Node(7, One(Leaf 30)))))
            Expect.equal 56 (max tree) "max is incorrect"
        testCase "average test" <| fun _ ->
            let tree = Node(15, Cons(Node(5, One(Leaf 56)), One(Node(7, One(Leaf 30)))))
            Expect.equal 22.6 (average tree) "average is incorrect"
    ]
