module HW4

open Expecto
open HW4
open System

let testSort sortFun1 sortFun1Name sortFun2 sortFun2Name =
    let msg = sprintf "%s is not equal to %s" sortFun1Name sortFun2Name
    let name = sprintf "%s and %s test" sortFun1Name sortFun2Name
    testProperty name <| fun (a) ->
        Expect.equal (sortFun1 a) (sortFun2 a) msg

let testWriteReadCollection writeFun writeFunName readFun readFunName =
    let msg = sprintf "%s is not inverse to %s" writeFunName readFunName
    let name = sprintf "%s and %s test" writeFunName readFunName
    testProperty name <| fun (a) ->
        let s = sprintf "%s" writeFunName
        writeFun s a
        Expect.sequenceEqual a (readFun s) msg

let testReadEx readFun readFunName =
    let msg = "Exception should be raised"
    let name1 = sprintf "%s Wrong file path given"  readFunName
    let name2 = sprintf "%s Invalid file name given"  readFunName
    let name3 = sprintf "%s Empty file path given"  readFunName
    testList "Exceptions" [
        testCase name1 <| fun _ ->
            Expect.throws (fun _ -> readFun "D:\notexistingfolder" |> ignore) msg
        testCase name2 <| fun _ ->
            Expect.throws (fun _ -> readFun "???" |> ignore) msg
        testCase name3 <| fun _ ->
            Expect.throws (fun _ -> readFun "" |> ignore) msg
    ]

let testPackUnpack (packFun) packFunName (unpackFun) unpackFunName =
    let msg = sprintf "%s is not inverse to %s" packFunName unpackFunName
    let name = sprintf "%s and %s test" packFunName unpackFunName
    testProperty name <| fun (a, b) ->
        let s = (a, b)
        Expect.equal s (unpackFun (packFun ((fst s), (snd s)))) msg
    
[<Tests>]
let tests =
    testList "Tests for HW4" [
        testProperty "Write test" <| fun (s:string) ->
            write "Input.txt" s
            if s <> null
            then    
                Expect.equal s (System.IO.File.ReadAllText "Input.txt") "Write is incorrect"
            else
                Expect.equal "" (System.IO.File.ReadAllText "Input.txt") "Write is incorrect"

        testWriteReadCollection writeArray "writeArray" readArray "readArray"
        testReadEx readArray "readArray"

        testWriteReadCollection writeList "writeList" readList "readList"
        testReadEx readList "readList"

        testCase "arrayBubblle sort test. Empty array given" <| fun _ ->
            Expect.equal (arrayBubbleSort [| |]) [| |] "Empty array should be returned"
        testSort arrayBubbleSort "arrayBubbleSort" Array.sort "Array.sort"

        testCase "listBubblleSort test. Empty array given" <| fun _ ->
            Expect.equal (listBubbleSort [ ]) [ ] "Empty list should be returned"
        testSort listBubbleSort "listBubbleSort" List.sort "List.sort"

        testCase "arrayQuickSort test. Empty array given" <| fun _ ->
            Expect.equal (arrayQuickSort [| |]) [| |] "Empty array should be returned"
        testSort arrayQuickSort "arrayQuicksort" Array.sort "Array.sort"

        testCase "listQuickSort test. Empty array given" <| fun _ ->
            Expect.equal (listQuickSort [ ]) [ ] "Empty list should be returned"
        testSort listQuickSort "listQuicksort" List.sort "List.sort"

        testPackUnpack pack16To32 "pack16To32" unpack32To16 "unpack32To16"

        testPackUnpack pack32To64 "pack32To64" unpack64To32 "unpack64To32"

        testProperty "pack16To64 and unpack64To16" <| fun (i, j, k, l) ->
            Expect.equal (i, j, k, l) (unpack64To16 (pack16To64 (i, j, k, l))) "pack16To64 and unpack64To16 should be opposite"
    ]
