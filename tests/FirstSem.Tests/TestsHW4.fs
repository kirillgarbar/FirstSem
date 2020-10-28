module HW4

open Expecto
open HW4
open System

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

        testProperty "writeArray and readArray test" <| fun (a:array<int>) ->
            writeArray "Input2.txt" a
            Expect.sequenceEqual a (readArray "Input2.txt") "writeArray and readArray should be inverse"
        testCase "Wrong file path given to readArray" <| fun _ ->
            Expect.throws (fun _ -> readArray "D:\notexistingfolder" |> ignore) "Exception should be raised"
        testCase "Invalid file name given to readArray" <| fun _ ->
            Expect.throws (fun _ -> readArray "???" |> ignore) "Exception should be raised"

        testProperty "writeList and readList test" <| fun (l:list<int>) ->
            writeList "Input3.txt" l
            Expect.sequenceEqual l (readList "Input3.txt") "writeList and readList should be inverse"
        testCase "Wrong file path given to readList" <| fun _ ->
            Expect.throws (fun _ -> readList "D:\notexistingfolder" |> ignore) "Exception should be raised"
        testCase "Invalid file name given to readList" <| fun _ ->
            Expect.throws (fun _ -> readList "???" |> ignore) "Exception should be raised"

        testCase "arrayBubblle sort test. Empty array given" <| fun _ ->
            Expect.equal (arrayBubbleSort [| |]) [| |] "Empty array should be returned"
        testProperty "arrayBubbleSort and Array.sort" <| fun (a:array<int>) ->
            Expect.equal (arrayBubbleSort a) (Array.sort a) "arrayBubbleSort is incorrect"

        testCase "listBubblleSort test. Empty array given" <| fun _ ->
            Expect.equal (listBubbleSort [ ]) [ ] "Empty list should be returned"
        testProperty "listBubbleSSort and List.sort" <| fun (a:list<int>) ->
            Expect.equal (listBubbleSort a) (List.sort a) "listBubbleSort is incorrect"

        testCase "arrayQuickSort test. Empty array given" <| fun _ ->
            Expect.equal (arrayQuickSort [| |]) [| |] "Empty array should be returned"
        testProperty "arrayQuickSort and Array.sort" <| fun (a:array<int>) ->
            Expect.equal (arrayBubbleSort a) (Array.sort a) "arrayQuickSort is incorrect"

        testCase "listQuickSort test. Empty array given" <| fun _ ->
            Expect.equal (listQuickSort [ ]) [ ] "Empty list should be returned"
        testProperty "listQuickSort and List.sort" <| fun (a:list<int>) ->
            Expect.equal (listBubbleSort a) (List.sort a) "listQuickSort is incorrect"

        testProperty "pack32To64 and unpack64To32" <| fun (i, j:int) ->
            Expect.equal [| i; j |] (unpack64To32 (pack32To64 i j)) "pack32To64 and unpack64To32 should be inverse"

        testProperty "pack16To32 and unpack32To16" <| fun (i, j:int16) ->
            Expect.equal [| i; j |] (unpack32To16 (pack16To32 i j)) "pack16To32 and unpack32To16 should be inverse"

        testProperty "pack16To64 and unpack64To16" <| fun (i, j, k, l:int16) ->
            Expect.equal [| i; j; k; l |] (unpack64To16 (pack16To64 i j k l)) "pack16To64 and unpack64To16 should be opposite"
    ]
