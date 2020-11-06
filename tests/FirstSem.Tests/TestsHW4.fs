module HW4

open Expecto
open HW4
open System

let testSort sortFun1 sortFun1Name sortFun2 sortFun2Name emptyColl =
    let msg = sprintf "%s is not equal to %s" sortFun1Name sortFun2Name
    let name1 = sprintf "%s and %s test" sortFun1Name sortFun2Name
    let name2 = sprintf "%s empty collection given" sortFun1Name
    testList "sortFun Tests" [
        testCase name2 <| fun _ ->
            Expect.equal (sortFun1 emptyColl) emptyColl "Empty collection should be returned"
        testProperty name1 <| fun (a) ->
            Expect.equal (sortFun1 a) (sortFun2 a) msg
    ]

let testWriteRead writeFun writeFunName readFun readFunName =
    let msg = sprintf "%s is not inverse to %s" writeFunName readFunName
    let name = sprintf "%s and %s test" writeFunName readFunName
    testProperty name <| fun (a) ->
        let s = sprintf "%s" writeFunName
        writeFun s a
        Expect.equal a (readFun s) msg

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

let testWriteEx writeFun writeFunName emptyCont =
    let msg = "Exception should be raised"
    let name1 = sprintf "%s Invalid file name given"  writeFunName
    let name2 = sprintf "%s Empty file path given"  writeFunName
    testList "Exceptions" [
        testCase name1 <| fun _ ->
            Expect.throws (fun _ -> writeFun "/\?" emptyCont |> ignore) msg
        testCase name2 <| fun _ ->
            Expect.throws (fun _ -> writeFun "" emptyCont |> ignore) msg
    ]

let testPackUnpack (packFun) packFunName (unpackFun) unpackFunName =
    let msg = sprintf "%s is not inverse to %s" packFunName unpackFunName
    let name1 = sprintf "%s and %s test" packFunName unpackFunName
    let name2 = sprintf "%s and %s test" unpackFunName packFunName
    testList "Pack and Unack tests" [
        testProperty name1 <| fun (a) ->
            Expect.equal a (unpackFun (packFun a)) msg
        testProperty name2 <| fun (a) ->
            Expect.equal a (packFun (unpackFun a)) msg
    ]
    
[<Tests>]
let tests =
    testList "Tests for HW4" [
       
        testWriteRead writeList "writeList" readList "readList"
        testWriteRead writeArray "writeArray" readArray "readArray"
       
        testReadEx readArray "readArray"
        testReadEx readList "readList"

        testWriteEx writeArray "writeArray" [| |]
        testWriteEx writeList "writeList" [ ] 

        testSort arrayBubbleSort "arrayBubbleSort" Array.sort "Array.sort" [| |]
        testSort listBubbleSort "listBubbleSort" List.sort "List.sort" [ ]
        testSort arrayQuickSort "arrayQuicksort" Array.sort "Array.sort" [| |]
        testSort listQuickSort "listQuicksort" List.sort "List.sort" [ ]

        testPackUnpack pack16To32 "pack16To32" unpack32To16 "unpack32To16"
        testPackUnpack pack32To64 "pack32To64" unpack64To32 "unpack64To32"
        testPackUnpack pack16To64 "pack16to64" unpack64To16 "unpack64to16"

    ]
