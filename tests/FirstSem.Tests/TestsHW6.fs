module TestsHW6

open Expecto
open HW3
open HW6
open System

let genRandBoolMat =
    let r = Random()
    let a = mat (r.Next 10) (r.Next 10)
    for i = 0 to a.Length - 1 do
        for j = 0 to a.[0].Length - 1 do
            a.[i].[j] <- r.Next 2
    a

[<Tests>]
let tests =
    testList "Tests for HW6" [

        testCase "readBoolMat test. Empty file given" <| fun _ ->
            writeMat "Test.txt" [| |]
            Expect.sequenceEqual (readBoolMat "Test.txt") [| |] "Empty matrix should be readen"
        testProperty "readBoolMat and writeMat test" <| fun _ ->
            let mat = genRandBoolMat
            writeMat "Test1.txt" mat
            Expect.sequenceEqual (readBoolMat "Test1.txt") mat "readBoolMat =/= writeMat?" 
        testCase "readBoolMat test. Not bool matrix given" <| fun _ ->
            let mat = [| [| 1; 2 |] |]
            writeMat "Test2.txt" mat
            Expect.throws (fun _ -> readBoolMat "Test2.txt" |> ignore) "Exception should be raised"

        testCase "boolMatToBMat test. Not bool matrix given" <| fun _ ->
            let mat = [| [| 1; 2 |] |]
            writeMat "Test3.txt" mat
            Expect.throws (fun _ -> readBoolMat "Test3.txt" |> ignore) "Exception should be raised"
        testProperty "boolMatToBMat and bMatToMat test" <| fun _ ->
            let m = genRandBoolMat
            Expect.sequenceEqual m (bMatToMat (boolMatToBMat m))

        testCase "mulBMat test. Wrong sized matrices given" <| fun _ ->
            let m1 = bMat([], 3, 4)
            let m2 = bMat([], 3, 3)
            Expect.throws (fun _ -> mulBMat m1 m2 |> ignore) "Exception should be raised"
        testCase "Multiplication 3x3 and 3x3 matrices" <| fun _ ->
            let m1 = bMat([ rc(0<row>, 0<col>); rc(1<row>, 1<col>); rc(2<row>, 0<col>); rc(2<row>, 2<col>) ], 3, 3)
            let m2 = bMat([ rc(0<row>, 1<col>); rc(1<row>, 2<col>); rc(2<row>, 0<col>); rc(2<row>, 1<col>); rc(2<row>, 2<col>) ], 3, 3)
            let r = bMatToMat (mulBMat m1 m2)
            let t = bMatToMat (bMat([rc(0<row>, 1<col>); rc(1<row>, 2<col>); rc(2<row>, 1<col>); rc(2<row>, 0<col>); rc(2<row>, 2<col>)], 3, 3))
            Expect.sequenceEqual r t  "mulBMat 3x3 3x3 is wrong"
        testCase "Multiplication 3x2 and 2x4 matrices" <| fun _ ->
            let m1 = bMat([ rc(0<row>, 0<col>); rc(1<row>, 1<col>); rc(2<row>, 1<col>)], 3, 2)
            let m2 = bMat([ rc(0<row>, 1<col>); rc(1<row>, 2<col>); rc(1<row>, 1<col>); rc(0<row>, 3<col>) ], 2, 4)
            let r = bMatToMat (mulBMat m1 m2)
            let t = bMatToMat (bMat([rc(0<row>, 1<col>); rc(0<row>, 3<col>); rc(1<row>, 2<col>); rc(1<row>, 1<col>); rc(2<row>, 2<col>); rc(2<row>, 1<col>)], 3, 4))
            Expect.sequenceEqual r t "mulBMat 3x2 2x4 is wrong"
        testCase "Multiplication 5x3 3x3 matrices" <| fun _ ->
            let m1 = bMat([ rc(0<row>, 0<col>); rc(1<row>, 1<col>); rc(2<row>, 1<col>); rc(2<row>, 1<col>); rc(3<row>, 1<col>); rc(3<row>, 0<col>); rc(4<row>, 2<col>)], 5, 3)
            let m2 = bMat([ rc(0<row>, 1<col>); rc(1<row>, 2<col>); rc(2<row>, 0<col>); rc(2<row>, 1<col>); rc(2<row>, 2<col>) ], 3, 3)
            let r = bMatToMat (mulBMat m1 m2)
            let t = bMatToMat (bMat([rc(0<row>, 1<col>); rc(1<row>, 2<col>); rc(2<row>, 2<col>); rc(3<row>, 2<col>); rc(3<row>, 1<col>); rc(4<row>, 0<col>); rc(4<row>, 1<col>); rc(4<row>, 2<col>)], 5, 3))
            Expect.sequenceEqual r t "mulBMat 5x3 3x3 is wrong"
        
    ]
