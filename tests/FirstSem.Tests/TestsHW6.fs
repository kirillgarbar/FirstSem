module TestsHW6

open Expecto
open HW6
open System

let genRandBMat() =
    let r = Random()
    let m = r.Next 10
    let n = r.Next 10
    if m = 0 || n = 0
    then
        boolMat([], 0, 0)
    else
        let l = [
            for i = 0 to m - 1 do
                for j = 0 to n - 1 do
                    if r.Next 2 = 1 then row_col(i * 1<row>, j * 1<col>)
        ]
        boolMat(l, m, n)

[<Tests>]
let tests =
    testList "Tests for HW6" [

        testCase "readBoolMat test. Empty file given" <| fun _ ->
            writeBoolMat "Test.txt" (boolMat([], 0, 0))
            Expect.equal (readBoolMat "Test.txt") (boolMat([], 0, 0)) "Empty matrix should be readen"
        testProperty "readBoolMat and writeMat test" <| fun _ ->
            let mat = genRandBMat()
            writeBoolMat "Test1.txt" mat
            Expect.equal (readBoolMat "Test1.txt") mat "readBoolMat =/= writeMat?" 
        testCase "readBoolMat test. Not bool matrix given" <| fun _ ->
            let mat = "1 2"
            System.IO.File.WriteAllText ("Test2.txt", mat)
            Expect.throws (fun _ -> readBoolMat "Test2.txt" |> ignore) "Exception should be raised"
        testCase "readBoolMat test. Wrong sized matrix given" <| fun _ ->
            let mat = "1 0\n1"
            System.IO.File.WriteAllText ("Test3.txt", mat)
            Expect.throws (fun _ -> readBoolMat "Test3.txt" |> ignore) "Exception should be raised"

        testCase "mulBMat test. Wrong sized matrices given" <| fun _ ->
            let m1 = boolMat([], 3, 4)
            let m2 = boolMat([], 3, 3)
            Expect.throws (fun _ -> mulBoolMat m1 m2 |> ignore) "Exception should be raised"
        testCase "Multiplication 3x3 and 3x3 matrices" <| fun _ ->
            let m1 = boolMat([ row_col(0<row>, 0<col>); row_col(1<row>, 1<col>); row_col(2<row>, 0<col>); row_col(2<row>, 2<col>) ], 3, 3)
            let m2 = boolMat([ row_col(0<row>, 1<col>); row_col(1<row>, 2<col>); row_col(2<row>, 0<col>); row_col(2<row>, 1<col>); row_col(2<row>, 2<col>) ], 3, 3)
            let r = mulBoolMat m1 m2
            let t = boolMat([row_col(0<row>, 1<col>); row_col(1<row>, 2<col>); row_col(2<row>, 1<col>); row_col(2<row>, 0<col>); row_col(2<row>, 2<col>)], 3, 3)
            Expect.equal r t  "mulBMat 3x3 3x3 is wrong"
        testCase "Multiplication 3x2 and 2x4 matrices" <| fun _ ->
            let m1 = boolMat([ row_col(0<row>, 0<col>); row_col(1<row>, 1<col>); row_col(2<row>, 1<col>)], 3, 2)
            let m2 = boolMat([ row_col(0<row>, 1<col>); row_col(1<row>, 2<col>); row_col(1<row>, 1<col>); row_col(0<row>, 3<col>) ], 2, 4)
            let r = mulBoolMat m1 m2
            let t = boolMat([row_col(0<row>, 1<col>); row_col(0<row>, 3<col>); row_col(1<row>, 2<col>); row_col(1<row>, 1<col>); row_col(2<row>, 2<col>); row_col(2<row>, 1<col>)], 3, 4)
            Expect.equal r t "mulBMat 3x2 2x4 is wrong"
        testCase "Multiplication 5x3 3x3 matrices" <| fun _ ->
            let m1 = boolMat([ row_col(0<row>, 0<col>); row_col(1<row>, 1<col>); row_col(2<row>, 1<col>); row_col(2<row>, 1<col>); row_col(3<row>, 1<col>); row_col(3<row>, 0<col>); row_col(4<row>, 2<col>)], 5, 3)
            let m2 = boolMat([ row_col(0<row>, 1<col>); row_col(1<row>, 2<col>); row_col(2<row>, 0<col>); row_col(2<row>, 1<col>); row_col(2<row>, 2<col>) ], 3, 3)
            let r = mulBoolMat m1 m2
            let t = boolMat([row_col(0<row>, 1<col>); row_col(1<row>, 2<col>); row_col(2<row>, 2<col>); row_col(3<row>, 2<col>); row_col(3<row>, 1<col>); row_col(4<row>, 0<col>); row_col(4<row>, 1<col>); row_col(4<row>, 2<col>)], 5, 3)
            Expect.equal r t "mulBMat 5x3 3x3 is wrong"
        
    ]
