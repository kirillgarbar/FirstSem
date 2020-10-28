module TestsHW3

open Expecto
open HW3

[<Tests>]
let tests =
    testList "Tests for HW3" [
        testCase "Build matrix test. Negative numbers given" <| fun _ ->
            Expect.throws (fun _ -> mat -1 2 |> ignore) "Exception should be raised"
        testCase "Build matrix test. Common case" <| fun _ ->
            Expect.sequenceEqual (mat 3 2) [| [| 0; 0 |]; [| 0; 0 |]; [| 0; 0 |] |] "Matrix 3x2 was waited"

        testCase "Build Fibonacci matrix test. Only one possible case" <| fun _ ->
            Expect.sequenceEqual fibMat [| [| 0; 1 |]; [| 1; 1 |] |] "Matrix [| 0; 1 |]; [| 1; 1 |] was waited"

        testCase "Build identity matrix test. Negative numbers given" <| fun _ ->
            Expect.throws (fun _ -> identMat -1 |> ignore) "Exception should be raised"
        testCase "Build identity matrix test. Common case" <| fun _ ->
            Expect.sequenceEqual (identMat 2) [| [| 1; 0 |]; [| 0; 1 |] |] "[| 1; 0 |]; [| 0; 1 |] was waited"

        testCase "Multiplying matrices test. Empty matrix given" <| fun _ ->
            Expect.throws (fun _ -> (mulMat ([|  |]) ([| [| 0; 1 |]; [| 1; 1 |] |])) |> ignore) "Exception should be raised"
        testCase "Multiplying matrices test. Wrong size of matrices" <| fun _ ->
            Expect.throws (fun _ -> (mulMat ([| [| 0; 1 |]; [| 1; 1 |] |]) ([| [| 0; 1 |]; [| 1; 1 |]; [| 1; 1 |] |])) |> ignore) "Exception should be raised"
        testCase "Multiplying matrices test. Common case" <| fun _ ->
            Expect.sequenceEqual (mulMat ([| [| 0; 1 |]; [| 1; 1 |] |]) ([| [| 0; 1 |]; [| 1; 1 |] |])) [| [| 1; 1 |]; [| 1; 2 |] |] "[| 1; 1 |]; [| 1; 2 |] was waited"

        testCase "Pow matrix test. Empty matrix given" <| fun _ ->
            Expect.throws (fun _ -> (powMat ([|  |]) 2) |> ignore) "Exception should be raised"
        testCase "Pow matrix test. Not squared matrix given" <| fun _ ->
            Expect.throws (fun _ -> (powMat ([| [| 1; 1; 1 |]; [| 1; 1; 1 |] |]) 2) |> ignore) "Exception should be raised"
        testCase "Pow matrix test. Negative power given" <| fun _ ->
            Expect.throws (fun _ -> (powMat ([| [| 1; 1 |]; [| 1; 1 |] |]) -1) |> ignore) "Exception should be raised"
        testCase "Pow matrix test. Power = zero" <| fun _ ->
            Expect.sequenceEqual (powMat ([| [| 0; 1 |]; [| 1; 1 |] |]) 0) [| [| 1; 0 |]; [| 0; 1 |] |] "[| 1; 0 |]; [| 0; 1 |] was waited"
        testCase "Pow matrix test. Common case" <| fun _ ->
            Expect.sequenceEqual (powMat ([| [| 0; 1 |]; [| 1; 1 |] |]) 2) [| [| 1; 1 |]; [| 1; 2 |] |] "[| 1; 1 |]; [| 1; 2 |] was waited"

        testCase "Task 1.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> fibRec -1 |> ignore) "Exception should be raised"
        testCase "Task 1.2 Common case" <| fun _ ->
            Expect.equal (fibRec 5) 8 "Fib(5) =/= 8?"

        testCase "Task 2.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> fibIt -1 |> ignore) "Exception should be raised"

        testCase "Task 3.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> fibTailRec -1 |> ignore) "Exception should be raised"

        testCase "Task 4.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> naiveFibMatrix -1 |> ignore) "Exception should be raised"

        testCase "Task 5.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> logFibMatrix -1 |> ignore) "Exception should be raised"

        testCase "Task 6.1 Negative number given" <| fun _ ->
            Expect.throws (fun _ -> allFib -1 |> ignore) "Exception should be raised"
        testCase "Task 6.2 Common case" <| fun _ ->
            Expect.sequenceEqual (allFib 5) ([| 1; 1; 2; 3; 5; 8 |]) "All fib numbers from 0 to 5 =/= 1; 1; 2; 3; 5; 8?"
        testProperty "Task 6.3 At least last fib number in sequence is correct" <| fun (n:int) ->
            Expect.equal (allFib (abs n)).[abs(n)] (logFibMatrix (abs n)) "Last number in sequence is wrong"

        testProperty "Task 1 and 2" <| fun (n:int) ->
            if abs n < 30 then Expect.equal (fibRec (abs n)) (fibIt (abs n)) "Results for fibRec and fibIt should be equal"  // fibRec can't handle large n due to the stack overflowing

        testProperty "Task 2 and 3" <| fun (n:int) ->
            Expect.equal (fibIt (abs n)) (fibTailRec (abs n)) "Results for fibIr and fibTailRec should be equal"

        testProperty "Task 3 and 4" <| fun (n:int) ->
            Expect.equal (fibTailRec (abs n)) (naiveFibMatrix (abs n)) "Results for fibTailRec and naiveFibMatrix should be equal"

        testProperty "Task 4 and 5" <| fun (n:int) ->
            Expect.equal (naiveFibMatrix (abs n)) (logFibMatrix (abs n)) "Results for fibTailRec and naiveFibMatrix should be equal"
    ]
