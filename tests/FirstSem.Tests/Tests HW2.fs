module Tests_HW2

open Expecto
open FirstSem

[<Tests>]
let tests =
    testList "Tests fpr HW2"[
        testCase "Taks 1.1 X = 0" <| fun _ ->
            Expect.equal 1 (HW2.calculateNaively(0)) "Value should be 1"
        testCase "Taks 1.2 X is negative" <| fun _ ->
            Expect.equal 11 (HW2.calculateNaively(-2)) "Value should be 11"
        testCase "Taks 1.3 X is positive" <| fun _ ->
            Expect.equal 31 (HW2.calculateNaively(2)) "Value should be 31"

        testCase "Taks 2.1 X = 0" <| fun _ ->
            Expect.equal 1 (HW2.calculateNaively(0)) "Value should be 1"
        testCase "Taks 2.2 X is negative" <| fun _ ->
            Expect.equal 11 (HW2.calculateNaively(-2)) "Value should be 11"
        testCase "Taks 2.3 X is positive" <| fun _ ->
            Expect.equal 31 (HW2.calculateNaively(2)) "Value should be 31"

        testCase "Taks 3.1 Common case" <| fun _ ->
            Expect.equal [| 0; 3; 4 |] (HW2.indicesLesserX(10, [| 1; 11; 15; 9; 8 |])) "Resulting array should be [| 0; 3; 4 |]"
        testCase "Taks 3.2 Empty array given" <| fun _ ->
            Expect.equal [||] (HW2.indicesLesserX(10, [||])) "Result should be an empty string"

        testCase "Taks 4.1 Common case" <| fun _ ->
            Expect.equal [| 0; 2; 4 |] (HW2.indicesNotFromRangeXY(10, 12, [| 1; 11; 15; 10; 8 |])) "Resulting array should be [| 0; 2; 4 |]"
        testCase "Taks 4.2 Empty array given" <| fun _ ->
            Expect.equal [||] (HW2.indicesLesserX(10, [||])) "Result should be an empty string"

        testCase "Taks 5.1 Both elements are positive but first is lesser" <| fun _ ->
            Expect.equal [| 2; 1 |] (HW2.swapFS([| 1; 2 |])) "Resulting array should be [| 2; 1 |]"
        testCase "Taks 5.2 Both elements are positive but first is greater" <| fun _ ->
            Expect.equal [| 1; 2 |] (HW2.swapFS([| 2; 1 |])) "Resulting array should be [| 1; 2 |]"
        testCase "Taks 5.3 Both elements are negative but first is lesser by absolute value" <| fun _ ->
            Expect.equal [| -2; -1 |] (HW2.swapFS([| -1; -2 |])) "Resulting array should be [| -2; -1 |]"
        testCase "Taks 5.4 Both elements are negative but first is greater by absolute value" <| fun _ ->
            Expect.equal [| -1; -2 |] (HW2.swapFS([| -2; -1 |])) "Resulting array should be [| -1; -2 |]"
        testCase "Taks 5.5 First is negative, second is positive and greater by absolute value" <| fun _ ->
            Expect.equal [| 2; -1 |] (HW2.swapFS([| -1; 2 |])) "Resulting array should be [| 2; -1 |]"
        testCase "Taks 5.6 First is negative, second is positive and lesser by absolute value" <| fun _ ->
            Expect.equal [| 1; -2 |] (HW2.swapFS([| -2; 1 |])) "Resulting array should be [| 1; -2 |]"
        testCase "Taks 5.7 First is positive, second is negative and greater by absolute value" <| fun _ ->
            Expect.equal [| -2; 1 |] (HW2.swapFS([| 1; -2 |])) "Resulting array should be [| -2; 1 |]"
        testCase "Taks 5.8 First is positive, second is negative and lesser by absolute value" <| fun _ ->
            Expect.equal [| -1; 2 |] (HW2.swapFS([| 2; -1 |])) "Resulting array should be [| -1; 2 |]"

        // swap alrgorithm has already been tested, so it won't be tested for task 6

        testCase "Taks 6.1 i is lesser then j" <| fun _ ->
            Expect.equal [| 1; 4; 3; 2; 5 |] (HW2.swapTwoByIndex(1, 3, [| 1; 2; 3; 4; 5 |])) "Resulting array should be [| 1; 4; 3; 2; 5 |]"
        testCase "Taks 6.2 i is greater then j" <| fun _ ->
            Expect.equal [| 1; 4; 3; 2; 5 |] (HW2.swapTwoByIndex(3, 1, [| 1; 2; 3; 4; 5 |])) "Resulting array should be [| 1; 4; 3; 2; 5 |]"
        ]
