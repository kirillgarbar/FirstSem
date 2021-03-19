module TestRuFS

open Expecto

// tests for printing and variable declaring
let pr1 =
    """
    let x = -2
    let y = 7
    let z = x + y * 3
    print x
    print y
    print z
    """
Interpreter.run (Main.parse pr1)
printfn "Expected \n-2\n7\n19"

// tests for calculating an expression
[<Tests>]
let tests =
    testList "Tests for RuFS" [
        testCase "Calculation test. Bracket parsing" <| fun _ ->
            let bp = "let x = 5 + 2 * ((4 - 2) - 7 / (3 * 2))"
            Expect.equal "7" (Interpreter.calculate (Main.parse bp) |> BigInt.bigIntToString) "Bracket parsing is wrong"

        testCase "Calculation test. Abs Bracket parsing" <| fun _ ->
            let abp = "let x = 5 + 2 * ||4 - 12| - 7 / |3 * 2||"
            Expect.equal "19" (Interpreter.calculate (Main.parse abp) |> BigInt.bigIntToString) "AbsBracket parsing is wrong"

        testCase "Power priority test" <| fun _ ->                
            let pp = "let x = 2 / 2 ^ 3"
            Expect.equal "0" (Interpreter.calculate (Main.parse pp) |> BigInt.bigIntToString) "Power priority is wrong"

        testCase "Unary minus test" <| fun _ ->                
            let um = "let x = 17 + -42 + +2"
            Expect.equal "-23" (Interpreter.calculate (Main.parse um) |> BigInt.bigIntToString) "Unary minus is wrong"

        testCase "Zero division test" <| fun _ ->                
            let zd = "let x = 13 / (5 - 5)"
            Expect.throws (fun _ -> (Interpreter.calculate (Main.parse zd)) |> ignore) "Exception should be raised"

        testCase "Wrong number test" <| fun _ ->                
            let wn = "let x = -014"
            let wn2 = "let x = 00"
            Expect.throws (fun _ -> (Interpreter.calculate (Main.parse wn)) |> ignore) "Exception should be raised"
            Expect.throws (fun _ -> (Interpreter.calculate (Main.parse wn2)) |> ignore) "Exception should be raised"

        testCase "Calculation test. Common case 1" <| fun _ ->
            let cc1 = "let x = 12 + 3 - 5 + 12 - 25 * (25 + 3) / 14 - 14 * 6 + 7 / 12 - 12 / 2 / 2"
            Expect.equal "-115" (Interpreter.calculate (Main.parse cc1) |> BigInt.bigIntToString) "Common case 1 is wrong"
        testCase "Calculation test. Common case 2" <| fun _ ->
            let cc2 = "let x = 1 + 2 * 3 - 7 / (2 + 5)"
            Expect.equal "6" (Interpreter.calculate (Main.parse cc2) |> BigInt.bigIntToString) "Common case 2 is wrong"
        testCase "Calculation test. Common case 3" <| fun _ ->
            let cc3 = "let x = 2 + 3 ^ |2 - 4|"
            Expect.equal "11" (Interpreter.calculate (Main.parse cc3) |> BigInt.bigIntToString) "Common case 3 is wrong"
    ]
