module MatrixGeneratorTest

open Expecto
open System
open FirstSem.Generator

let readMatrix path = Array.map (fun (s:string) -> s.Split ' ') (IO.File.ReadAllLines path)

[<Tests>]
let tests =
    testList "Tests for MatrixGenetator" [
    testProperty "write and read id test for Int matrices" <| fun (rows, cols, sparsity) ->
       let im = generateMatrix (abs(rows) % 10 + 1) (abs(cols) % 10 + 1) (abs(sparsity) % 101) Int |> Array.ofSeq |> Array.map (Array.ofSeq)
       writeMatrix "GeneratorTestInt.txt" im
       writeMatrix "GeneratorTestInt2.txt" im
       let rim = readMatrix "GeneratorTestInt.txt"
       Expect.equal rim im "write and read should be id"

    testProperty "write and read id test for Bool matrices" <| fun (rows, cols, sparsity) ->
        let bm = generateMatrix (abs(rows) % 10 + 1) (abs(cols) % 10 + 1) (abs(sparsity) % 101) Bool |> Array.ofSeq |> Array.map (Array.ofSeq)
        writeMatrix "GeneratorTestBool.txt" bm
        let rbm = readMatrix "GeneratorTestBool.txt"
        Expect.equal rbm bm "write and read should be id"

    testProperty "write and read id test for Float matrices" <| fun (rows, cols, sparsity) ->
        let fm = generateMatrix (abs(rows) % 10 + 1) (abs(cols) % 10 + 1) (abs(sparsity) % 101) Float |> Array.ofSeq |> Array.map (Array.ofSeq)
        writeMatrix "GeneratorTestFloat.txt" fm
        let rfm = readMatrix "GeneratorTestFloat.txt"
        Expect.equal rfm fm "write and read should be id"

    testCase "Zero dimensional matrix test" <| fun _ ->
        Expect.throws (fun _ -> generateMatrix 0 5 10 Int |> ignore) "Exception should be raised"
    ]
