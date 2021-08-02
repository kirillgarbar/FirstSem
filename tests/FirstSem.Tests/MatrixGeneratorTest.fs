module MatrixGeneratorTest

open Expecto
open System
open FirstSem.Generator

let readMatrix path = Array.map (fun (s:string) -> s.Split ' ') (IO.File.ReadAllLines path)

[<Tests>]
let tests =
    testList "Tests for MatrixGenetator" [
    testProperty "write and read id test" <| fun _ ->
       let im = generateMatrix (Random().Next(1, 10)) (Random().Next(1, 10)) (Random().Next(101)) intGenerator
       let bm = generateMatrix (Random().Next(1, 10)) (Random().Next(1, 10)) (Random().Next(101)) boolGenerator
       let fm = generateMatrix (Random().Next(1, 10)) (Random().Next(1, 10)) (Random().Next(101)) floatGenerator
       writeMatrix "GeneratorTest1.txt" im
       writeMatrix "GeneratorTest2.txt" bm
       writeMatrix "GeneratorTest3.txt" fm
       let rim = readMatrix "GeneratorTest1.txt"
       let rbm = readMatrix "GeneratorTest2.txt"
       let rfm = readMatrix "GeneratorTest3.txt"
       Expect.equal rim im "write and read should be id"
       Expect.equal rbm bm "write and read should be id"
       Expect.equal rfm fm "write and read should be id"

    testCase "Zero dimensional matrix test" <| fun _ ->
        Expect.throws (fun _ -> generateMatrix 0 5 10 intGenerator |> ignore) "Exception should be raised"
    ]
