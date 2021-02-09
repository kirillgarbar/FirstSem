module TestsHW8

open Expecto
open MyList
open BigInt
open System.Numerics

let genRandomList() =
    let rnd = System.Random()
    List.init (rnd.Next(1, 40)) (fun _ -> rnd.Next(9))

let genRandomBigInteger() =
    fst (genRandomList() |> List.fold (fun (i, p) x -> (i + (BigInteger x)*p, p*(BigInteger 10))) (BigInteger 0, BigInteger 1))

let bigIntegerToBigInt (x:BigInteger) =
    let y = x |> string
    let newX = if y.[0] = '-' then y.[1..] else y
    let list = newX |> List.ofSeq |> List.map (fun i -> i |> string |> int) |> listToMyList
    BigInt((if x >= BigInteger 0 then Positive else Negative), list)

[<Tests>]
let tests =
    testList "Tests for BigInt" [
        testCase "equal test" <| fun _ ->
            let a = BigInt(Positive, Cons(1, One 2))
            let a1 = BigInt(Positive, Cons(1, One 2))
            let a2 = BigInt(Negative, Cons(1, One 2))
            let a3 = BigInt(Positive, Cons(1, One 1))
            let a4 = BigInt(Negative, Cons(1, One 1))
            Expect.isTrue (equal a a1) "euqal is wrong"
            Expect.isFalse (equal a a2) "euqal are wrong"
            Expect.isFalse (equal a a3) "euqal are wrong"
            Expect.isFalse (equal a a4) "euqal are wrong"

        testCase "bigIntegerToBigInt test" <| fun _ ->
            let a = 0 |> BigInteger
            let a1 = a |> bigIntegerToBigInt
            let b = 123 |> BigInteger
            let b1 = b |> bigIntegerToBigInt
            let c = -123 |> BigInteger
            let c1 = c |> bigIntegerToBigInt
            Expect.equal Positive a1.Sign "Signs are wrong"
            Expect.equal Positive b1.Sign "Signs are wrong"
            Expect.equal Negative c1.Sign "Signs are wrong"
            Expect.isTrue (equal (BigInt(Positive, One 0)) a1) "bigIntegerToBigInt is wrong"
            Expect.isTrue (equal (BigInt(Positive, Cons(1, Cons(2, One 3)))) b1) "bigIntegerToBigInt is wrong"
            Expect.isTrue (equal (BigInt(Negative, Cons(1, Cons(2, One 3)))) c1) "bigIntegerToBigInt is wrong"

        testCase "notLesser test" <| fun _ ->
            let a = Cons(1, Cons(1, One 0))
            let b = Cons(1, Cons(1, One 0))
            let c = Cons(1, Cons(0, One 9))
            let d = Cons(9, One 9)
            Expect.isTrue (notLesser a b) "notLesser is wrong"
            Expect.isTrue (notLesser a c) "notLesser is wrong"
            Expect.isTrue (notLesser a d) "notLesser is wrong"

        testCase "reverseSign test" <| fun _ ->
            let b = BigInt(Negative, One 0)
            Expect.equal Positive (reverseSign b).Sign "reverseSign is wrong"

        testProperty "equalize test" <| fun (a, b) ->
            let l1 = genRandomList a |> listToMyList
            let l2 = genRandomList b |> listToMyList
            let eq1, eq2 = equalize (l1, l2)
            Expect.equal (len eq1) (len eq2) "len eq1 =/= len eq2"

        testCase "delZeroHead test" <| fun _ ->
            let l = genRandomList() |> listToMyList |> delZeroHead |> myListToList
            let lc = concat (Cons(0, Cons(0, One 0))) (l |> listToMyList)
            let r = delZeroHead lc |> myListToList
            Expect.sequenceEqual l r "delZeroHead is wrong"

        testCase "addZeroes test" <| fun _ ->
            let l = genRandomList() |> listToMyList |> addZeroes 3 |> myListToList
            Expect.sequenceEqual l.[0..2] [0;0;0] "addZeroes is wrong"
            
        testProperty "sum test" <| fun _ ->
            let x = genRandomBigInteger()
            let y = genRandomBigInteger()
            let s = x + y
            let s1 = (BigInteger -1) * x + y
            let s2 = x + y * (BigInteger -1)
            let s3 = (BigInteger -1) * x + y * (BigInteger -1)
            let x1 = x |> bigIntegerToBigInt
            let y1 = y |> bigIntegerToBigInt
            let sb = sum x1 y1
            let sb1 = sum (reverseSign x1) y1
            let sb2 = sum x1 (reverseSign y1)
            let sb3 = sum (reverseSign x1) (reverseSign y1)
            Expect.isTrue (equal (bigIntegerToBigInt s) sb) "sum is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s1) sb1) "sum is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s2) sb2) "sum is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s3) sb3) "sum is wrong"
            
        testProperty "sub test" <| fun _ ->
            let x = genRandomBigInteger()
            let y = genRandomBigInteger()
            let s = x - y
            let s1 = (BigInteger -1) * x - y
            let s2 = x - y * (BigInteger -1)
            let s3 = (BigInteger -1) * x - y * (BigInteger -1)
            let x1 = x |> bigIntegerToBigInt
            let y1 = y |> bigIntegerToBigInt
            let sb = sub x1 y1
            let sb1 = sub (reverseSign x1) y1
            let sb2 = sub x1 (reverseSign y1)
            let sb3 = sub (reverseSign x1) (reverseSign y1)
            Expect.isTrue (equal (bigIntegerToBigInt s) sb) "sub is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s1) sb1) "sub is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s2) sb2) "sub is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s3) sb3) "sub is wrong"
            
        testProperty "mul test" <| fun _ ->
            let x = genRandomBigInteger()
            let y = genRandomBigInteger()
            let s = x * y
            let s1 = (BigInteger -1) * x * y
            let s2 = x * y * (BigInteger -1)
            let s3 = (BigInteger -1) * x * y * (BigInteger -1)
            let x1 = x |> bigIntegerToBigInt
            let y1 = y |> bigIntegerToBigInt
            let sb = mul x1 y1
            let sb1 = mul (reverseSign x1) y1
            let sb2 = mul x1 (reverseSign y1)
            let sb3 = mul (reverseSign x1) (reverseSign y1)
            Expect.isTrue (equal (bigIntegerToBigInt s) sb) "mul is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s1) sb1) "mul is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s2) sb2) "mul is wrong"
            Expect.isTrue (equal (bigIntegerToBigInt s3) sb3) "mul is wrong"
            
        testProperty "div test" <| fun _ ->
            let x = genRandomBigInteger()
            let y = genRandomBigInteger()
            if y <> (BigInteger 0)
            then
                let s = x / y
                let s1 = (BigInteger -1) * x / y
                let s2 = x / y * (BigInteger -1)
                let s3 = (BigInteger -1) * x / y * (BigInteger -1)
                let x1 = x |> bigIntegerToBigInt
                let y1 = y |> bigIntegerToBigInt
                let sb = div x1 y1
                let sb1 = div (reverseSign x1) y1
                let sb2 = div x1 (reverseSign y1)
                let sb3 = div (reverseSign x1) (reverseSign y1)
                Expect.isTrue (equal (bigIntegerToBigInt s) sb) "div is wrong"
                Expect.isTrue (equal (bigIntegerToBigInt s1) sb1) "div is wrong"
                Expect.isTrue (equal (bigIntegerToBigInt s2) sb2) "div is wrong"
                Expect.isTrue (equal (bigIntegerToBigInt s3) sb3) "div is wrong"
            else
                Expect.isTrue true ""

        testCase "div test. Division by zero" <| fun _ ->
            Expect.throws (fun _ -> div (BigInt(Positive, One 1)) (BigInt(Positive, One 0)) |> ignore) "Exception should be raised"
            Expect.throws (fun _ -> div (BigInt(Positive, Cons(1, One 1))) (BigInt(Positive, One 0)) |> ignore) "Exception should be raised"
    ]

