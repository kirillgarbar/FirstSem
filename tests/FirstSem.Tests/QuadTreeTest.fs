module QuadTreeTest

open Expecto
open SparseMatrix
open QuadTreeMatrix
open Group

let genRandomSparseMatrix size =
    let rnd = System.Random()
    let rec go l c =
          if c = 0 then l
          else go (CRV(rnd.Next(0, size), rnd.Next(0, size), rnd.Next(1, 100)) :: l) (c - 1)
 
    let count = rnd.Next(0, size*size)
    SparseMatrix(size, go [] count)

let mat a b =
    if a < 0 || b < 0
    then
        failwith "Size of matrix must be positive"
    else
        let m = Array2D.zeroCreate a b 
        m

let genMatrixBySparseMatrix (sm:SparseMatrix<int>) =
    let m = mat sm.Size sm.Size
    for rcv in sm.List do
        m.[rcv.Col, rcv.Row] <- rcv.Value
    m

let sumMat (a:int[,]) (b:int[,]) =
    let r = Array2D.copy a
    if Array2D.length1 a <> Array2D.length1 b && Array2D.length2 a <> Array2D.length2 b then failwith "Size of matrices should be equal"
    else
        for i in 0 .. Array2D.length1 a - 1 do
            for j in 0 .. Array2D.length2 a - 1 do
                r.[i, j] <- r.[i, j] + b.[i, j]
    r
                
let mulMat (a:int[,]) (b:int[,]) =
    if Array2D.length1 a = 0 || Array2D.length1 b = 0 || Array2D.length2 a = 0 || Array2D.length2 b = 0
    then failwith "Matrices should not be empty"
    elif Array2D.length2 a <> Array2D.length1 b
    then
        failwith "The number of columns in a is not equal to the number of rows in b"       
    else
        let m = mat (Array2D.length2 a) (Array2D.length1 b)
        for i = 0 to Array2D.length2 a - 1 do
            for j = 0 to Array2D.length1 b - 1 do
                for k = 0 to Array2D.length1 a - 1 do
                    m.[j, i] <- m.[j, i] + a.[k, i] * b.[j, k]
        m

let multiplyByScalar s (x:int[,]) =
    let y = Array2D.copy x
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            y.[i, j] <- s * y.[i, j]
    y

let tensor (o:int[,]) (t:int[,]) =
    let mutable count1, count2 = 0, 0
    let output = Array2D.create (Array2D.length1 o * Array2D.length2 t) (Array2D.length1 o * Array2D.length2 t) 1
    for i in 0 .. Array2D.length1 o - 1 do
        for j in 0 .. Array2D.length2 o - 1 do
            count1 <- i * (Array2D.length1 t) 
            count2 <- j * (Array2D.length2 t)
            Array2D.blit (multiplyByScalar o.[i,j] t) 0 0 output count1 count2 (Array2D.length1 t) (Array2D.length2 t)
    output

let array2DToSparseMatrix (x:int[,]) =
    let a = Array.create (Array2D.length1 x * Array2D.length2 x) (CRV(0, 0, 0))
    let mutable c = 0
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            a.[c] <- CRV(i, j, x.[i,j])
            c <- c + 1
    SparseMatrix(Array2D.length1 x, a |> Array.filter (fun x -> x.Value <> 0) |> Array.toList)

let qtToSparseMatrix size (x:QuadTree<int>) =
    let a = Array.create (size * size) (CRV(0, 0, 0))
    let mutable c = 0
    for i in 0 .. size - 1 do
        for j in 0 .. size - 1 do
            let y = x.get (i, j) size
            match y with
            | Leaf x ->
                a.[c] <- (CRV(i, j, x))
                c <- c + 1
            | _ -> c <- c
    SparseMatrix(size, a |> Array.filter (fun x -> x.Value <> 0) |> Array.toList)

let sr = new SemiRing<int>(new Monoid<int>((+), 0), (*))
let testRing = SemiRing sr

[<Tests>]
let tests =
    testList "Tests for QuadTreeMatrix" [
        testProperty "initQT test" <| fun _ ->
            let size = System.Random().Next(0, 65) |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size
            let m1 = genMatrixBySparseMatrix sm1
            let r = QuadTree.initQT sm1 |> qtToSparseMatrix size |> genMatrixBySparseMatrix
            Expect.equal m1 r "initQT is wrong"

        testProperty "get/set test" <| fun _ ->
            let size = System.Random().Next(0, 65) |> roundToPowerOfTwo
            let i = System.Random().Next(0, size)
            let j = System.Random().Next(0, size)
            let v = System.Random().Next(0, size)
            let sm1 = genRandomSparseMatrix size
            let t = QuadTree.initQT sm1
            let g = (t.set (CRV(i, j, v)) size).get (i, j) size
            let r = 
                match g with
                | None -> v - 1
                | Leaf x -> x
            Expect.equal v r "get/set is wrong"
          
        testProperty "sum test" <| fun _ ->
            let size = System.Random().Next(0, 65) |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size
            let sm2 = genRandomSparseMatrix size
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let sum1 = sumMat m1 m2 |> array2DToSparseMatrix |> QuadTree.initQT
            let qt1 = QuadTree.initQT sm1
            let qt2 = QuadTree.initQT sm2
            let sum2 = QuadTree.sum qt1 qt2 testRing
            Expect.equal sum1 sum2 "sum is wrong"

        testProperty "mul test" <| fun _ ->
            let size = System.Random().Next(0, 65) |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size
            let sm2 = genRandomSparseMatrix size
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let mul1 = mulMat m1 m2 |> array2DToSparseMatrix |> QuadTree.initQT
            let qt1 = QuadTree.initQT sm1
            let qt2 = QuadTree.initQT sm2
            let mul2 = QuadTree.mul qt1 qt2 testRing
            Expect.equal mul1 mul2 "sum is wrong"
            
        testProperty "scalarMul test" <| fun _ ->
            let size = System.Random().Next(0, 65) |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size
            let s = System.Random().Next(0, 10)
            let m1 = genMatrixBySparseMatrix sm1
            let mul1 = multiplyByScalar s m1 |> array2DToSparseMatrix |> QuadTree.initQT
            let qt1 = QuadTree.initQT sm1
            let mul2 = qt1.scalarMul s testRing
            Expect.equal mul1 mul2 "scalarMul is wrong"

        testProperty "tensorMul test" <| fun _ ->
            let size = System.Random().Next(0, 33) |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size
            let sm2 = genRandomSparseMatrix size
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let mul1 = tensor m1 m2 |> array2DToSparseMatrix |> QuadTree.initQT
            let qt1 = QuadTree.initQT sm1
            let qt2 = QuadTree.initQT sm2
            let mul2 = QuadTree.tensorMul qt1 qt2 testRing
            Expect.equal mul1 mul2 "tensorMul is wrong"

    ]

