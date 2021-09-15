module QuadTreeTest

open Expecto
open SparseMatrix
open QuadTreeMatrix
open AlgebraicStruct

let genRandomSparseMatrix cols rows =
    let rnd = System.Random()
    let rec go l c =
          if c = 0 then l
          else go (CRV(rnd.Next(0, cols), rnd.Next(0, rows), rnd.Next(1, 100)) :: l) (c - 1)
 
    let count = rnd.Next(0, cols*rows)
    SparseMatrix(cols, rows, go [] count)

let mat rows cols =
    if cols < 0 || rows < 0
    then
        failwith "Size of matrix must be positive"
    else
        let m = Array2D.zeroCreate rows cols
        m

let genMatrixBySparseMatrix (sm:SparseMatrix<int>) =
    let m = mat sm.Rows sm.Cols
    for crv in sm.List do
        m.[crv.Row, crv.Col] <- crv.Value
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
        let m = mat (Array2D.length1 a) (Array2D.length2 b)
        for i = 0 to Array2D.length1 m - 1 do
            for j = 0 to Array2D.length2 m - 1 do
                for k = 0 to Array2D.length2 a - 1 do
                    m.[i, j] <- m.[i, j] + a.[i, k] * b.[k, j]
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
    for i in 0 .. Array2D.length2 x - 1 do
        for j in 0 .. Array2D.length1 x - 1 do
            a.[c] <- CRV(i, j, x.[j,i])
            c <- c + 1
    SparseMatrix(Array2D.length2 x, Array2D.length1 x, a |> Array.filter (fun x -> x.Value <> 0) |> Array.toList)

let qtmToSparseMatrix (x:QuadTreeMatrix<int>) =
    let a = Array.create (x.Cols * x.Rows) (CRV(0, 0, 0))
    let mutable c = 0
    for i in 0 .. x.Cols - 1 do
        for j in 0 .. x.Rows - 1 do
            let y = x.get (i, j)
            match y with
            | Leaf x ->
                a.[c] <- (CRV(i, j, x))
                c <- c + 1
            | _ -> c <- c
    SparseMatrix(x.Cols, x.Rows, a |> Array.filter (fun x -> x.Value <> 0) |> Array.toList)

let sr = new SemiRing<int>(new Monoid<int>((+), 0), (*))
let testRing = SemiRing sr

[<Tests>]
let tests =
    testList "Tests for QuadTreeMatrix" [
        testProperty "initQT test" <| fun (a, b) ->
            let cols = abs a % 64
            let rows = abs b % 64
            let sm1 = genRandomSparseMatrix cols rows
            let m1 = genMatrixBySparseMatrix sm1
            let r = QuadTreeMatrix.initQTM sm1 |> qtmToSparseMatrix |> genMatrixBySparseMatrix
            Expect.equal m1 r "initQT is wrong"

        testProperty "get/set test" <| fun (a, b) ->
            let cols = abs a % 64 + 1
            let rows = abs b % 64 + 1
            let i = System.Random().Next(0, cols)
            let j = System.Random().Next(0, rows)
            let v = System.Random().Next(0, 100)
            let sm1 = genRandomSparseMatrix cols rows
            let t = QuadTreeMatrix.initQTM sm1
            t.set (CRV(i, j, v))
            let g = t.get (i, j)
            let r = 
                match g with
                | None -> v - 1
                | Leaf x -> x
                | _ -> failwith "impossible case"
            Expect.equal v r "get/set is wrong"
          
        testProperty "sum test" <| fun (a, b) ->
            let cols = abs a % 64 + 1
            let rows = abs b % 64 + 1
            let sm1 = genRandomSparseMatrix cols rows
            let sm2 = genRandomSparseMatrix cols rows
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let sum1 = sumMat m1 m2 |> array2DToSparseMatrix |> QuadTreeMatrix.initQTM
            let qt1 = QuadTreeMatrix.initQTM sm1
            let qt2 = QuadTreeMatrix.initQTM sm2
            let sum2 = QuadTreeMatrix.sum qt1 qt2 testRing
            Expect.equal sum1 sum2 "sum is wrong"

        testProperty "mul test" <| fun (a, b, c) ->
            let firstDim = abs a % 64 + 1
            let secondDim = abs b % 64 + 1
            let thirdDim = abs c % 64 + 1
            let sm1 = genRandomSparseMatrix firstDim secondDim
            let sm2 = genRandomSparseMatrix thirdDim firstDim
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let mul1 = mulMat m1 m2 |> array2DToSparseMatrix |> QuadTreeMatrix.initQTM
            let qt1 = QuadTreeMatrix.initQTM sm1
            let qt2 = QuadTreeMatrix.initQTM sm2
            let mul2 = QuadTreeMatrix.mul qt1 qt2 testRing
            Expect.equal mul1 mul2 "mul is wrong"
            
        testProperty "scalarMul test" <| fun (a, b) ->
            let cols = abs a % 64 + 1
            let rows = abs b % 64 + 1
            let sm1 = genRandomSparseMatrix cols rows
            let s = System.Random().Next(0, 10)
            let m1 = genMatrixBySparseMatrix sm1
            let mul1 = multiplyByScalar s m1 |> array2DToSparseMatrix |> QuadTreeMatrix.initQTM
            let qt1 = QuadTreeMatrix.initQTM sm1
            let mul2 = qt1.scalarMul s testRing
            Expect.equal mul1 mul2 "scalarMul is wrong"

        testProperty "tensorMul test" <| fun a ->
            let size = abs a % 16 |> roundToPowerOfTwo
            let sm1 = genRandomSparseMatrix size size
            let sm2 = genRandomSparseMatrix size size
            let m1 = genMatrixBySparseMatrix sm1
            let m2 = genMatrixBySparseMatrix sm2
            let mul1 = tensor m1 m2 |> array2DToSparseMatrix |> QuadTreeMatrix.initQTM
            let qt1 = QuadTreeMatrix.initQTM sm1
            let qt2 = QuadTreeMatrix.initQTM sm2
            let mul2 = QuadTreeMatrix.tensorMul qt1 qt2 testRing
            Expect.equal mul1 mul2 "tensorMul is wrong"

    ]

