namespace FirstSem

open System

module Generator =

    type GeneratorType =
        | Int
        | Bool
        | Float

    let signGenerator() = if Random().Next(2) = 0 then 1 else -1
    
    let intGenerator() = Random().Next(1, 1000) * signGenerator() |> string
    
    let boolGenerator() = "1"

    let floatGenerator() =
        let whole = Random().Next(1000) |> string
        let fractional = Random().Next(1000) |> string
        (whole + "." + fractional |> float) * (signGenerator() |> float) |> string
    
    let generateMatrix rows cols sparsity t =
        if rows <= 0 || cols <= 0 then failwith "Size should be positive"
        if sparsity > 100 || sparsity < 0 then failwith "Sparcity should be between 0 and 100"

        seq {
            for _ in 1..rows do
                seq {
                for _ in 1..cols do
                    match t with
                    | Int -> if Random().Next(101) <= sparsity then "0" else intGenerator()
                    | Bool -> if Random().Next(101) <= sparsity then "0" else boolGenerator()
                    | Float -> if Random().Next(101) <= sparsity then "0.0" else floatGenerator()
                }
        }

    let writeMatrix path m =
        let writeSeq = Seq.map (String.concat " ") m
        IO.File.WriteAllLines(path, writeSeq)
