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
    
    let generateMatrix rows cols sparcity valueGenerator =
        if rows <= 0 || cols <= 0 then failwith "Size should be positive"
        if sparcity > 100 || sparcity < 0 then failwith "Sparcity should be between 0 and 100"

        let mutable nonZeroElementCount = if sparcity = 100 then 0 else rows * cols / (100 / (100 - sparcity))
        let nonZeroElements = [ for i in 1..nonZeroElementCount -> valueGenerator() ]
        let resultingArray = Array.init rows (fun _ -> Array.create cols "0")

        for element in nonZeroElements do
            resultingArray.[Random().Next(rows)].[Random().Next(cols)] <- element
        resultingArray

    let writeMatrix path (m:string [][]) =
        let writeArray = Array.map (String.concat " ") m
        IO.File.WriteAllLines(path, writeArray)
