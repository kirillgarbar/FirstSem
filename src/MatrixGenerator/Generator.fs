namespace FirstSem

open System

module Generator =
    let signGenerator() = if Random().Next(2) = 0 then 1 else -1
    
    let intGenerator() = Random().Next(1, 1000) * signGenerator() |> string
    
    let boolGenerator() = "1"
    
    let floatGenerator() =
        let whole = Random().Next(1000) |> string
        let fractional = Random().Next(1000) |> string
        (whole + "." + fractional |> float) * (signGenerator() |> float) |> string
    
    let matrixToString (a:string [][]) = Array.fold (fun s i -> s + (Array.fold (fun s1 j -> s1 + j + " ") "" i).Trim() + "\n") "" a
    
    let generateMatrix rows cols sparcity valueGenerator =
        if rows <= 0 || cols <= 0 then failwith "Size should be positive"
        if sparcity > 100 || sparcity < 0 then failwith "Sparcity should be between 0 and 100"

        let mutable nonZeroElementCount = if sparcity = 100 then 0 else rows * cols / (100 / (100 - sparcity))
        let nonZeroElements = [ for i in 1..nonZeroElementCount -> valueGenerator() ]
        let resultingArray = Array.init rows (fun _ -> Array.create cols "0")
        let slots = [| for i in 0..rows - 1 do for j in 0..cols - 1 -> (i, j) |]

        let rec placeValue (slots:(int*int) []) nonZeroElements = 
            if slots.Length = 0 
            then 
                1 
            else
                let x = Random().Next(slots.Length)
                let slot = slots.[x]
                match nonZeroElements with
                | h :: t -> 
                    resultingArray.[fst slot].[snd slot] <- h
                    placeValue (Array.append (slots.[..x - 1]) (slots.[x + 1..])) t
                | [] -> 1
    
        placeValue slots nonZeroElements |> ignore
        resultingArray

    let writeMatrix path (m:string [][]) = IO.File.WriteAllText(path, matrixToString m)
