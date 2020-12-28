module Main

open BigInt
open MyList

[<EntryPoint>]
let main (argv: string array) =
    let x = BigInt(1, Cons(8, Cons(8, Cons(1, Cons(0, One 6)))))
    let y = BigInt(1, Cons(1, Cons(1, One 0)))
    let s = div x y
    printfn "%A" (s.Bits, s.Sign)
    0
