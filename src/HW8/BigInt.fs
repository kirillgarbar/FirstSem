module BigInt

open MyList

[<Struct>]
type BigInt =
    val Sign:int
    val Bits:MyList<int>
    new (s, b) = {
        Sign = (if b = One 0 then 1 elif s = 1 || s = -1 then s else failwith "Sign should be 1 or -1");
        Bits =
            if (fold (fun _ i -> if (i < 0 || i > 9) then false else true) true b)
            then
                b
            else
                failwith "Numbers should be in range 0..9"
        }

let notLesser x y = // сравнивает списки в лексикографическом порядке
    let lx = len x
    let ly = len y
    if lx > ly
    then
        true
    elif ly > lx
    then
        false
    else
        let rec go x y =
            match x with
            | One x1 ->
                match y with
                | One y1 -> x1 >= y1
            | Cons(x1, tailx) ->
                match y with
                | Cons(y1, taily) -> if x1 = y1 then go tailx taily else x1 >= y1

        go x y

let reverseSign (x:BigInt) =
    BigInt(x.Sign * -1, x.Bits)

let sign i = if i < 0 then -1 else 1
            
let sumOrSub (x:BigInt) (y:BigInt) operator = 
    let xe, ye = equalize x.Bits y.Bits // выравниваем два числа
    let mapped = map2 (fun x1 y1 -> operator (x.Sign * x1) (y.Sign * y1)) xe ye |> addZeroes 1 |> reverse // суммируем/вычитаем поразрядно
    match mapped with
    | One i -> if i < -9 then BigInt(-1, Cons(1, One (abs i%10))) elif i > 9 then BigInt(1, Cons(1, One (i%10))) else BigInt(sign i, One (abs i))
    | Cons(i, tail) ->
        let last = len mapped - 2
        let _, _, folded = fold (fun (c, remainder, l) i -> // переносим остатки на следующие разряды
            let j = remainder + i
            if c = last && j < 0
            then
                (0, 0, Cons(j, l))
            else
                (c + 1, ((if j < 0 then -1 else j/10)), Cons((j + 10) % 10, l))) (1, (if i < 0 then -1 else i/10), One ((i + 10) % 10)) tail
        let r = delZeroHead folded
        match r with
        | One i -> BigInt(sign i, One (abs i))
        | Cons(i, tail) -> BigInt(sign i, Cons(abs(i), tail))

let sum (x:BigInt) (y:BigInt) =
    if x.Sign = y.Sign
    then
        if x.Sign = 1 then sumOrSub x y (+)
        else reverseSign (sumOrSub (reverseSign x) (reverseSign y) (+))
    else
        if x.Sign = 1
        then
            if notLesser x.Bits y.Bits then sumOrSub x y (+)
            else reverseSign (sumOrSub (reverseSign y) x (-))
        else
            if notLesser x.Bits y.Bits then reverseSign (sumOrSub (reverseSign x) y (-))
            else sumOrSub y x (+)

let sub (x:BigInt) (y:BigInt) =
    if x.Sign = y.Sign
    then
        if x.Sign = 1
        then
            sum x (reverseSign y)
        else
            sum (reverseSign y) x
    else
        if x.Sign = 1
        then
            sum x (reverseSign y)
        else
            reverseSign (sum (reverseSign x) y)

let mul (x:BigInt) (y:BigInt) =
    let rec go x y r rank =
        match y with
        | One y1 ->
            let mapped = map (fun x1 -> x1 * y1 * rank) x |> addZeroes ((len y) + rank/10) |> reverse       // поразрядно умножаем
            match mapped with
            | Cons(x1, tail) ->
                let _, folded = fold (fun (remainder, l) i ->       // переносим остатки
                    let j = remainder + i
                    (j / 10, Cons(j%10, l))) (x1/10, One (x1%10)) tail
                (sum (BigInt(1, r)) (BigInt(1, delZeroHead folded))).Bits
        | Cons(y1, taily) -> 
            let mapped = map (fun x1 -> x1 * y1 * rank) x |> addZeroes ((len y) + rank/10) |> reverse       // поразрядно умножаем
            match mapped with
            | Cons(x1, tailx) ->    
                let _, folded = fold (fun (remainder, l) i ->       // переносим остатки
                    let j = remainder + i
                    (j / 10, Cons(j%10, l))) (x1/10, One (x1%10)) tailx
                go x taily (sum (BigInt(1, r)) (BigInt(1, delZeroHead folded))).Bits (rank*10)
                
    match x.Bits with
    | One x1 ->
        match y.Bits with
        | One y1 -> 
            BigInt(x.Sign * y.Sign, Cons(x1*y1/10, One (x1*y1%10)) |> delZeroHead)
        | Cons(_, _) ->
            BigInt(x.Sign * y.Sign, go y.Bits x.Bits (One 0) 1)
    | Cons(_, _) ->
        match y.Bits with
        | One y1 -> 
            BigInt(x.Sign * y.Sign, go x.Bits y.Bits (One 0) 1)
        | Cons(_, _) ->
            BigInt(x.Sign * y.Sign, go x.Bits (reverse y.Bits) (One 0) 1)

let div (x:BigInt) (y:BigInt) =
    let divide x y =    // функция, находящая частное(от 0 до 9) и остаток от деления
        let mutable down = 1
        let mutable up = 10
        while up - down > 1 do
            let r = BigInt(1, intToMyList ((up + down) / 2))
            let f = (mul (BigInt(1, y)) r)
            if notLesser x f.Bits
            then
                down <- ((up + down) / 2)
            else
                up <- ((up + down) / 2)
        let m = (up + down) / 2
        let z = mul (BigInt(1, y)) (BigInt(1, One m))
        let remainder = sub (BigInt(1, x)) z
        (m, remainder.Bits)

    let rSign = x.Sign * y.Sign
    match x.Bits with
    | One x1 ->
        match y.Bits with
        | One y1 ->
            if y1 = 0 then failwith "Division by zero"
            else BigInt(rSign, One (x1 / y1))
        | Cons(_, _) ->
            BigInt(1, One 0)
    | Cons(_, _) ->
        if y.Bits = One 0 then failwith "Division by zero"
        else 
            let divLen = len y.Bits
            let _, res, _, c = fold (fun (dividend, result, divLen, c) x1 ->    // отрезаем от делимого числа до тех пор, пока не получится
                let newC = c + 1                                                // использовать divide и добавляем нули, если было занято более 1 разряда подряд
                let newRes = if newC >= 2 then Cons(0, result) else result
                let divd = concat dividend (One x1) |> delZeroHead
                if notLesser divd y.Bits
                then
                    let m, rem = divide divd y.Bits
                    (rem, Cons(m, newRes), divLen, 0)
                else
                    (divd, newRes, divLen, c + 1)) (One 0, One 0, divLen, 0) x.Bits
            let newRes = addZeroes (if c > 0 then 1 else 0) res
            match reverse newRes with
            | One r1 -> BigInt(rSign, res)
            | Cons(r1, tailr) -> BigInt(rSign, delZeroHead tailr)



            
