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

let reverseSign (x:BigInt) =
    BigInt(x.Sign * -1, x.Bits)

let private manageRemainders head tail last =   // Проходит по списку, перекидывая лишнее на следующий разряд
    let _, remainder, folded = fold (fun (counter, r, res) x ->  
        let y = x + r
        if counter = last && y < 0
        then
            (counter + 1, 0, Cons(y, res))
        elif y >= 0
        then
            (counter + 1, y / 10, Cons(y % 10, res))
        else
            (counter + 1, -1, Cons(10 + y, res))) (if head >= 0 then (0, head / 10, One (head % 10)) else (0, -1, One (10 + head))) tail
    delZeroHead (Cons(remainder, folded))

let sumOrSub (x:BigInt) (y:BigInt) operator =
    let xEq, yEq = equalize (x.Bits, y.Bits)        // Уровняли списки по длине
    let mapped = map2 (fun x1 y1 -> operator (x.Sign * x1) (y.Sign * y1)) xEq yEq |> delZeroHead |> reverse     // Сложили/вычли поразрядно и развернули список
    match mapped with
    | One h -> BigInt(sign h, if h > 9 then Cons(h / 10, One (h % 10)) else One (abs h))
    | Cons(h, tail) ->
        let last = len mapped - 2
        let result = manageRemainders h tail last
        match result with                          // Определяем знак числа
        | One h -> BigInt(sign h, Cons(h / 10, One (abs h % 10)) |> delZeroHead)
        | Cons(h, tail) -> BigInt(sign h, Cons(abs h, tail))

let sum (x:BigInt) (y:BigInt) =         // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    if x.Sign = y.Sign                  // Поэтому приходится делать проверку на модуль и знак
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

let sub (x:BigInt) (y:BigInt) = sum x (reverseSign y)

let mul (x:BigInt) (y:BigInt) =
    let rec go x y r rank =
        match y with
        | One y1 ->
            let mapped = map (fun x1 -> x1 * y1) x |> delZeroHead |> reverse |> addZeroes rank  // Умножаем поразрядно, добавляя нули в конец числа в соответствии с разрядом множителя
            match mapped with
            | One h ->
                let result = Cons(h / 10, One (h % 10)) |> delZeroHead
                sum r (BigInt(1, result))
            | Cons(h, tail) ->
                let result = manageRemainders h tail 0
                sum r (BigInt(1, result))
        | Cons(y1, taily) ->
            let mapped = map (fun x1 -> x1 * y1) x |> delZeroHead |> reverse |> addZeroes rank
            match mapped with
            | One h ->
                let result = Cons(h / 10, One (h % 10)) |> delZeroHead
                go x taily (sum r (BigInt(1, result))) (rank + 1)
            | Cons(h, tail) ->
                let result = manageRemainders h tail 0
                go x taily (sum r (BigInt(1, result))) (rank + 1)

    let resultPlusOne = go x.Bits (y.Bits |> reverse) (BigInt(1, One 1)) 0
    let result = sub resultPlusOne (BigInt(1, One 1))
    BigInt(x.Sign * y.Sign, result.Bits)

let div (x:BigInt) (y:BigInt) =
    let divide x y =            // Находит частное(от 0 до 9) и остаток от деления.
        let mutable down = 1    // Применяется только если длина делимого равна или больше на 1, чем у делителя
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
            let divisorLen = len y.Bits
            let _, res, _, c = fold (fun (dividend, result, divisorLen, c) x1 ->    // Отрезаем от делимого числа до тех пор, пока не получится...
                let newC = c + 1                                                    // ...использовать divide и добавляем нули, если было занято более 1 разряда за раз
                let newRes = if newC >= 2 then Cons(0, result) else result
                let newDividend = concat dividend (One x1) |> delZeroHead
                if len newDividend > divisorLen || (len newDividend = divisorLen && notLesser newDividend y.Bits)
                then
                    let m, rem = divide newDividend y.Bits
                    (rem, Cons(m, newRes), divisorLen, 0)
                else
                    (newDividend, newRes, divisorLen, c + 1)) (One 0, One 0, divisorLen, 0) x.Bits
            let newRes = addZeroes (if c > 0 then 1 else 0) res
            match reverse newRes with
            | One r1 -> BigInt(rSign, res)
            | Cons(r1, tailr) -> BigInt(rSign, delZeroHead tailr)
