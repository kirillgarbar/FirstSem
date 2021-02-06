module BigInt

open MyList

type sign =
    | Positive
    | Negative

[<Struct>]
type BigInt =
    val Sign:sign
    val Numbers:MyList<int>
    new (s, b) = {
        Sign = if b = One 0 then Positive else s
        Numbers =
            if (fold (fun _ i -> if (i < 0 || i > 9) then false else true) true b)
            then
                b
            else
                failwith "Numbers should be in range 0..9"
        }

let rec equal (x:BigInt) (y:BigInt) =
    if x.Sign <> y.Sign
    then
        false
    else
    match x.Numbers with
    | One x1 ->
        match y.Numbers with
        | One y1 -> x1 = y1
        | Cons _ -> false
    | Cons(x1, tailx) ->
        match y.Numbers with
        | One _ -> false
        | Cons (y1, taily) -> if x1 <> y1 then false else equal (BigInt(Positive, tailx)) (BigInt(Positive, taily))

let getSign (x:BigInt) =
    if x.Sign = Positive then 1 else -1

let setSign x =
    if x = 1 || x = 0 then Positive elif x = -1 then Negative else failwith "1 or -1 expected"

let reverseSign (x:BigInt) =
    BigInt((if x.Sign = Positive then Negative else Positive), x.Numbers)

let equalize (x, y) = // добавляет нули в начало одного из списков, пока их длина разная
    let rec go x y dif =
        if dif = 0 then (x, y) elif dif < 0 then go (Cons(0, x)) y (dif + 1) else go x (Cons(0, y)) (dif - 1)

    let dif = len x - len y
    go x y dif
    
let rec delZeroHead l = // удаляет все нули из префикса списка
    match l with
    | One i -> l
    | Cons(h, tail) -> if h = 0 then delZeroHead tail else l

let rec addZeroes c l =
    if c <= 0 then l else addZeroes (c - 1) (Cons(0, l))

let notLesser x y = // сравнивает списки в лексикографическом порядке
    let lx = len x
    let ly = len y
    if lx <> ly
    then
        lx > ly
    else
        let rec go x y =    
            match x with
            | One x1 ->
                match y with
                | One y1 -> x1 >= y1
                | Cons _ -> failwith "Impossible case"
            | Cons(x1, tailx) ->
                match y with
                | One _ -> failwith "Impossible case"
                | Cons(y1, taily) -> if x1 = y1 then go tailx taily else x1 >= y1

        go x y

let private manageRemainders head tail =   // Проходит по списку, перекидывая лишнее на следующий разряд
    let _, remainder, folded = fold (fun (counter, r, res) x ->  
        let y = x + r
        if y >= 0
        then
            (counter + 1, y / 10, Cons(y % 10, res))
        else
            (counter + 1, -1, Cons(10 + y, res))) (if head >= 0 then (0, head / 10, One (head % 10)) else (0, -1, One (10 + head))) tail
    delZeroHead (Cons(remainder, folded))

let sumOrSub (x:BigInt) (y:BigInt) operator =
    let xEq, yEq = equalize (x.Numbers, y.Numbers)        // Уровняли списки по длине
    let mapped = map2 (fun x1 y1 -> operator (getSign x * x1) (getSign y * y1)) xEq yEq |> delZeroHead |> reverse     // Сложили/вычли поразрядно и развернули список
    match mapped with
    | One h -> BigInt(setSign(sign h), if h > 9 then Cons(h / 10, One (h % 10)) else One (abs h))
    | Cons(h, tail) ->
        let result = manageRemainders h tail
        match result with                          // Определяем знак числа
        | One h -> BigInt(setSign(sign h), Cons(h / 10, One (abs h % 10)) |> delZeroHead)
        | Cons(h, tail) -> BigInt(setSign(sign h), Cons(abs h, tail))

let sum (x:BigInt) (y:BigInt) =         // Если вычитаемое больше уменьшаемого по модулю, разность "в столбик" не работает
    if x.Sign = y.Sign                  // Поэтому приходится делать проверку на модуль и знак
    then
        if x.Sign = Positive then sumOrSub x y (+)
        else reverseSign (sumOrSub (reverseSign x) (reverseSign y) (+))
    elif x.Sign = Positive
    then
        if notLesser x.Numbers y.Numbers then sumOrSub x y (+)
        else reverseSign (sumOrSub (reverseSign y) x (-))
    else
        if notLesser x.Numbers y.Numbers then reverseSign (sumOrSub (reverseSign x) y (-))
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
                sum r (BigInt(Positive, result))
            | Cons(h, tail) ->
                let result = manageRemainders h tail
                sum r (BigInt(Positive, result))
        | Cons(y1, taily) ->
            let mapped = map (fun x1 -> x1 * y1) x |> delZeroHead |> reverse |> addZeroes rank
            match mapped with
            | One h ->
                let result = Cons(h / 10, One (h % 10)) |> delZeroHead
                go x taily (sum r (BigInt(Positive, result))) (rank + 1)
            | Cons(h, tail) ->
                let result = manageRemainders h tail
                go x taily (sum r (BigInt(Positive, result))) (rank + 1)

    let resultPlusOne = go x.Numbers (y.Numbers |> reverse) (BigInt(Positive, One 1)) 0
    let result = sub resultPlusOne (BigInt(Positive, One 1))
    BigInt(setSign(getSign x * getSign y), result.Numbers)

let div (x:BigInt) (y:BigInt) =
    let divide x y =            // Находит частное(от 0 до 9) и остаток от деления.
        let mutable down = 1    // Применяется только если длина делимого равна или больше на 1, чем у делителя
        let mutable up = 10
        while up - down > 1 do
            let r = BigInt(Positive, intToMyList ((up + down) / 2))
            let f = (mul (BigInt(Positive, y)) r)
            if notLesser x f.Numbers
            then
                down <- ((up + down) / 2)
            else
                up <- ((up + down) / 2)
        let m = (up + down) / 2
        let z = mul (BigInt(Positive, y)) (BigInt(Positive, One m))
        let remainder = sub (BigInt(Positive, x)) z
        (m, remainder.Numbers)

    let rSign = setSign(getSign x * getSign y)
    match x.Numbers with
    | One x1 ->
        match y.Numbers with
        | One y1 ->
            if y1 = 0 then failwith "Division by zero"
            else BigInt(rSign, One (x1 / y1))
        | Cons _ ->
            BigInt(Positive, One 0)
    | Cons _ ->
        if y.Numbers = One 0
        then
            failwith "Division by zero"
        else 
            let divisorLen = len y.Numbers
            let _, res, _, c = fold (fun (dividend, result, divisorLen, c) x1 ->    // Отрезаем от делимого числа до тех пор, пока не получится...
                let newC = c + 1                                                    // ...использовать divide и добавляем нули, если было занято более 1 разряда за раз
                let newRes = if newC >= 2 then Cons(0, result) else result
                let newDividend = concat dividend (One x1) |> delZeroHead
                if len newDividend > divisorLen || (len newDividend = divisorLen && notLesser newDividend y.Numbers)
                then
                    let m, rem = divide newDividend y.Numbers
                    (rem, Cons(m, newRes), divisorLen, 0)
                else
                    (newDividend, newRes, divisorLen, c + 1)) (One 0, One 0, divisorLen, 0) x.Numbers
            let newRes = addZeroes (if c > 0 then 1 else 0) res
            match reverse newRes with
            | One _ -> BigInt(rSign, res)
            | Cons (_, tailr) -> BigInt(rSign, delZeroHead tailr)
