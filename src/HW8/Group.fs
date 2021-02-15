module Group

type Monoid<'t> =
    val Sum: 't -> 't -> 't
    val Neutral: 't
    new (x, y) = { Sum = x; Neutral = y }

type SemiRing<'t>  =
    val Monoid: Monoid<'t>
    val Mul: 't -> 't -> 't
    new (mon, mul) = { Monoid = mon; Mul = mul }

type Group<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>
