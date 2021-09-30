module AlgebraicStruct

type Monoid<'t> =
    val Sum: 't -> 't -> 't
    val Neutral: 't
    new (x, y) = { Sum = x; Neutral = y }

type SemiRing<'t>  =
    val Monoid: Monoid<'t>
    val Mul: 't -> 't -> 't
    new (mon, mul) = { Monoid = mon; Mul = mul }

type AlgebraicStruct<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>

    member this.getNeutral =
        match this with
        | Monoid m -> m.Neutral
        | SemiRing s -> s.Monoid.Neutral

    member this.getSumOp =
        match this with
        | Monoid m -> m.Sum
        | SemiRing s -> s.Monoid.Sum

    member this.getMulOp =
        match this with
        | Monoid _ -> failwith "Monoid doesn't have a mul operation"
        | SemiRing s -> s.Mul
