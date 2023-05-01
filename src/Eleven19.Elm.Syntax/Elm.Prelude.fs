[<AutoOpen>]
module Eleven19.Elm.Prelude

open System

type Order =
    | LT
    | EQ
    | GT

    static member Compare<'T when 'T: comparison>(x: 'T, y: 'T) =
        Order.FromComparisonResult(compare x y)

    static member FromComparisonResult n =
        if n < 0 then LT
        elif n > 0 then GT
        else EQ


let (|LT|GT|EQ|) n =
    if n < 0 then LT
    elif n > 0 then GT
    else EQ

[<RequireQualifiedAccess>]
module Order =
    let compare x y =
        compare x y
        |> Order.FromComparisonResult
