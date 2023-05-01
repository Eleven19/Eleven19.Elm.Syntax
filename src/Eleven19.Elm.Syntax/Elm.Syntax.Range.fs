namespace Eleven19.Elm.Syntax

open Eleven19.Elm
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

/// Source location
type Location = {
    Row: int
    Column: int
} with

    static member Empty = { Location.Row = 0; Column = 0 }

    static member Compare(left, right) : Order =
        if left.Row < right.Row then Order.LT
        elif left.Row > right.Row then Order.GT
        else Order.compare left.Column right.Column

    static member Sort(locations: #seq<Location>) =
        locations
        |> Seq.sortBy (fun l -> l.Row, l.Column)

type Range = {
    Start: Location
    End: Location
} with

    static member Empty = {
        Start = Location.Empty
        End = Location.Empty
    }

    static member Compare(left, right) =
        match Location.Compare(left.Start, right.Start) with
        | Order.EQ -> Location.Compare(left.End, right.End)
        | order -> order

    static member Combine(ranges: #seq<Range>) =
        let starts =
            Seq.map (fun r -> r.Start) ranges
            |> Location.Sort

        let ends =
            Seq.map (fun r -> r.End) ranges
            |> Location.Sort
            |> Seq.rev

        Option.map2
            (fun start end' -> { Start = start; End = end' })
            (Seq.tryHead starts)
            (Seq.tryHead ends)
        |> Option.defaultValue Range.Empty


type Range with

    member self.Encode =
        [
            self.Start.Row
            self.Start.Column
            self.End.Row
            self.End.Column
        ]
        |> List.map Encode.int
        |> Encode.list

module Range =
    let emptyRange: Range = Range.Empty

    let encode (range: Range) = range.Encode

    let inline combine (ranges: #seq<Range>) = Range.Combine ranges

    let inline compare left right = Range.Compare(left, right)

    let inline compareLocations left right = Location.Compare(left, right)

    let inline sortLocations locations = Location.Sort locations

    let fromList (input: string list) =
        match input with
        | [ a; b; c; d ] ->
            Result.Ok {
                Range.Start = { Row = int a; Column = int b }
                End = { Row = int c; Column = int d }
            }
        | _ -> Result.Error "Invalid input list"
