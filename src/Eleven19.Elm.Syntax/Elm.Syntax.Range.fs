namespace Eleven19.Elm.Syntax

open Eleven19.Elm
#if FABLE_COMPILER
open Thoth.Json
#else
open Microsoft.FSharp.Core
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


module Range =
    let emptyRange: Range = Range.Empty

    let inline combine (ranges: #seq<Range>) = Range.Combine ranges

    let inline compare left right = Range.Compare(left, right)

    let inline compareLocations left right = Location.Compare(left, right)

    let inline sortLocations locations = Location.Sort locations

    let private fromList (input: int list) =
        match input with
        | [ a; b; c; d ] ->
            Result.Ok {
                Range.Start = { Row = a; Column = b }
                End = { Row = c; Column = d }
            }
        | _ -> Result.Error "Invalid input list"

    let private fromResult =
        function
        | Result.Ok successValue -> Decode.succeed successValue
        | Result.Error errorMessage -> Decode.fail errorMessage

    let encode (range: Range) =
        [
            range.Start.Row
            range.Start.Column
            range.End.Row
            range.End.Column
        ]
        |> List.map Encode.int
        |> Encode.list

    let decoder: Decoder<Range> =
        Decode.list Decode.int
        |> Decode.andThen (
            fromList
            >> fromResult
        )

type Range with

    static member Decoder = Range.decoder
    static member Encode(range) = Range.encode range
    member self.Encode() = Range.encode self
