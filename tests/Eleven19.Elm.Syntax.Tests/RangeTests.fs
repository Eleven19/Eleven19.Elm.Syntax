namespace Eleven19.Elm.Syntax.Tests

open System
open Eleven19.Elm
open Expecto
open Eleven19.Elm.Syntax
open Eleven19.Elm.Syntax.Range
open FsCheck
open Thoth.Json.Net

module RangeTests =
    let combineTests = [
        test "given an empty list is an empty range" {
            Range.combine []
            |> Expect.equal Range.emptyRange
        }

        test "given a singleton list is the same range" {
            let range = {
                Range.Start = { Row = 1; Column = 1 }
                End = { Row = 1; Column = 9 }
            }

            Range.combine [ range ]
            |> Expect.equal range
        }

        test "given a list of ranges, where one covers all the others, gives that range" {
            let outerRange = {
                Range.Start = { Row = 1; Column = 1 }
                End = { Row = 5; Column = 100 }
            }

            let ranges = [
                outerRange
                {
                    Range.Start = { Row = 2; Column = 1 }
                    End = { Row = 2; Column = 9 }
                }
                {
                    Range.Start = { Row = 3; Column = 5 }
                    End = { Row = 4; Column = 15 }
                }
            ]

            Range.combine ranges
            |> Expect.equal outerRange
        }

        test "given a list of distinct ranges where none overlap, gives a range over all" {
            let outerRange = {
                Start = { Row = 2; Column = 1 }
                End = { Row = 10; Column = 79 }
            }

            let ranges = [
                {
                    Start = outerRange.Start
                    End = { Row = 2; Column = 21 }
                }
                {
                    Start = { Row = 4; Column = 6 }
                    End = { Row = 4; Column = 100 }
                }
                {
                    Start = { Row = 9; Column = 5 }
                    End = outerRange.End
                }
            ]

            Range.combine ranges
            |> Expect.equal outerRange
        }
    ]

    let compareLocationsTests = [
        describe "EQ" [
            test "when locations are equal" {
                let location = { Row = 1; Column = 3 }

                Range.compareLocations location location
                |> Expect.equal Order.EQ
            }
        ]
        describe "LT" [
            test "when left row < right row" {
                let left = { Row = 1; Column = 1 }
                let right = { left with Row = 3 }

                Range.compareLocations left right
                |> Expect.equal Order.LT
            }
            test "when left row = right row and left column < right column" {
                let left = { Row = 1; Column = 1 }
                let right = { left with Column = 3 }

                Range.compareLocations left right
                |> Expect.equal Order.LT
            }
        ]
        describe "GT" [
            test "when left row > right row" {
                let left = { Row = 3; Column = 4 }
                let right = { left with Row = 2 }

                Range.compareLocations left right
                |> Expect.equal Order.GT
            }
            test "when left row = right row and left column > right column" {
                let left = { Row = 2; Column = 6 }
                let right = { left with Column = 3 }

                Range.compareLocations left right
                |> Expect.equal Order.GT
            }
        ]
    ]

    let compareTests = [
        describe "EQ" [
            test "when ranges are equal" {
                let range = {
                    Start = { Row = 1; Column = 1 }
                    End = { Row = 1; Column = 9 }
                }

                Range.compare range range
                |> Expect.equal Order.EQ
            }
        ]
        describe "LT" [
            test "when left start < right start" {
                let left = {
                    Start = { Row = 1; Column = 1 }
                    End = { Row = 1; Column = 2 }
                }

                let right = {
                    left with
                        End = { Row = 2; Column = 2 }
                }

                Range.compare left right
                |> Expect.equal Order.LT
            }

            test "when left start == right start and left end < right end" {
                let left = {
                    Start = { Row = 2; Column = 1 }
                    End = { Row = 2; Column = 3 }
                }

                let right = {
                    left with
                        End = { Row = 2; Column = 9 }
                }

                Range.compare left right
                |> Expect.equal Order.LT
            }
        ]
        describe "GT" [
            test "when left start > right start" {
                let left = {
                    Start = { Row = 5; Column = 6 }
                    End = { Row = 5; Column = 7 }
                }

                let right = {
                    left with
                        End = { Row = 5; Column = 2 }
                }

                Range.compare left right
                |> Expect.equal Order.GT
            }

            test "when left start == right start and left end > right end" {
                let left = {
                    Start = { Row = 5; Column = 2 }
                    End = { Row = 5; Column = 9 }
                }

                let right = {
                    left with
                        End = { Row = 5; Column = 4 }
                }

                Range.compare left right
                |> Expect.equal Order.GT
            }
        ]
    ]

    let encodeTests = [
        test "given an empty range encodes to a zeroed out JSON list" {
            let actual =
                Range.emptyRange
                |> Range.encode

            let expected =
                Encode.list [
                    Encode.int 0
                    Encode.int 0
                    Encode.int 0
                    Encode.int 0
                ]
                |> Encode.toString 0

            Encode.toString 0 actual
            |> Expect.equal expected
        }

        test
            "given a range encodes to a 4 element list of start row, start column, end row, end column" {
            let range = {
                Start = { Row = 1; Column = 2 }
                End = { Row = 3; Column = 4 }
            }

            Range.encode range
            |> Encode.toString 0
            |> Expect.equal
            <| Encode.toString
                0
                (Encode.list [
                    Encode.int 1
                    Encode.int 2
                    Encode.int 3
                    Encode.int 4
                ])
        }
    ]

    let decoderTests = [
        test "given a JSON array of 4 ints returns a Range" {
            let json = "[1,80,5,120]"

            let expected = {
                Start = { Row = 1; Column = 80 }
                End = { Row = 5; Column = 120 }
            }

            json
            |> Decode.fromString Range.decoder
            |> Expect.equal (Result.Ok expected)
        }

        test "given an empty JSON array then the decoder returns an Error" {
            let json = "[]"
            let expected = Result.Error "Invalid input list"

            let actual =
                json
                |> Decode.fromString Range.decoder

            Expect.isError actual "Expected an Error"

        }
    ]


    [<Tests>]
    let tests =
        describe "Elm.Syntax.Range" [
            describe "combine" combineTests
            describe "compare" compareTests
            describe "compareLocations" compareLocationsTests
            describe "encode" encodeTests
            describe "decoder" decoderTests
        ]
