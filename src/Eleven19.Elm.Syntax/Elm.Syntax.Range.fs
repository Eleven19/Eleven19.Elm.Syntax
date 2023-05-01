namespace Eleven19.Elm.Syntax

/// Source location
type Location = { Row: int; Column: int }

type Range = { Start: Location; End: Location }
