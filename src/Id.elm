module Id exposing (Environment, Id, Line, Stop, Vehicle, fromString, toString)


type Id a
    = Id String


type Stop
    = Stop


type Environment
    = Environment


type Vehicle
    = Vehicle


type Line
    = Line


fromString : String -> Id a
fromString id =
    Id id


toString : Id a -> String
toString (Id id) =
    id
