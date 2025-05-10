module IdDict exposing (IdDict, empty, insert, values)

import Dict exposing (Dict)
import Id exposing (Id)


type IdDict a v
    = IdDict (Dict String v)


empty : IdDict a v
empty =
    IdDict Dict.empty


insert : Id a -> v -> IdDict a v -> IdDict a v
insert key value (IdDict dict) =
    IdDict (Dict.insert (Id.toString key) value dict)


values : IdDict k v -> List v
values (IdDict dict) =
    Dict.values dict
