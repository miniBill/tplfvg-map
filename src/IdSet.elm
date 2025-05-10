module IdSet exposing (IdSet, empty, fromList, insertAll, member, toList)

import Id exposing (Id)
import Set exposing (Set)


type IdSet a
    = IdSet (Set String)


empty : IdSet a
empty =
    IdSet Set.empty


insertAll : List (Id a) -> IdSet a -> IdSet a
insertAll list set =
    List.foldl insert set list


insert : Id a -> IdSet a -> IdSet a
insert id (IdSet set) =
    IdSet (Set.insert (Id.toString id) set)


toList : IdSet a -> List (Id a)
toList (IdSet set) =
    Set.foldr (\id acc -> Id.fromString id :: acc) [] set


member : Id a -> IdSet a -> Bool
member id (IdSet set) =
    Set.member (Id.toString id) set


fromList : List (Id a) -> IdSet a
fromList list =
    IdSet (Set.fromList (List.map Id.toString list))
