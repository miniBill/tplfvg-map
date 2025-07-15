module IdSet exposing (IdSet, empty, insert, member, remove, size, toList, union)

import Id exposing (Id)
import Set exposing (Set)


type IdSet a
    = IdSet (Set String)


empty : IdSet a
empty =
    IdSet Set.empty


insert : Id a -> IdSet a -> IdSet a
insert id (IdSet set) =
    IdSet (Set.insert (Id.toString id) set)


toList : IdSet a -> List (Id a)
toList (IdSet set) =
    Set.foldr (\id acc -> Id.fromString id :: acc) [] set


member : Id a -> IdSet a -> Bool
member id (IdSet set) =
    Set.member (Id.toString id) set


size : IdSet a -> Int
size (IdSet set) =
    Set.size set


union : IdSet a -> IdSet a -> IdSet a
union (IdSet l) (IdSet r) =
    IdSet (Set.union l r)


remove : Id a -> IdSet a -> IdSet a
remove id (IdSet set) =
    IdSet (Set.remove (Id.toString id) set)
