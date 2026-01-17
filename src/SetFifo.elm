module SetFifo exposing (SetFifo, dequeue, empty, enqueue, fromSeqSet, member)

import Fifo exposing (Fifo)
import SeqSet exposing (SeqSet)


type SetFifo a
    = SetFifo (Fifo a) (SeqSet a)


enqueue : a -> SetFifo a -> SetFifo a
enqueue x ((SetFifo fifo set) as setFifo) =
    if SeqSet.member x set then
        setFifo

    else
        SetFifo (Fifo.insert x fifo) (SeqSet.insert x set)


member : a -> SetFifo a -> Bool
member x (SetFifo _ set) =
    SeqSet.member x set


dequeue : SetFifo a -> Maybe ( a, SetFifo a )
dequeue (SetFifo fifo set) =
    case Fifo.remove fifo of
        ( Nothing, _ ) ->
            Nothing

        ( Just head, tail ) ->
            Just ( head, SetFifo tail (SeqSet.remove head set) )


empty : SetFifo a
empty =
    SetFifo Fifo.empty SeqSet.empty


fromSeqSet : SeqSet a -> SetFifo a
fromSeqSet set =
    SetFifo (Fifo.fromList (SeqSet.toList set)) set
