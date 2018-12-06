type 'a stack = { mutable ind:int; size: int; mutable elts: 'a array }

exception Stack_empty
exception Stack_full


(** initialize a stack of max size n. ind is the index 
    couldn't just initialize elts as an array of length n because you need to fill in the array with something, we don't have that 'thing' yet, not even the type!!*)
let init_stack n = { ind = 0; size = n; elts = [||] }

(** pop that guards against popping empty stack *)
let pop p = 
    if p.ind = 0 then raise Stack_empty
    else (p.ind <- p.ind -1; p.elts.(p.ind))

let push e p = 
    if p.elts = [||] then (                 (** didn't use p.ind = 0 as it could also be that a stack has already allocated array, but just emptied it *)
        (p.elts <- Array.make p.size e);    (** interesting! when the first element is pushed, the array is initialized as a array of e of max length *)
        p.ind <- 1
    )
    else if p.ind >= p.size then raise Stack_full
    else (
        p.elts.(p.ind) <- e;
        p.ind <- p.ind + 1
    )


