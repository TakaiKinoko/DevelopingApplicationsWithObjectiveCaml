type 'a stack = {mutable ind:int; mutable size:int; mutable elts: 'a array}

exception Stack_empty

let init_stack n = {ind = 0; size= max n 1; elts = [||]}

(** the stack grow "rightward" *)
let push e p = 
    if p.elts = [||] 
    then (
        p.elts <- Array.make p.size e;
        p.ind <- 1) 
    else if p.ind = p.size 
    then (
        let oldsize = p.size 
        and oldelts = p.elts in 
        p.size <-  oldsize * 2; 
        p.elts <- Array.make p.size e;
        for i = 0 to oldsize - 1 do 
        p.elts.(i) <- oldelts.(i)
        done;
        p.ind <- p.ind + 1 )
    else (
        p.elts.(p.ind) <- e;
        p.ind <- p.ind + 1
    )

(** also allow pop to decrease the size of the stack to reclaim unused memory *)
let pop (p: 'a stack) = 
    if p.ind = 0 then raise Stack_empty
    (** whenever the stack occupancy rate falls under 1/4, cut the stack size in half *)
    else if p.ind <= p.size / 4 then 
    begin    (** remember that ind is the next free slot to be filled, that makes ind - 1 the element to be popped *)
        p.size <- p.size / 2;
        p.elts <- Array.sub p.elts 0 p.size;
        p.ind <- p.ind - 1;
        p.elts.(p.ind)
    end
    else (
        p.ind <- p.ind - 1;
        p.elts.(p.ind)
    )