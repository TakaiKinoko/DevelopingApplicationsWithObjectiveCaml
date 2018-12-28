type 'a cell = 
    {mutable prev: 'a dlist; 
     mutable next: 'a dlist; 
     info: 'a}
and 'a dlist = Empty | Cell of 'a cell

let add x = function 
    | Empty -> Cell {info = x; prev = Empty; next = Empty}
    | Cell c as l ->
        let new_cell = {info = x; prev=c.prev ; next = l} in 
        let new_dlist = Cell new_cell in 
        c.prev <- new_dlist;
        (match new_cell.prev with 
            | Empty -> ()
            | Cell pl -> pl.next <- new_dlist);
        new_dlist

let remove_cell = function 
    | Empty -> failwith "Already empty"
    | Cell c -> match (c.prev, c.next) with
        Empty, Empty -> Empty
        | Cell c1 as l, Empty -> c1.next <- Empty; l
        | Empty, ((Cell c2) as l) -> c2.prev <- Empty; l
        | Cell c1 as l1, (Cell c2 as l2) -> c1.next <- l2; c2.prev <-l1; l1

let rec remove x l = 
    let rec remove_left = function 
        | Empty -> ()
        | Cell c as l -> let pl = c.prev in 
                        if c.info = x then ignore (remove_cell l);
                            remove_left pl
    and remove_right = function
        | Empty -> ()
        | Cell c as l -> let nl = c.next in 
                        if c.info = x then ignore (remove_cell l);
                            remove_right nl
    in match l with 
        Empty -> Empty
        | Cell c as l -> if c.info = x then remove x (remove_cell l)
                        else (remove_left c.prev; remove_right c.next ; l)