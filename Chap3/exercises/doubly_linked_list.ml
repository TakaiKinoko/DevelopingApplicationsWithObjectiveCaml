type 'a dll = 
    | Empty 
    | Node of {mutable pred: 'a dll; mutable succ: 'a dll; item: 'a}

let add (elem: 'a) (lst: 'a dll) = 
    match lst with 
    | Empty -> Node {pred = Empty; succ = Empty; item = elem}
    | Node n -> 
        if n.pred = Empty then 
            let head = Node {pred = lst; succ = lst; item = elem} in
            n.pred <- head;
            n.succ <- head;
            lst
        else let old = n.succ in 
             let new_head = Node {pred = lst ; succ = old; item = elem} in
             match old with 
             | Empty -> failwith "error"
             | Node m -> 
                m.pred <- new_head;
                n.succ <- new_head;
                lst 

(** elem is sure to be an element of lst *)
let rec remove elem lst =  
    match lst with 
    | Empty -> failwith "Empty list"
    | Node n -> 
        if n.item = elem && n.pred = Empty then Empty
        else if n.item = elem then n.succ. <- n.succ