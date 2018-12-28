(*type 'a dll = 
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
*)
type 'a cell = {mutable pred: 'a dlist; mutable succ: 'a dlist; item: 'a}
and 'a dlist = Empty | Cell of 'a cell