(** Write a function merge_i which takes as input two integer lists sorted in increasing order 
and returns a new sorted list containing the elements of the first two.*)

let rec merge_i la lb = 
    match la,lb with
    |[], _ -> lb
    |_ ,[] -> la
    | ha::ta, hb::tb -> 
        if ha < hb
        then  ha::(merge_i ta lb)
        else  hb::(merge_i la tb)

(** Write a general function merge which takes as argument a comparison function and two lists sorted in this order 
  and returns the list merged in the same order.
 The comparison function will be of type 'a -> 'a -> bool.  *)

(** Note how this function can take polymorphic parameters whereas the previous one can't *)
let rec merge (comp: 'a->'a-> bool) la lb = 
    match la, lb with 
    | [], _ -> lb
    | _, [] -> la
    | ha::ta, hb::tb -> 
        if comp ha hb
        then ha::(merge comp ta lb)
        else hb::(merge comp la tb)


(** What happens if one of the lists is not in the required decreasing order?  *)
(** test case -- merge-wrong input*)

(** Write a new list type in the form of a record containing three fields: 
the conventional list, 
an order function and a boolean indicating whether the list is in that order.  *)

type 'a newlist = {l:'a list; ord_fun: ('a -> 'a -> bool); in_order: bool}


(**  Write the function insert which adds an element to a list of this type. *)
(** incremental steps. fst how to insert an element to a conventional list*)
let rec insert (f:'a->'a->bool) (elm:'a) (lst:'a list) = 
    match lst with 
    |hd::tl -> if f elm hd then elm::lst  else hd::(insert f elm tl) 
    |[] -> elm::[]

(** now write the real one, nesting the previous function inside *)
let rec insert_newlist elm lst = 
    let rec f elm l = 
    match l with 
    |[]-> [elm]
    |hd::tl -> if lst.ord_fun elm hd then elm::l else hd::(f elm tl)
    in 
    if lst.in_order then {lst with l = f elm lst.l}
    else {lst with l= elm::lst.l}
    
(** Write a function sort which insertion sorts the elements of a list.  *)
(** naive implementation, doesn't pass test case insertsion sort 6
let rec sort comp_fun lst =
    match lst with
    | h1::h2::tl -> if comp_fun h1 h2 then h1::(sort comp_fun (h2::tl))
                    else sort comp_fun (insert comp_fun h1 (h2::tl))
    | h1::[] -> [h1]
    | [] -> [] *)
let sort lst = 
    if lst.in_order then lst
    else List.fold_right insert_newlist lst.l {l=[]; ord_fun = lst.ord_fun; in_order=true} 

(** Write a new function merge for these lists.  *)
let merge_newlist la lb = 
    {la with l =(merge la.ord_fun (sort la).l (sort lb).l); in_order=true}

(** BOOK SOLUTION:  *)
let rec merge_alt la lb = 
    if la.in_order then 
        if lb.in_order then { l = (merge la.ord_fun la.l lb.l); ord_fun = la.ord_fun; in_order = true}
        else List.fold_right insert_newlist lb.l la
    else 
        if lb.in_order then merge_alt lb la
        else merge_alt (sort la) lb