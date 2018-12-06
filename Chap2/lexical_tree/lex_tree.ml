(**Lexical trees (or tries) are used for the representation of dictionaries.
 *)
(** lex_node has one constructor with arguments *)
type lex_node = Letter of char * bool * lex_tree
    and lex_tree = lex_node list

type word = string;;  (** word would hence be an alias to string type *)


(** Write the function exists which tests whether a word belongs to a dictionary of type lex_tree.  *)
let rec exists (w:word) (d:lex_tree) : bool = 
    (** 1st locate the beginning char of the string *)
    let aux sw i n = match d with 
    [] -> false
    | (Letter (c, b, l))::t when c=sw.[i] -> 
        if n = 1 then b else exists (String.sub sw (i+1)(n-1)) l
    | (Letter (c, b, l))::t -> exists sw t
    in aux w 0 (String.length w)

(** Write a function insert which takes a word and a dictionary and returns a new dictionary which additionally contains this word. 
   If the word is already in the dictionary, it is not necessary to insert it.  *)
let rec insert (w:word) (d:lex_tree) : lex_tree = 
    let aux sw i n = 
        if n = 0 then d
        else match d with 
        | []-> [Letter(sw.[i], n =1, insert (String.sub sw (i+1) (n-1)) [])]
        | (Letter(c, b, l))::t when c = sw.[i] -> 
            if n = 1 then (Letter(c, true, l))::t
            else Letter(c, b, insert (String.sub sw (i+1)(n-1)) l)::t
        | (Letter(c, b, l))::t -> (Letter(c, b, l)):: (insert sw t)
    in aux w 0 (String.length w)
    
(** MINE *)
let construct (wl:word list): lex_tree =
    let f d w = insert w d 
    in 
    List.fold_left f [] wl    

(** BOOK: *)
(*let construct l = 
    let rec aux l d = 
    match l with 
    | [] -> d
    | h::t -> aux t (insert h d)
    in aux l []   *)


(** Write a function verify which takes a list of words and a dictionary and 
    returns the list of words not belonging to this dictionary.  *)

(*let verify lst dic = 
    let rec aux lst ret_val =
    match lst with 
    |[] -> ret_val
    |h::t -> if exists h dic then aux t (h::ret_val) else aux t ret_val
    in aux lst []  *)

(*let verify (lst:word list) (dic:lex_tree) = 
    List.fold_left (fun w l-> if exists w dic then (w::l) else l) [] lst   *)

(** MINE -- return all that belong *)
let verify lst dic = 
    List.filter (fun x-> exists x dic) lst 


(*let verify_alt lst dic  =  
    let rec filter p = function
    | []-> []
    | h::t -> if p h then h::(filter p t) else filter p t 
    in 
    filter (function x-> not (exists x dic)) lst  *)

(** BOOK -- return all that don't belong *)
let rec filter p = function 
     [] -> []
    |h::t -> if p h then h::(filter p t) else filter p t

let verify_alt lst dic = filter (function x-> not(exists x dic)) lst


(** Write a function select which takes a dictionary and a length and 
 returns the set of words of this length. *)

