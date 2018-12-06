type lex_node = Letter of char * bool * lex_tree 
and  lex_tree = lex_node  list

type word = string

let exists (w:word) (dic:lex_tree) = 
    let rec aux n tree = 
    match tree with 
    | hd::tl -> begin
        match hd with
        |Letter (c, b, t) -> 
        if n = (String.length w)-1 && w.[n] = c then b 
        else if n = (String.length w)-1 then false
        else if w.[n] = c then aux (n+1) t
        else aux n tl
        end
    | [] -> false 
    in
    aux 0 dic


let rec insert (w:word) (dic:lex_tree) = 
    if exists w dic then dic 
    else 
    let len = String.length w in 
    match dic with 
    | hd::tl -> begin 
        match hd with 
        | Letter (c, b, t)-> if c = w.[0] && len = 1 then Letter(c, true, t)::tl else if c = w.[0] then insert (String.sub w 1 (len-1)) t else insert w t
    end
    | [] -> if len = 1 then dic@(Letter(w.[0], true, [])::[]) else insert (String.sub w 1 (len-1)) (dic@(Letter(w.[0], false, [])::[]))
    
