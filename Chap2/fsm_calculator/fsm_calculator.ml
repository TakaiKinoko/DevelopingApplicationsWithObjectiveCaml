(* this simple calculator is modeled on a finite state machine. pretty cool! *)

type key = Plus | Minus | Times | Div | Equals | Digit of int
type state = {
    lcd : int;  (* last computation done   *)
    lka : key;  (* last key activated      *)
    loa : key;  (* last operator activated *)
    vpr : int   (* value printed           *)
}

let is_digit = function x -> (x >= 0) && (x <= 9)

let valid ky = 
    match ky with 
     Digit n -> is_digit n
    | _ -> true 

let evaluate x y ky = 
    match ky with 
    | Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | Equals -> y
    | Digit _ -> failwith "evaluate: non op"


let transition st ky = 
   let digit_transition n = function 
       Digit _ -> { st with lka=ky; vpr=st.vpr*10+n }  
     |  _      -> { st with lka=ky; vpr=n }  
   in
      match ky with 
          Digit p  ->  digit_transition p st.lka 
        |  _         ->  let res = evaluate st.lcd st.vpr st.loa  
                         in { lcd=res; lka=ky; loa=ky; vpr=res } 
    