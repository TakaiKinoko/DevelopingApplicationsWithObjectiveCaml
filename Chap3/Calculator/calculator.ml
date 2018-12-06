type key = Plus | Minus | Times | Div | Equals | Digit of int 
        | Store | Recall | Clear | Off 

type state = {
  mutable lcd : int;  (* last computation done   *)
  mutable lka : bool; (* last key activated      *)
  mutable loa : key;  (* last operator activated *)
  mutable vpr : int;  (* value printed           *)
  mutable mem : int   (* memory of calculator    *)
 }

exception Invalid_key
exception Key_off

let translation c = match c with
     '+' -> Plus
   | '-' -> Minus
   | '*' -> Times
   | '/' -> Div
   | '=' -> Equals
   | 'C' | 'c' -> Clear
   | 'M' -> Store
   | 'm' -> Recall
   | 'o' | 'O' -> Off
   | '0'..'9' as c -> Digit ((Char.code c) - (Char.code '0'))
   | _ -> raise Invalid_key

let transition s key = match key with 
     Clear ->   s.vpr <- 0 
   | Digit n -> s.vpr <- ( if s.lka then s.vpr*10+n else n );
                s.lka <- true
   | Store  ->  s.lka <- false ; 
                s.mem <- s.vpr 
   | Recall ->  s.lka <- false ;
                s.vpr <- s.mem
   | Off -> raise Key_off
   |  _  -> let lcd = match s.loa with  
                         Plus  -> s.lcd + s.vpr  
                       | Minus -> s.lcd - s.vpr 
                       | Times  -> s.lcd * s.vpr 
                       | Div   -> s.lcd / s.vpr 
                       | Equals  -> s.vpr
                       | _ -> failwith "transition: impossible match"
                   in 
              s.lcd <- lcd ;
              s.lka <- false ; 
              s.loa <- key ;
              s.vpr <- s.lcd

let go () = 
   let state = { lcd=0; lka=false; loa=Equals; vpr=0; mem=0 } 
   in try 
        while true do 
          try 
            let input = translation (input_char stdin)
            in transition state input ;
               print_newline () ;
               print_string "result: " ;
               print_int state.vpr ;
               print_newline () 
          with
            Invalid_key -> ()  (* no effect *)
        done
      with
        Key_off -> ()

(** acts like a main entry *)
let () = 
    go ()