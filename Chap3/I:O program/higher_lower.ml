(** INVOCATION :$ ocamlbuild higher_lower.byte 
                $ higher_lower.byte
*)

let randnum = Random.self_init (); Random.int 100  (**   a random number in [0,100)   *)
let count = ref 1 (** static variable to keep track of the number of guesses *)

let rec game randnum = 
    let guess = read_int ()
    in 
    if guess = randnum then print_string ("Bingo\nGot the right answer after "^string_of_int !count^" guesses!\n")
    else begin 
    if guess > randnum then print_string "Lower, please!\n" else print_string "Higher, please!\n";
    count := !count + 1;
    game randnum
   end

let () = 
    let () = print_string "guess a number between 0 and 100" 
    in print_newline (); 
    game randnum 