open OUnit2
open Lists_sol  (** has to be captalized *)

(** INVOCATION: $ ocamlbuild -use-ocamlfind -tag debug lists_tests.byte    
                $ ./lists_tests.byte 
                
    NOTE: the .byte file shouldn't be capitalized, I think.             
                *)

(** Create a .merlin (it's not visible  unless you -ls the dir) file and put two lines in it: 
    B _build
    PKG oUnit
 *)

let nlst1 = {l = [1;2;3;4;5;6]; ord_fun = (<=) ; in_order = true}
let nlst2 = {l = [3;4;5;1;2;6]; ord_fun = (<); in_order = false}

let tests = "test suite for chap2 exercises - lists" >::: [
    "merge_i 1" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge_i [1;5;6] [2;3;4]));
    "merge <" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge (<) [1;2;3] [4;5;6]));
    "merge <= 1" >:: (fun _ -> assert_equal [1;1;1;1] (merge (<=) [1;1;1] [1]));
    "merge <= 2" >:: (fun _ -> assert_equal [1;1;1;1;2;2;4] (merge (<=) [1;1;1;4] [1;2;2]));
    "merge <= 3" >:: (fun _ -> assert_equal [1;1;2;2;3;3;4;4;5;5;6;6] (merge (<=) [1;2;3;4;5;6] [1;2;3;4;5;6]));
    "merge <= 4" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge (<=) [1;2;3;4;5;6] []));
    "merge > int" >:: (fun _ -> assert_equal [100;99;98;97;50] (merge (>)[100; 98;50] [99;97]));
    "merge > string" >:: (fun _ -> assert_equal ["B"; "A"] (merge (>) ["B"] ["A"]));
    "merge > string" >:: (fun _ -> assert_equal ["z"; "wicked"; "queen"; "apple"] (merge (>) ["z"; "apple"] ["wicked"; "queen"]));
    "merge- wrong inputs" >:: (fun _ -> assert_equal [1;2;4;3;5] (merge (<) [1;4;3] [2;5]));
    "insert conventional 1" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (insert (<) 4 [1;2;3;5;6]));
    "insert conventional 2" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (insert (<) 1 [2;3;4;5;6]));
    "insert conventional 2" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (insert (<) 6 [1;2;3;4;5]));
    "newlist 1" >:: (fun _ -> assert_equal [1;2;3;3;4;5;6] (insert_newlist 3 nlst1).l);
    "newlist 2" >:: (fun _ -> assert_equal [4;3;4;5;1;2;6] (insert_newlist 4 nlst2).l);
    "insertion sort 1" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (sort {l=[1;3;2;6;5;4]; ord_fun = (<); in_order = false}).l);
    "insertion sort 2" >:: (fun _ -> assert_equal [] (sort {l=[] ;ord_fun= (<); in_order = false}).l);
    "insertion sort 3" >:: (fun _ -> assert_equal [1] (sort {l=[1] ;ord_fun= (<); in_order = false}).l);
    "insertion sort 4" >:: (fun _ -> assert_equal [1;2] (sort {l=[2;1] ;ord_fun= (<); in_order = false}).l);
    "insertion sort 5" >:: (fun _ -> assert_equal [1;2;3;4;5;6;7;8;9] (sort {l=[9;8;7;6;5;4;3;2;1] ;ord_fun= (<); in_order = false}).l);
    "insertion sort 6" >:: (fun _ -> assert_equal [1;1;3;3;5;5;7;7;9;9] (sort {l=[9;7;5;3;1;9;7;5;3;1] ;ord_fun= (<); in_order = false}).l);
    "merge new lists 1" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge_newlist {l = [2;3;4]; in_order = true; ord_fun = (<)} {l = [1;5;6]; in_order = true; ord_fun = (<)}).l );
    "merge new lists 2" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge_newlist {l = [4;3;2]; in_order = false; ord_fun = (<)} {l = [6;5;1]; in_order = false; ord_fun = (<)}).l );
    "merge alt 1" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge_alt {l = [2;3;4]; in_order = true; ord_fun = (<)} {l = [1;5;6]; in_order = true; ord_fun = (<)}).l );
    "merge alt 2" >:: (fun _ -> assert_equal [1;2;3;4;5;6] (merge_alt {l = [4;3;2]; in_order = false; ord_fun = (<)} {l = [6;5;1]; in_order = false; ord_fun = (<)}).l );
]

let _ = run_test_tt_main tests