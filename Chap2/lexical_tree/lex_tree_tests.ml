open OUnit
open Lex_tree

(** from bottom up *)
let node_E = Letter('E', true, [])   (** leaves of the tree contains an empty lex_tree*)
let node_R = Letter('R', true, [node_E])
let node_S = Letter('S', false, [node_E])
let node_L = Letter('L', false, [node_S])
let node_A = Letter('A', true, [node_L; node_R])
let node_D = Letter('D', true, [])
let node_Z = Letter('Z', false, [node_E])
let node_E' = Letter('E', false, [node_D; node_Z])
let node_I = Letter('I', false, [node_E'])
let node_R' = Letter('R', false, [node_I])
let node_F = Letter('F', false, [node_A; node_R'])
let single_tree_F = node_F::[]

let node_Y = Letter('Y', true, [])
let node_B = Letter('B', false, [node_Y])
let node_A' = Letter('A', false, [node_B; node_Y])
let node_S = Letter('S', true, [])
let node_S' = Letter('S', false, [node_S])
let node_O = Letter('O', false, [node_S'])
let node_B' = Letter('B', false, [node_O; node_A'])

let tree_B_F = node_B'::single_tree_F 

let tests = "test suite for chap2 exercises - lexical tree" >::: [
    "exists FARE" >:: (fun _-> assert_equal true (exists "FARE" single_tree_F));
    "exists FA" >:: (fun _-> assert_equal true (exists "FA" single_tree_F));
    "exists FALSE" >:: (fun _-> assert_equal true (exists "FALSE" single_tree_F));
    "exists FRIED" >:: (fun _-> assert_equal true (exists "FRIED" single_tree_F));
    "exists FRIEZE" >:: (fun _-> assert_equal true (exists "FRIEZE" single_tree_F));
    "exists FAR" >:: (fun _-> assert_equal true (exists "FAR" single_tree_F));
    "exists F" >:: (fun _-> assert_equal false (exists "F" single_tree_F));
    "exists FORK" >:: (fun _-> assert_equal false (exists "FORK" single_tree_F));
    "exists FRIE" >:: (fun _-> assert_equal false (exists "FRIE" single_tree_F));
    "exists FB" >:: (fun _-> assert_equal false (exists "FB" single_tree_F));
    "exists BAY" >:: (fun _ -> assert_equal true (exists "BAY" tree_B_F));
    "exists BABY" >:: (fun _ -> assert_equal true (exists "BABY" tree_B_F));
    "exists BOSS" >:: (fun _ -> assert_equal true (exists "BOSS" tree_B_F));
    "exists FARE" >:: (fun _-> assert_equal true (exists "FARE" tree_B_F));
    "exists FA" >:: (fun _-> assert_equal true (exists "FA" tree_B_F));
    "exists FALSE" >:: (fun _-> assert_equal true (exists "FALSE" tree_B_F));
    "exists FRIED" >:: (fun _-> assert_equal true (exists "FRIED" tree_B_F));
    "exists FRIEZE" >:: (fun _-> assert_equal true (exists "FRIEZE" tree_B_F));

    "insert FRIDAY" >:: (fun _ -> assert_equal true (exists "FRIDAY" (insert "FRIDAY" tree_B_F)));
    "insert BABE" >:: (fun _ -> assert_equal true (exists "BABE" (insert "BABE" tree_B_F)));
    "insert APPLE" >:: (fun _ -> assert_equal true (exists "APPLE" (insert "APPLE" tree_B_F)));
    "insert TRIP" >:: (fun _ -> assert_equal true (exists "TRIP" (insert "TRIP" tree_B_F)));
    "insert O" >:: (fun _ -> assert_equal true (exists "O" (insert "O" tree_B_F)));
    "insert OCAML" >:: (fun _ -> assert_equal true (exists "OCAML" (insert "OCAML" tree_B_F)));

    "construct single_tree_F" >:: (fun _ -> assert_equal single_tree_F (construct ["FA"; "FALSE"; "FAR"; "FARE"; "FRIED"; "FRIEZE"]));
    (** the following one doesn't pass for either implementation *)
    "construct tree_B_F" >:: (fun _ -> assert_equal tree_B_F 
        (construct [ "BABY"; "BAY"; "BOSS";"FA"; "FALSE"; "FAR"; "FARE"; "FRIED"; "FRIEZE"(*;"FRIDAY";"BABE";"APPLE";"TRIP";"O";"OCAML"*)]));
    "verify 1" >::(fun _ -> assert_equal ["FA"; "FRIED"] (verify ["FAT"; "FA"; "FOREIGN"; "FRIED"; "FORGE"; "FOX"] single_tree_F));
    "verify_alt 1" >::(fun _ -> assert_equal ["FAT"; "FOREIGN";"FORGE"; "FOX"] (verify_alt ["FAT"; "FA"; "FOREIGN"; "FRIED"; "FORGE"; "FOX"] single_tree_F));
]

let _ = run_test_tt_main tests