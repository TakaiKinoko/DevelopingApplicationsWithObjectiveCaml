open OUnit2
open Graphs

(** INVOCATION W/O TAG: 
    ocamlbuild -use-ocamlfind -pkg oUnit debug tests.byte
 *)
let example_graph = [('a', ['b';'c']); ('c',['e']);('e',[]);('b', ['d']); ('d', ['e'])]

let tests = "test suite for graph traversal " >:::[
    "insert 1 ">:: (fun _ -> assert_equal [('a', [])] (insert_vtx 'a' []));
    "insert 2 ">:: (fun _ -> assert_equal (insert_vtx 'd' example_graph) example_graph);
    "insert 3" >:: (fun _ -> assert_equal (insert_vtx 'f' example_graph) (('f',[])::example_graph));

    "insert_edge 1">:: (fun _ -> assert_equal [('a', ['b'; 'c']); ('c', ['e']); ('e', []); ('b', ['d']); ('d', ['c'; 'e'])] (insert_edge 'd' 'c' example_graph));
    
    "has_edges_from 1">:: (fun _ -> assert_equal ['a'] (has_edges_from 'c' example_graph));
    "has_edges_from 2">:: (fun _ -> assert_equal ['c', 'd'] (List.sort Pervasives.compare (has_edges_from 'e' example_graph)));
]

let _ = run_test_tt_main tests