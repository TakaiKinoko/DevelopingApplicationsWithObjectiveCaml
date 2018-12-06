type 'a graph = ('a * 'a list) list

(** Write a function insert_vtx which inserts a vertex into a graph and returns the new graph. *)
(** assume no knowledge of the graph *)
let example_graph = [('a', ['b';'c']); ('c',['e']);('e',[]);('b', ['d']); ('d', ['e'])]

let insert_vtx v g = 
    let rec aux v subg = 
    match subg with 
    | (x, _)::tl -> if x = v then g else aux v tl
    | [] -> (v, [])::g
    in
    aux v g 


(**Write a function insert_edge which adds an edge to a graph already possessing these two vertices.  *)
(** undirected graph version s*)
(*let insert_edge v1 v2 g = 
    let rec aux subg =
    match subg with
    | (x, l)::tl -> if x = v1 then ((x, v2::l)::tl) else if x = v2 then ((x, v1::l)::tl) else ((x, l)::(aux tl))
    | []-> []
    in 
    aux g *) 

(** the edge is v1->v2 *)
let insert_edge v1 v2 g = 
    let rec aux subg =
    match subg with
    | (x, l)::tl -> if x = v1 then ((x, v2::l)::tl) else ((x, l)::(aux tl))
    | []-> []
    in 
    aux g

(** 3.Write a function has_edges_to which returns all the vertices following directly 
from a given vertex. *)
let has_edges_to v g =  List.assoc v g

let rec has_edges_to_alt v g = 
    match g with 
    | []-> []
    | (v', el)::_ when v' = v -> el
    | _::tl -> has_edges_to_alt v tl


(** 4.Write a function has_edges_from which 
returns the list of all the vertices leading directly to a given vertex  *)
let has_edges_from (v:'a) (g: 'a graph) : 'a list =
    List.fold_left (fun acc (v',l) -> if List.mem v l then v'::acc else acc) [] g