type matrix = { n:int; m: int; t: float array array}


(** if used t = Array.make n (Array.make m 0.0) instead, when modifying one element, others share!!! *)
let create_mat n m = {n = n; m = m; t = Array.make_matrix n m 0.0}

let access_mat m i j = m.t.(i).(j)

let mod_mat m i j e = m.t.(i).(j) <- e


let add_mat p q = 
    if p.n = q.n && p.m = q.m then begin 
        let new_m = create_mat p.n p.m 
        in (
        for i = 0 to p.n - 1 do (
            for j = 0 to p.m -1 do 
            mod_mat new_m i j ((access_mat p i j) +. (access_mat q i j))
            done
        ) done; 
        new_m)
        end
    else 
    failwith "Incompatible matrices"


let mul_mat p q = 
    if p.m = q.n then 
    let new_m = create_mat p.n q.m 
    in 
    for i = 0 to p.n - 1 do (
        for j = 0 to q.m - 1 do (
            let sum = ref 0. in 
            for h = 0 to p.m - 1 do (
                sum := !sum +. ((access_mat p i h) *. (access_mat q h j))
            )done; 
            mod_mat new_m i j !sum
        )done
    )done; new_m
    else failwith "Incompatible matrices"