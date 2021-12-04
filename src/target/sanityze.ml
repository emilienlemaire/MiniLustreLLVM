open Ast_lib.Imp_ast
open Ast_lib.Asttypes

module Ss = Set.Make(String)

let empty_node_mem = ref Ss.empty

let remove_prime str =
    Str.(global_replace (regexp "'") "" str)


let sanityze_var_list vl =
    let rec loop l acc =
        match l with
        | [] -> acc
        | (_, Tunit)::tl -> loop tl acc
        | (name, t)::tl -> loop tl acc@[(remove_prime name), t]
    in
    loop vl []

let sanityze_mem mem name =
    let (fby_mem, rm_fby) = List.fold_left (fun (fby_mem, s) (n, t) ->
        if t = Tunit then
            (fby_mem, Ss.add n s)
        else
            ((remove_prime n, t)::fby_mem, s)
    ) ([], Ss.empty) mem.fby_mem in
    let node_mem = List.fold_left (fun (node_mem) (name, node) ->
        if Ss.mem (remove_prime node) (!empty_node_mem) then
            node_mem
        else
            (remove_prime name, remove_prime node)::node_mem
    ) [] mem.node_mem in
    if (List.length fby_mem) + (List.length node_mem) = 0 then
        empty_node_mem := Ss.add (remove_prime name) (!empty_node_mem);
    (rm_fby, {fby_mem; node_mem})

let sanityze_init init rm_fby =
    let fby_init = List.fold_left (fun acc (n, cst) ->
        if Ss.mem (remove_prime n) rm_fby then
            acc
        else
            (remove_prime n, cst)::acc
    ) [] init.fby_init in
    let node_init = List.fold_left (fun acc (name, node) ->
        if Ss.mem (remove_prime node) (!empty_node_mem) then
            acc
        else
            (remove_prime name, remove_prime node)::acc
    ) [] init.node_init in
    {fby_init; node_init}

(* TODO: Add sanityzing to the equations *)
let sanityze_node node =
    let (rm_fby, mem) = sanityze_mem node.mn_mem node.mn_name in
    { node with
        mn_name = remove_prime node.mn_name;
        mn_input_step = sanityze_var_list node.mn_input_step;
        mn_output_step = sanityze_var_list node.mn_output_step;
        mn_local = sanityze_var_list node.mn_local;
        mn_mem = mem;
        mn_init = sanityze_init node.mn_init rm_fby;
    }

let sanityze file =
    List.map sanityze_node file
