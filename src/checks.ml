(* Vérification des transformations *)

open Ast_lib
open Asttypes
open Typed_ast
open Typed_ast_utils
open Target_lib


(* Vérification de la normalisation *)

exception Normalization of t_node

let atom expr =
  begin match expr.texpr_desc with
  | TE_const c -> true
  | TE_ident x -> true
  | TE_unop (_, _)
  | TE_binop (_, _, _)
  | TE_app (_, _)
  | TE_prim (_, _)
  | TE_if (_, _, _)
  | TE_fby (_, _)
  | TE_tuple _
  | TE_print _ -> false
  end

let rec bexpr expr =
  if atom expr then true
  else
    begin match expr.texpr_desc with
    | TE_unop (op, e) -> bexpr e
    | TE_binop (op, e1, e2) -> bexpr e1 && bexpr e2
    | TE_if (e, e1, e2) -> bexpr e && bexpr e1 && bexpr e2
    | TE_tuple (el) -> List.for_all bexpr el
    | TE_app (_, _)
    | TE_prim (_, _)
    | TE_fby (_, _)
    | TE_print _ -> false
    | TE_const _
    | TE_ident _ -> assert false
    end

let normalized_expr expr =
  if bexpr expr then true
  else
    begin match expr.texpr_desc with
    | TE_app (_, el) | TE_prim (_, el) | TE_print (el) -> List.for_all bexpr el
    | TE_fby (c, { texpr_desc = TE_tuple el} ) -> List.for_all atom el
    | TE_fby (c, e) -> atom e
    | TE_const _
    | TE_ident _
    | TE_unop (_, _)
    | TE_binop (_, _, _)
    | TE_if (_, _, _)
    | TE_tuple _ -> assert false
    end

let normalized_node n =
  List.for_all (fun eq -> normalized_expr eq.teq_expr) n.tn_equs

let normalization f =
  try
    List.iter
      (fun n -> if not (normalized_node n) then raise (Normalization n))
      f
  with Normalization n ->
    Format.eprintf "Warning: node %s is not in normal form.@." n.tn_name


(* Vérification de l'ordonnancement *)

exception Scheduling of t_node

let defs_of_patt patt acc =
  List.fold_left (fun acc x -> Scheduling.S.add x acc) acc patt.tpatt_desc

let deps_of_expr =
  let rec deps_of_expr expr acc =
    match expr.texpr_desc with
    | TE_ident x -> expr, Scheduling.S.add x acc
    | TE_fby (c, e) -> expr, acc
    | TE_const _
    | TE_unop (_, _)
    | TE_binop (_, _, _)
    | TE_app (_, _)
    | TE_prim (_, _)
    | TE_if (_, _, _)
    | TE_tuple _
    | TE_print _ -> expr_map_fold deps_of_expr expr acc
  in
  fun expr ->
    snd (deps_of_expr expr Scheduling.S.empty)

let scheduled_node node =
  let defs =
    List.fold_left
      (fun acc (x, _) -> Scheduling.S.add x acc)
      Scheduling.S.empty
      node.tn_input
  in
  let _ =
    List.fold_left
      (fun defs eq ->
        let deps = deps_of_expr eq.teq_expr in
        if Scheduling.S.subset deps defs then
          defs_of_patt eq.teq_patt defs
        else
          raise (Scheduling node))
      defs
      node.tn_equs
  in
  ()

let scheduling f =
  try
    List.iter scheduled_node f
  with Scheduling n ->
    Format.eprintf "Warning: node %s is not scheduled.@." n.tn_name

