open Typed_ast

(** [expr_map f expr] applique la fonction [f] aux sous-expressions
    directes de [expr]. Cette fonction de fait pas de parcours en
    profondeur.
*)
let rec expr_map f expr =
  let desc =
    match expr.texpr_desc with
    | TE_const c -> TE_const c
    | TE_ident x -> TE_ident x
    | TE_unop (op, e) -> TE_unop (op, f e)
    | TE_binop (op, e1, e2) ->
        let e1 = f e1 in
        let e2 = f e2 in
        TE_binop (op, e1, e2)
    | TE_app (n, el) -> TE_app (n, List.map f el)
    | TE_prim (n, el) -> TE_prim (n, List.map f el)
    | TE_if (e, e1, e2) ->
        let e = f e in
        let e1 = f e1 in
        let e2 = f e2 in
        TE_if (e, e1, e2)
    | TE_fby (c, e) -> TE_fby (c, f e)
    | TE_tuple el -> TE_tuple (List.map f el)
    | TE_print el -> TE_print (List.map f el)
  in
  { expr with texpr_desc = desc }


(** [expr_map f expr acc] applique la fonction [f] avec l'argument
    [acc] aux sous-expressions directes de [expr]. Cette fonction de
    fait pas de parcours en profondeur.
*)
let expr_map_fold f expr acc =
  let desc, acc =
    match expr.texpr_desc with
    | TE_const c -> TE_const c, acc
    | TE_ident x -> TE_ident x, acc
    | TE_unop (op, e) ->
        let e, acc = f e acc in
        TE_unop (op, e), acc
    | TE_binop (op, e1, e2) ->
        let e1, acc = f e1 acc in
        let e2, acc = f e2 acc in
        TE_binop (op, e1, e2), acc
    | TE_app (n, el) ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        TE_app (n, List.rev rev_el), acc
    | TE_prim (n, el) ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        TE_prim (n, List.rev rev_el), acc
    | TE_if (e, e1, e2) ->
        let e, acc = f e acc in
        let e1, acc = f e1 acc in
        let e2, acc = f e2 acc in
        TE_if (e, e1, e2), acc
    | TE_fby (c, e) ->
        let e, acc = f e acc in
        TE_fby (c, e), acc
    | TE_tuple el ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        TE_tuple (List.rev rev_el), acc
    | TE_print el ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        TE_print (List.rev rev_el), acc
  in
  { expr with texpr_desc = desc }, acc
