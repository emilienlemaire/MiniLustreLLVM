open Typed_ast

let new_local =
  let cpt = ref 0 in fun () -> incr cpt; "aux'"^(string_of_int !cpt)

(** [new_pat e] prend en argument une expression [e] et retourne une
    variable (ou une liste de variable si [e] est un tuple) du même
    type que [e].

    Plus précisément, cette fonction retourne un triplet [(decl, patt, expr)],
    où
    [decl] est la déclaration de la variable (ou une liste de déclaration
    si [e] est un tuple),
    [patt] est la variable (ou le tuple de variables) vue comme un motif et
    [expr] est la variable (ou le tuple de variables) vue comme une expression.
*)
let new_pat ({ texpr_type= ty; texpr_loc = loc } as e) =
  match ty with
  | [t] ->
      let x = new_local() in
      let decl = [x,t] in
      let patt = { tpatt_desc = [x]; tpatt_type = ty; tpatt_loc = loc } in
      let expr = { e with texpr_desc = TE_ident x } in
      decl, patt, expr
  | lt ->
      let lx = List.map (fun _ -> new_local()) lt in
      let decl = List.combine lx lt in
      let patt = { tpatt_desc = lx; tpatt_type = ty; tpatt_loc = loc } in
      let le =
	List.map
	  (fun (x,t) ->
            { texpr_desc = TE_ident x; texpr_type = [t]; texpr_loc = loc}
          ) decl
      in
      decl, patt, { e with texpr_desc = TE_tuple le }

(** [normalize ctx e] met l'expression [e] en forme normale (en <bexpr>)
    et ajoute à [ctx] les équations introduites lors de la normalisation.
*)
let rec normalize ctx e =
  match e.texpr_desc with
  | TE_const _ | TE_ident _ -> ctx, e

  | TE_unop(op,e1) ->
      let ctx, e1' = normalize ctx e1 in
      ctx, { e with texpr_desc = TE_unop(op,e1') }

  | TE_binop(op,e1,e2) ->
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      ctx, { e with texpr_desc = TE_binop(op,e1',e2') }

  | TE_app(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_app(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_prim(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_prim(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_print le ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_print(le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_if(e1,e2,e3) ->
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      let ctx, e3' = normalize ctx e3 in
      ctx, { e with texpr_desc = TE_if(e1',e2',e3') }

  | TE_tuple l ->
      let ctx, l' = normalize_list ctx l in
      ctx, { e with texpr_desc = TE_tuple l'}

  | TE_fby(c,e1) ->
      (* c fby e1 => x, { x = c fby y; y = normalize e1; } *)
      let (new_vars,new_eqs), e1' = normalize ctx e1 in
      let y_decl, y_patt, y_expr = new_pat e1' in
      let y_eq = { teq_patt = y_patt; teq_expr = e1' } in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_fby(c,y_expr) }; }
      in
      (x_decl@y_decl@new_vars, x_eq::y_eq::new_eqs), x_expr


and normalize_list ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx,l) e ->
	let ctx, e' = normalize ctx e in
	ctx, e'::l ) (ctx,[]) l
  in ctx, List.rev l

let normalize_equation node e =
  let (locals, new_eqs), e' = normalize ([],[]) e.teq_expr in
  { node with
    tn_local = locals@node.tn_local;
    tn_equs = { e with teq_expr = e' } :: (List.rev new_eqs) @ node.tn_equs }

let file =
  List.map
    (fun n ->
      let n =
	List.fold_left normalize_equation { n with tn_equs=[] } n.tn_equs
      in
      { n with tn_equs = List.rev n.tn_equs })
