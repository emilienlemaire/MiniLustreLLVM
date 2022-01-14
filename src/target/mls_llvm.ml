(*TODO: Clean this file, OMG it's so ugly, why?????*)
open Ast_lib.Imp_ast
open Ast_lib.Asttypes

module Ss = Set.Make(String)

let llvm_context = Llvm.global_context ()
let llvm_module = Llvm.create_module llvm_context "test"
let llvm_builder = Llvm.builder llvm_context

let node_ret_struct = Hashtbl.create 64
let mem_struct_attr_offset = Hashtbl.create 64
let mem_struct_offset_attr = Hashtbl.create 64
let node_vars_llvalue = Hashtbl.create 512
let init_funcs = Hashtbl.create 64
let node_vals = Hashtbl.create 64

let empty_mem_nodes = ref Ss.empty

let void_ty = Llvm.void_type llvm_context
let i1_ty = Llvm.i1_type llvm_context
let i8_ty = Llvm.i8_type llvm_context
let i32_ty = Llvm.i32_type llvm_context
let i64_ty = Llvm.i64_type llvm_context
let float_ty = Llvm.float_type llvm_context
let double_ty = Llvm.double_type llvm_context
let string_ty = Llvm.pointer_type (Llvm.i8_type llvm_context)

let llvm_false = Llvm.const_int i1_ty 0
let llvm_true = Llvm.const_int i1_ty 1

let eq_typ_num = ref 0
let str_num = ref 0
let tuple_num = ref 0

let get_next_eq_num () =
    incr eq_typ_num;
    !eq_typ_num

let get_next_str () =
    let num = string_of_int !str_num in
    incr str_num;
    "str."^num

let get_next_tuple () =
    let str = "tuple_t_"^(string_of_int (!tuple_num)) in
    incr tuple_num;
    str

let get_llvm_type typed_var =
    let (_, imp_type) = typed_var in
    match imp_type with
    | Tunit -> void_ty
    | Tbool -> i1_ty
    | Tint -> i32_ty
    | Tfloat -> float_ty
    | Tstring -> string_ty

(*TODO: Change to make array*)
let make_function_type mem_struct args_t ret_t =
    let mem_struct = if Llvm.is_opaque mem_struct then
            []
        else
            [Llvm.pointer_type mem_struct]
    in
    Llvm.function_type ret_t (Array.of_list (mem_struct@args_t))

let rec _get_struct_size_f typ =
    let body = Llvm.struct_element_types typ in
    Array.fold_left (fun acc typ ->
        acc +. (match (Llvm.classify_type typ) with
        | Llvm.TypeKind.Integer -> 0.5
        | Llvm.TypeKind.Float -> 0.5
        | Llvm.TypeKind.Struct -> _get_struct_size_f typ
        | _ -> failwith "Encountered an impossible type")
    ) 0.0 body


let get_struct_size typ =
    _get_struct_size_f typ
        |> ceil
        |> int_of_float

let make_return_type ret_t name =
    match ret_t with
    | [] ->    void_ty
    | hd::[] ->    get_llvm_type hd
    | _ ->
        let struct_type = Llvm.named_struct_type llvm_context ("ret_" ^ name) in
        Llvm.struct_set_body struct_type (Array.of_list (List.map get_llvm_type ret_t)) false;
        Hashtbl.add node_ret_struct name struct_type;
        let size = get_struct_size struct_type in
        if size > 1 then
            Llvm.array_type i64_ty size
        else
            i64_ty

let alloc_locals vars =
    List.map (fun var ->
        let (name, _) = var in
        let llvm_typ = get_llvm_type var in
        Llvm.build_alloca llvm_typ name llvm_builder
    ) vars

exception TypeError of string

let make_mem_struct node =
    let struct_name = (node.mn_name^"_mem") in
    let mem_struct = Llvm.named_struct_type llvm_context struct_name in
    let offsets = ref [] in
    let attrs = ref [] in
    let mem_types = List.mapi (fun idx ((name, _) as elt) ->
        offsets := (name, idx)::(!offsets);
        attrs := (idx, name)::(!attrs);
        get_llvm_type elt
    ) node.mn_mem.fby_mem in
    let base_length = List.length mem_types in
    let mem_types = mem_types @ List.mapi (fun idx (name, func) ->
        offsets := (name, idx + base_length)::(!offsets);
        attrs := (idx + base_length, name)::(!attrs);
        match Llvm.type_by_name llvm_module (func^"_mem") with
        | Some t -> t
        | None ->
            raise (TypeError (
                Printf.sprintf
                    "Type %s is not defined yet for node %s.\n"
                    (func^"_mem")
                    node.mn_name
                ))
    ) node.mn_mem.node_mem in
    Hashtbl.add mem_struct_attr_offset struct_name (!offsets);
    Hashtbl.add mem_struct_offset_attr struct_name (!attrs);
    if List.length mem_types > 0 then
        Llvm.struct_set_body mem_struct (Array.of_list mem_types) false;
    (* Printf.printf "Type for node %s: %s\n" node.mn_name (Llvm.string_of_lltype mem_struct); *)
    mem_struct

let llvalue_of_const cst =
    match cst with
    | Cunit -> (Llvm.undef void_ty)
    | Cbool b -> (Llvm.const_int i1_ty (if b then 1 else 0))
    | Cint i -> (Llvm.const_int i32_ty i)
    | Cfloat f -> Llvm.const_float float_ty f
    | Cstring str ->
            let re = Str.regexp "\\\\n" in
            let str = Str.global_replace re "\x0A" str in
            let str = Llvm.const_stringz llvm_context str in
            let str = Llvm.define_global (get_next_str ()) str llvm_module in
            Llvm.set_global_constant true str;
            Llvm.set_unnamed_addr true str;
            Llvm.set_linkage Llvm.Linkage.Private str;
            str

let remove_void_consts l =
    let rec loop l acc =
        match l with
        | [] -> acc
        | (_, Cunit)::tl -> loop tl acc
        | hd::tl -> loop tl acc@[hd]
    in
    loop l []

type init_t =
    | Const of const
    | Function of string

let init_node mem_struct node =
    let func_name = node.mn_name^"_init" in
    let func_type = Llvm.function_type (Llvm.pointer_type mem_struct) [||] in
    let func = Llvm.define_function func_name func_type llvm_module in
    let entry_block = (Llvm.basic_blocks func).(0) in
    Llvm.position_at_end entry_block llvm_builder;
    let elem_types = Llvm.struct_element_types mem_struct in
    let struct_alloca = Llvm.build_alloca mem_struct "mem" llvm_builder in
    let attrs = Hashtbl.find mem_struct_offset_attr (Option.get (Llvm.struct_name mem_struct)) in
    let init_vars = List.fold_left (fun acc (var, cst) ->
        (var, Const cst)::acc
    ) [] node.mn_init.fby_init in
    let init_vars = List.fold_left (fun acc (var, node) ->
        (var, Function (node^"_init"))::acc
    ) init_vars node.mn_init.node_init in
    let _ = Array.mapi (fun idx _ ->
        let name = List.assoc idx attrs in
        let ptr = Llvm.build_struct_gep struct_alloca idx name llvm_builder in
        let llval = (match List.assoc name init_vars with
        | Const cst -> llvalue_of_const cst
        | Function name ->
                let f_init = Hashtbl.find init_funcs name in
                Llvm.build_call f_init [| |] "" llvm_builder)
        in
        Llvm.build_store llval ptr llvm_builder
    ) elem_types in
    Hashtbl.add init_funcs func_name func;
    Llvm.build_ret struct_alloca llvm_builder

let make_eq_type eq =
    let typ = if List.length eq.meq_patt = 1 then
        get_llvm_type (List.hd eq.meq_patt)
    else
        let types = List.map (fun t_var ->
            get_llvm_type t_var
        ) eq.meq_patt in
        let struct_typ = Llvm.named_struct_type llvm_context (
            Printf.sprintf "eq_type_%d" (get_next_eq_num ())
        ) in
        Llvm.struct_set_body struct_typ (Array.of_list types) false;
        struct_typ
    in
    typ

let rec compile_expr typ ({mn_name = name;_ } as node) ({mexpr_desc = expr; _} as _e) =
    let named_llvalues = Hashtbl.find node_vars_llvalue name in
    match expr with
    | ME_const cst ->
            llvalue_of_const cst
    | ME_ident id ->
            let llval = List.assoc id named_llvalues in
            llval
    | ME_mem field ->
            let mem_struct_offset = Hashtbl.find mem_struct_attr_offset (name^"_mem") in
            let idx = List.assoc field mem_struct_offset in
            let mem_obj = List.assoc "mem" named_llvalues in
            let gep = Llvm.build_struct_gep mem_obj idx "" llvm_builder in
            let inst_name = Printf.sprintf "load_%s" field in
            Llvm.build_load gep inst_name llvm_builder
    | ME_unop (u, e') ->
            (let open Llvm.TypeKind in
            let llval = compile_expr typ node e' in
            match (Llvm.classify_type (Llvm.type_of llval)), u with
            | Integer, Uminus -> Llvm.build_neg llval "" llvm_builder
            | Integer, Unot   -> Llvm.build_not llval "" llvm_builder
            | Float, Uminus_f -> Llvm.build_fneg llval "" llvm_builder
            | _               -> Llvm.build_unreachable llvm_builder)
    | ME_binop (op, lhs, rhs) ->
            let open Llvm.TypeKind in
            let lhs_val = compile_expr typ node lhs in
            let rhs_val = compile_expr typ node rhs in
            (match Llvm.(classify_type (type_of lhs_val), classify_type (type_of rhs_val)) with
            | Integer, Integer -> (match op with
                | Beq -> Llvm.build_icmp Llvm.Icmp.Eq lhs_val rhs_val "cmp_eq" llvm_builder
                | Bneq -> Llvm.build_icmp Llvm.Icmp.Ne lhs_val rhs_val "cmp_neq" llvm_builder
                | Blt -> Llvm.build_icmp Llvm.Icmp.Slt lhs_val rhs_val "cmp_lt" llvm_builder
                | Ble -> Llvm.build_icmp Llvm.Icmp.Sle lhs_val rhs_val "cmp_le" llvm_builder
                | Bgt -> Llvm.build_icmp Llvm.Icmp.Sgt lhs_val rhs_val "cmp_gt" llvm_builder
                | Bge -> Llvm.build_icmp Llvm.Icmp.Sge lhs_val rhs_val "cmp_ge" llvm_builder
                | Badd -> Llvm.build_add lhs_val rhs_val "add" llvm_builder
                | Bsub -> Llvm.build_sub lhs_val rhs_val "sub" llvm_builder
                | Bmul -> Llvm.build_nsw_mul lhs_val rhs_val "mul" llvm_builder
                | Bdiv -> Llvm.build_sdiv lhs_val rhs_val "div" llvm_builder
                | Bmod -> Llvm.build_srem lhs_val rhs_val "mod" llvm_builder (*srem for signed remainder*)
                | Band -> Llvm.build_and lhs_val rhs_val "and" llvm_builder
                | Bor -> Llvm.build_or lhs_val rhs_val "or" llvm_builder
                | _ -> Llvm.build_unreachable llvm_builder
            )
            | Float, Float -> (match op with
                | Beq -> Llvm.build_fcmp Llvm.Fcmp.Ueq lhs_val rhs_val "cmp_eq" llvm_builder
                | Bneq -> Llvm.build_fcmp Llvm.Fcmp.Une lhs_val rhs_val "cmp_neq" llvm_builder
                | Blt -> Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmp_lt" llvm_builder
                | Ble -> Llvm.build_fcmp Llvm.Fcmp.Ule lhs_val rhs_val "cmp_le" llvm_builder
                | Bgt -> Llvm.build_fcmp Llvm.Fcmp.Ugt lhs_val rhs_val "cmp_gt" llvm_builder
                | Bge -> Llvm.build_fcmp Llvm.Fcmp.Uge lhs_val rhs_val "cmp_ge" llvm_builder
                | Badd_f -> Llvm.build_fadd lhs_val rhs_val "add" llvm_builder
                | Bsub_f -> Llvm.build_fsub lhs_val rhs_val "sub" llvm_builder
                | Bmul_f -> Llvm.build_fmul lhs_val rhs_val "mul" llvm_builder
                | Bdiv_f -> Llvm.build_fdiv lhs_val rhs_val "div" llvm_builder
                | Band -> Llvm.build_and lhs_val rhs_val "and" llvm_builder
                | Bor -> Llvm.build_or lhs_val rhs_val "or" llvm_builder
                | _ -> Llvm.build_unreachable llvm_builder
                )
            | _ -> Llvm.build_unreachable llvm_builder
            )
    | ME_app (n_name, mem_field, args) -> (* string * string * m_expr list: first one name of the func second name of the mem field*)
            let mem_struct_offset = Hashtbl.find mem_struct_attr_offset (name^"_mem") in
            let idx = List.assoc_opt mem_field mem_struct_offset in
            let args_val = if idx = None then
                List.fold_left (fun acc elt ->
                    let c = compile_expr typ node elt in
                    if Llvm.is_undef c then
                        acc
                    else
                        c::acc
                ) [] (List.rev args)
            else
                let idx = Option.get idx in
                let mem_obj = List.assoc "mem" named_llvalues in
                let gep = Llvm.build_struct_gep mem_obj idx "" llvm_builder in
                let gep_val = Llvm.build_load gep "" llvm_builder in
                let args_val = List.map (compile_expr typ node) args in
                gep_val::args_val
            in
            let args_arr = Array.of_list args_val in
            let callee_name = Printf.sprintf "%s_step" (Sanityze.remove_prime n_name) in
            let node_val = Hashtbl.find node_vals callee_name in
            let call = Llvm.build_call node_val args_arr "" llvm_builder in
            let node_typ = Hashtbl.find node_ret_struct callee_name in
            (* Llvm.dump_module llvm_module; *)
            if (Llvm.classify_type node_typ) = Llvm.TypeKind.Struct then
                let size = get_struct_size node_typ in
                let struct_val = Llvm.build_alloca node_typ "" llvm_builder in
                (if size = 1 then
                    let cast = Llvm.build_bitcast struct_val (Llvm.pointer_type i64_ty) "" llvm_builder in
                    let _ = Llvm.build_store call cast llvm_builder in
                    ()
                else
                    let call_ret_val = Llvm.build_alloca (Llvm.type_of call) "" llvm_builder in
                    let _ = Llvm.build_store call call_ret_val llvm_builder in
                    let struct_cast = Llvm.build_bitcast struct_val (Llvm.pointer_type i8_ty) "" llvm_builder in
                    let call_cast   = Llvm.build_bitcast call_ret_val (Llvm.pointer_type i8_ty) "" llvm_builder in
                    let mem_cpy_fun = Option.get (Llvm.lookup_function "llvm.memcpy.p0i8.p0i8.i64" llvm_module) in
                    let _ = Llvm.build_call mem_cpy_fun [|struct_cast; call_cast; Llvm.size_of node_typ; llvm_false|] "" llvm_builder in
                    ()
                );
                struct_val
            else
                Llvm.build_unreachable llvm_builder
    | ME_prim (p_name, args, _) -> (*string * m_expr list * int: name of prim, arg list, length of returned tuple*)
        let l = List.map (compile_expr typ node) args in
        (match p_name with
        | "int_of_float" ->
            assert ((List.length l) = 1);
            let f = List.hd l in
            Llvm.build_fptosi f i32_ty "" llvm_builder
        | "float_of_string" ->
            let func = Option.get (Llvm.lookup_function "\x01_strtof" llvm_module) in
            assert ((List.length l) = 1);
            let str = List.hd l in
            let gep_idx = [|Llvm.const_int i64_ty 0; Llvm.const_int i64_ty 0|] in
            let gep = Llvm.build_gep str gep_idx "" llvm_builder in
            let args = [|gep; Llvm.const_pointer_null (Llvm.pointer_type string_ty)|] in
            Llvm.build_call func args "" llvm_builder
        | "int_of_string" ->
            let func = Option.get (Llvm.lookup_function "strtol" llvm_module) in
            assert ((List.length l) = 1);
            let str = List.hd l in
            let gep_idx = [|Llvm.const_int i64_ty 0; Llvm.const_int i64_ty 0|] in
            let gep = Llvm.build_gep str gep_idx "" llvm_builder in
            let base = Llvm.const_int i32_ty 10 in
            let args = [|gep; Llvm.const_pointer_null (Llvm.pointer_type string_ty); base|] in
            let call = Llvm.build_call func args "" llvm_builder in
            Llvm.build_trunc call i32_ty "" llvm_builder
        | "bool_of_string" ->
            let args = List.map (fun {mexpr_desc = expr; _} ->
                expr
            ) args in
            (match args with
            | (ME_const (Cstring "true"))::_ -> Llvm.const_int i1_ty 1
            | (ME_const (Cstring "false"))::_ -> Llvm.const_int i1_ty 0
            | _ -> raise (Invalid_argument "bool_of_string expects a string that is either \"true\" of \"false\"")
            )
        | "float_of_int" ->
            assert ((List.length l) = 1);
            let i = List.hd l in
            Llvm.build_sitofp i float_ty "" llvm_builder
        (*TODO: Try with readline*)
        | "read" ->
            let buf = Llvm.build_alloca (Llvm.array_type i8_ty 1024) "" llvm_builder in
            let gep_idx = [|Llvm.const_int i64_ty 0; Llvm.const_int i64_ty 0|] in
            let gep = Llvm.build_in_bounds_gep buf gep_idx "" llvm_builder in
            let func = Option.get (Llvm.lookup_function "gets" llvm_module) in
            Llvm.build_call func [|gep|] "" llvm_builder
        | "cos" ->
            let func = Option.get (Llvm.lookup_function "llvm.cos.f32" llvm_module) in
            assert ((List.length l) = 1);
            let arg = [|List.hd l|] in
            Llvm.build_call func arg "" llvm_builder
        | "sin" ->
            let func = Option.get (Llvm.lookup_function "llvm.sin.f32" llvm_module) in
            assert ((List.length l) = 1);
            let arg = [|List.hd l|] in
            Llvm.build_call func arg "" llvm_builder
        | "random_int"
        | "random_float"
        | "draw_point"
        | "draw_line"
        | "draw_circle"
        | "draw_rect"
        | "fill_rect"
        | "get_mouse" -> failwith (Printf.sprintf "Prim %s not yet implemented" p_name)
        | p -> failwith (Printf.sprintf "%s is not a prim" p)
        )
    (*TODO: Add branch to phi node *)
    | ME_if (cond, then_e, else_e) ->
        let curr_f = Hashtbl.find node_vals (name^"_step") in
        let bbs = Llvm.basic_blocks curr_f in
        let idx = (Array.length bbs) - 1 in
        let ret_b = bbs.(idx) in
        let phi_b = Llvm.insert_block llvm_context "phi_b" ret_b in
        let else_b = Llvm.insert_block llvm_context "else_b" phi_b in
        let then_b = Llvm.insert_block llvm_context "then_b" else_b in
        let cond_val = compile_expr typ node cond in
        let _ = Llvm.build_cond_br cond_val then_b else_b llvm_builder in
        Llvm.position_at_end then_b llvm_builder;
        let then_val = compile_expr typ node then_e in
        Llvm.position_at_end else_b llvm_builder;
        let else_val = compile_expr typ node else_e in
        Llvm.position_at_end phi_b llvm_builder;
        Llvm.build_phi [(then_val, then_b); (else_val, else_b)] "" llvm_builder
    | ME_print el ->
        let print_str = List.fold_left (fun acc {mexpr_type = typ; _} ->
            let space = if acc = "" then "" else " " in
            match typ with
            | [Tstring] -> acc^space^"%s"
            | [Tbool] -> acc^space^"%d"
            | [Tfloat] -> acc^space^"%f"
            | [Tint] -> acc^space^"%d"
            | _ -> failwith "Type not implemented yet"
        ) "" el in
        let print_str = print_str^"\n" in
        let func = Option.get (Llvm.lookup_function "printf" llvm_module) in
        let str = Llvm.const_stringz llvm_context print_str in
        let str = Llvm.define_global (get_next_str ()) str llvm_module in
        Llvm.set_global_constant true str;
        Llvm.set_linkage Llvm.Linkage.Private str;
        Llvm.set_unnamed_addr true str;
        let vals = List.map (fun elt ->
            let e = compile_expr typ node elt in
            if elt.mexpr_type = [Tstring] then
                Llvm.define_global (get_next_str ()) e llvm_module
            else
                e
        ) el in
        let geps = List.map (fun llval ->
            let gep_idx = [|Llvm.const_int i64_ty 0; Llvm.const_int i64_ty 0|] in
            let class_ty = Llvm.classify_type (Llvm.type_of llval) in
            if  class_ty = Llvm.TypeKind.Array then
                Llvm.build_gep llval gep_idx "" llvm_builder
            else if class_ty = Llvm.TypeKind.Pointer then
                let elt_ty = Llvm.classify_type (Llvm.element_type (Llvm.type_of llval)) in
                if elt_ty = Llvm.TypeKind.Array then
                    Llvm.build_gep llval gep_idx "" llvm_builder
                else if elt_ty = Llvm.TypeKind.Float then
                    let load = Llvm.build_load llval "" llvm_builder in
                    Llvm.build_fpext load double_ty "" llvm_builder
                else
                    Llvm.build_load llval "" llvm_builder
            else
                llval
        ) (str::vals) in
        let args = Array.of_list geps in
        Llvm.build_call func args "" llvm_builder
    | ME_tuple el ->
        let llvals = List.rev (clean_compile typ node el) in
        let named_llvals = Hashtbl.find node_vars_llvalue node.mn_name in
        let lltyps = List.map (fun elt ->
            let name = Llvm.value_name elt in
            if (List.assoc_opt name named_llvals) = None then
                Llvm.type_of elt
            else
                Llvm.element_type (Llvm.type_of elt)
        ) llvals in
        let tuple_typ = Llvm.named_struct_type llvm_context (get_next_tuple ()) in
        Llvm.struct_set_body tuple_typ (Array.of_list lltyps) false;
        let tuple_struct = Llvm.build_alloca tuple_typ "" llvm_builder in
        (*On sauvegarde les valeurs dans le tuple*)
        List.iteri (fun idx elt ->
            let gep = Llvm.build_struct_gep tuple_struct idx "" llvm_builder in
            (* let ptr = Llvm.build_load gep "" llvm_builder in *)
            let name = Llvm.value_name elt in
            let llval = match (List.assoc_opt name named_llvalues) with
            | Some _ -> Llvm.build_load elt "" llvm_builder
            | None   -> elt
            in
            (* Llvm.dump_module llvm_module; *)
            let _ = Llvm.build_store llval gep llvm_builder in
            ()
        ) llvals;
        tuple_struct

and clean_compile typ node =
    List.fold_left (fun acc elt ->
        let c = compile_expr typ node elt in
        if Llvm.is_undef c then
            acc
        else
            c::acc
    ) []

let compile_equation node eq =
    let typ = make_eq_type eq in
    let named_llvals = Hashtbl.find node_vars_llvalue node.mn_name in
    if Llvm.type_is_sized typ then
        let llvalue = compile_expr typ node eq.meq_expr in
        if (List.length eq.meq_patt) > 1 then
            begin
            let l = List.mapi (fun idx elt ->
                let (name, _) = elt in
                let gep = Llvm.build_struct_gep llvalue idx "" llvm_builder in
                let value = Llvm.build_load gep "" llvm_builder in
                let ptr = List.assoc name named_llvals in
                Llvm.build_store value ptr llvm_builder
            ) (List.rev eq.meq_patt) in
            assert ((List.length l) > 0);
            List.hd (List.rev l)
            end
        else
            if List.length eq.meq_patt = 0 then
                llvalue
            else
                let (name, _) = (List.hd eq.meq_patt) in
                let ptr = List.assoc name named_llvals in
                let llvalue = if Llvm.classify_type (Llvm.type_of llvalue) = Llvm.TypeKind.Pointer then
                    Llvm.build_load llvalue "" llvm_builder
                else
                    llvalue
                in
                Llvm.build_store llvalue ptr llvm_builder
    else
        Llvm.build_ret_void llvm_builder

let compile_node node =
    let mem_struct = make_mem_struct node in
    let _ = if not (Llvm.is_opaque mem_struct) then
            init_node mem_struct node
        else
            (empty_mem_nodes := Ss.add node.mn_name (!empty_mem_nodes);
            Llvm.const_null i32_ty)
    in
    let input_list = node.mn_input_step in
    let output_list = node.mn_output_step in
    let llvm_args_types = List.map get_llvm_type input_list in
    let func_name = node.mn_name^"_step" in
    let llvm_ret_type = make_return_type output_list func_name in
    let func_type = make_function_type mem_struct llvm_args_types llvm_ret_type in
    let func_val = Llvm.define_function func_name func_type llvm_module in
    let dec = if Llvm.is_opaque mem_struct then 0 else 1 in
    Array.iteri (fun idx param ->
        if idx = 0  && not (Llvm.is_opaque mem_struct) then
            Llvm.set_value_name "mem" param
        else
            begin
                let param_name, _ = List.nth input_list (idx - dec) in
                let param_name = param_name in
                Llvm.set_value_name param_name param;
            end
    ) (Llvm.params func_val);
    let entry_block = (Llvm.basic_blocks func_val).(0) in
    Llvm.position_at_end entry_block llvm_builder;
    let ret_b = Llvm.append_block llvm_context "ret_b" func_val in
    let local_vars = node.mn_local@node.mn_output_step in
    let llvalues = alloc_locals local_vars in
    let name_llvalues = List.map (fun value ->
        (Llvm.value_name value, value)
    ) llvalues in
    let name_llvalues = (List.map (fun elt ->
        Llvm.value_name elt, elt
    ) (Array.to_list (Llvm.params func_val)))@name_llvalues in
    Hashtbl.add node_vars_llvalue node.mn_name name_llvalues;
    Hashtbl.add node_vals (node.mn_name^"_step") func_val;
    let _ = List.map (fun elt ->
        compile_equation node elt
    ) node.mn_compute in
    let _ = Llvm.build_br ret_b llvm_builder in
    Llvm.position_at_end ret_b llvm_builder;
    let output_names = List.map (fun (n, _) -> n) (List.rev output_list) in
    (*TODO: Compute update of mem here*)
    let ret_list = List.map (fun n -> List.assoc n name_llvalues) output_names in
    match ret_list with
    | [] -> Llvm.build_ret_void llvm_builder
    | hd::[] ->
            Llvm.build_ret hd llvm_builder
    | _ ->
        let ret_struct = Hashtbl.find node_ret_struct func_name in
        let struct_val = Llvm.build_alloca ret_struct "" llvm_builder in
        List.iteri (fun idx elt ->
            let gep = Llvm.build_struct_gep struct_val idx "" llvm_builder in
            let llval = Llvm.build_load elt "" llvm_builder in
            let _ = Llvm.build_store llval gep llvm_builder in
            ()
        ) ret_list;
        let ret_val = if (Llvm.classify_type llvm_ret_type) = Llvm.TypeKind.Integer then
            (Llvm.build_bitcast struct_val (Llvm.pointer_type i64_ty) "" llvm_builder
                |> Llvm.build_load) "" llvm_builder
        else
            let arr_val = Llvm.build_alloca llvm_ret_type "" llvm_builder in
            let arr_ptr = Llvm.build_bitcast arr_val (Llvm.pointer_type i8_ty) "" llvm_builder in
            let struct_ptr = Llvm.build_bitcast struct_val (Llvm.pointer_type i8_ty) "" llvm_builder in
            let mem_cpy_fun = Option.get (Llvm.lookup_function "llvm.memcpy.p0i8.p0i8.i64" llvm_module) in
            let _ = Llvm.build_call mem_cpy_fun [|arr_ptr; struct_ptr; Llvm.size_of ret_struct; llvm_false|] "" llvm_builder in
            Llvm.build_load arr_val "" llvm_builder
        in
        Llvm.build_ret ret_val llvm_builder

let printf_params_t = [|Llvm.pointer_type i8_ty|]
let printf_typ = Llvm.var_arg_function_type i32_ty printf_params_t

let strtol_params_t = [|Llvm.pointer_type i8_ty; Llvm.pointer_type (Llvm.pointer_type i8_ty); i32_ty|]
let strtol_typ = Llvm.function_type i64_ty strtol_params_t

let strtof_params_t = [|Llvm.pointer_type i8_ty; Llvm.pointer_type (Llvm.pointer_type i8_ty)|]
let strtof_typ = Llvm.function_type float_ty strtof_params_t

let mem_cpy_params_t = [|Llvm.pointer_type i8_ty; Llvm.pointer_type i8_ty; i64_ty; i1_ty|]
let mem_cpy_typ = Llvm.function_type void_ty mem_cpy_params_t

let compile nodes =
    let sanityzed_nodes = Sanityze.sanityze nodes in
    let _ = Llvm.declare_function "printf" printf_typ llvm_module in
    let _ = Llvm.declare_function "strtol" strtol_typ llvm_module in
    let _ = Llvm.declare_function "\x01_strtof" strtof_typ llvm_module in
    let _ = Llvm.declare_function "llvm.memcpy.p0i8.p0i8.i64" mem_cpy_typ llvm_module in
    let l = List.map compile_node sanityzed_nodes in
    (*TODO: Add the main node if it is set*)
    (*TODO: Make the module printed to a file / maybe made into an executable *)
    Llvm.dump_module llvm_module;
    l

