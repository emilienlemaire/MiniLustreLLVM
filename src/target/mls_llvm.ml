open Ast_lib.Imp_ast
open Ast_lib.Asttypes

let llvm_context = Llvm.global_context ()

let llvm_module = Llvm.create_module llvm_context "test"

let llvm_builder = Llvm.builder llvm_context

let mem_struct_attr_offset = Hashtbl.create 64
let mem_struct_offset_attr = Hashtbl.create 64
let global_llvalue = Hashtbl.create 64
let init_funcs = Hashtbl.create 64

let void_ty = Llvm.void_type llvm_context
let i1_ty = Llvm.i1_type llvm_context
let i32_ty = Llvm.i32_type llvm_context
let float_ty = Llvm.float_type llvm_context
(*FIXME: This is not viable for strings, as we need an array instead of a pointer*)
let string_ty = Llvm.pointer_type (Llvm.i8_type llvm_context)


(* TODO: Sanityze and translate the equations:
    - For the tuples, we can use structs
    - Figure out all the string stuff*)

let get_llvm_type typed_var =
    let (_, imp_type) = typed_var in
    match imp_type with
    | Tunit -> void_ty
    | Tbool -> i1_ty
    | Tint -> i32_ty
    | Tfloat -> float_ty
    | Tstring -> string_ty


let make_function_type mem_struct args_t ret_t =
    Llvm.function_type ret_t (Array.of_list (mem_struct::args_t))

let make_return_type ret_t name =
    if List.length ret_t = 0 then
        void_ty
    else if List.length ret_t = 1 then
        get_llvm_type (List.hd ret_t)
    else
        let struct_type = Llvm.named_struct_type llvm_context ("ret_" ^ name) in
        Llvm.struct_set_body struct_type (Array.of_list (List.map get_llvm_type ret_t)) false;
        struct_type

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
            Llvm.dump_module llvm_module;
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
    | Cunit -> (Llvm.const_null i32_ty)
    | Cbool b -> (Llvm.const_int i1_ty (if b then 1 else 0))
    | Cint i -> (Llvm.const_int i32_ty i)
    | Cfloat f -> Llvm.const_float float_ty f
    | Cstring str -> Llvm.const_string llvm_context str

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
    let func_type = Llvm.function_type mem_struct [||] in
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

let compile_node node =
    (* Printf.printf "Working on node %s\n" node.mn_name; *)
    let mem_struct = make_mem_struct node in
    let _ = if not (Llvm.is_opaque mem_struct) then
            init_node mem_struct node
    else
        Llvm.const_null i32_ty
    in
    let input_list = node.mn_input_step in
    let output_list = node.mn_output_step in
    let llvm_args_types = List.map get_llvm_type input_list in
    let func_name = node.mn_name^"_step" in
    let llvm_ret_type = make_return_type output_list func_name in
    let func_type = make_function_type mem_struct llvm_args_types llvm_ret_type in
    let func_val = Llvm.define_function func_name func_type llvm_module in
    Array.iteri (fun idx param ->
        if idx = 0 then
            Llvm.set_value_name "mem" param
        else
            let param_name, _ = List.nth input_list (idx - 1) in
            let param_name = param_name in
            Llvm.set_value_name param_name param
    ) (Llvm.params func_val);
    let entry_block = (Llvm.basic_blocks func_val).(0) in
    Llvm.position_at_end entry_block llvm_builder;
    let local_vars = node.mn_local in
    let _ = alloc_locals local_vars in
    Llvm.build_ret_void llvm_builder


let compile nodes =
    let sanityzed_nodes = Sanityze.sanityze nodes in
    let l = List.map compile_node sanityzed_nodes in
    Llvm.dump_module llvm_module;
    l
