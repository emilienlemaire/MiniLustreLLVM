
(* Programme principal *)

open Frontend
open Format
open Lexing
open Lexer
open Parser
open Ast_lib
open Ast
open Target_lib
open Printers

let usage = "usage: "^Sys.argv.(0)^" [options] file.mls"

let parse_only = ref false
let type_only = ref false
let norm_only = ref false
let sched_only = ref false
let lucy_printer = ref false
let ocaml_printer = ref true
let main_node = ref ""
let steps = ref ~-1
let verbose = ref false
let graphics = ref false
let ml_only = ref false
let ll_only = ref false

let spec =
  ["-parse-only", Arg.Set parse_only,       "  stops after parsing";
   "-type-only",  Arg.Set type_only,        "  stops after typing";
   "-norm-only",  Arg.Set norm_only,        "  stops after normalization";
   "-sched-only", Arg.Set sched_only,       "  stops after scheduling";
   "-ml-only",    Arg.Set ml_only,          "  compile only to ocaml";
   "-ll-only",    Arg.Set ll_only,          "  compile only to llvm";
   "-main",       Arg.Set_string main_node, "<name>  main node";
   "-steps",      Arg.Set_int steps,        "<int>  steps of test";
   "-verbose",    Arg.Set verbose,          "print intermediate transformations";
   "-graphics",   Arg.Set graphics,         "add graphics context in .ml generated";
   "-v",          Arg.Set verbose,          "print intermediate transformations";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".mls") then
      raise (Arg.Bad "no .mls extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let ft = Typing.type_file f !main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Typed ast                          */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    if !type_only then exit 0;
    let ft = Normalization.file ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Normalized ast                     */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    Checks.normalization ft;
    if !norm_only then exit 0;
    let ft = Scheduling.schedule ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Scheduled ast                      */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft;
    end;
    Checks.scheduling ft;
    if !sched_only then exit 0;
    let imp_prg = Imp.compile ft in
    let imp_prg = Imp.rename_nodes imp_prg main_node in


    if not !ml_only then begin
        let llvm = Target_lib.Mls_llvm.compile file imp_prg !main_node !steps in
        if !verbose then begin
          Format.printf "/**************************************/@.";
          Format.printf "/* LLVM IR                            */@.";
          Format.printf "/**************************************/@.";
          print_string llvm;
          print_newline ();
      end;
      let ll = (Filename.chop_suffix file ".mls") ^ ".ll" in
      let cll = open_out ll in
      Printf.fprintf cll "%s" llvm;
      close_out cll;
    end;

    if not !ll_only then begin
        let ml = (Filename.chop_suffix file ".mls") ^ ".ml" in
        let cml = open_out ml in
        Ocaml_printer.output_ocaml cml imp_prg !main_node !steps !graphics;
        close_out cml;
    end;

    exit 0
  with
    | Lexical_error s ->
        report_loc (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "lexical error: %s\n@." s;
        exit 1
    | Parsing.Parse_error ->
        report_loc (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "syntax error\n@.";
        exit 1
    | Typing.Error(l,e) ->
        report_loc l;
        eprintf "%a\n@." Typing.report e;
        exit 1
