open Format
open Parser
open Lexer
open Lexing
open Error      

type mode_t = Normal | Intermediate | Final
       

let mode = ref Normal
let in_file = ref None

let filename = ref ""
let optimizations = ref false

		   
let usage_msg = "Usage: tonyc [options] filename \n\nOptions:"
let spec = Arg.align [
	       "-i", Arg.Unit (fun () -> mode := Intermediate), "Output Intermediate Code";
	       "-O", Arg.Unit (fun () -> optimizations := true), "Enable Optimizations"
	     ]
let anon_fun str =
  in_file := Some str
		  
(* Function that updates the filename in Lexer buffer *)
let update_filename lexbuf filename =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with pos_fname = filename }
		  
let main =
  Arg.parse spec anon_fun usage_msg;
  let in_channel = match !in_file with
    | None -> stdin
    | Some str -> filename := str; open_in str in
  let lexbuf = Lexing.from_channel in_channel in
  update_filename lexbuf !filename;
  try
    let quads = Array.of_list (List.rev (Parser.program Lexer.lexer lexbuf)) in
    let _ = Optimize.optimize !optimizations quads in
    match !mode with
    | Intermediate ->
       Quads.print_quads stdout quads
    | Final ->
       ()
    | Normal ->
       match !in_file with
       | None ->
	  internal "Need to read from a source file!"; raise Terminate
       | Some str ->
	  let imm_file = open_out ((Filename.chop_extension str) ^ ".imm") in
	  Quads.print_quads imm_file quads             
  with
    Error -> 
    let pos = position_point lexbuf.Lexing.lex_curr_p in
    print_position err_formatter pos;
    error "syntax error";
    exit 1 
  | Terminate ->
     exit 1
			   
