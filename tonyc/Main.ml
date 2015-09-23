open Format
open Parser
open Lexer
open Lexing
open Error

let lexbuf = Lexing.from_channel stdin
    
let main =
  try
    Parser.program Lexer.lexer lexbuf;
    exit 0
  with 
    Error ->
     let pos = position_point lexbuf.Lexing.lex_curr_p in
     print_position err_formatter pos;
     error "syntax error";
     exit 1
  | Terminate -> 
     exit 1
