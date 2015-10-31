{
  open Lexing
  open StringUtils
  open Parser
  open Error
  open Format

(* Function that counts program lines *)
let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
}

let create_hashtable size init = 
  let tbl = Hashtbl.create size in
  List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
  (tbl)

(* Create a hashtable for Tony keywords *)
let keywords = create_hashtable 26 [
  ("and", T_and);    
  ("bool", T_bool);  
  ("char", T_char);  
  ("decl", T_decl);  
  ("def", T_def);    
  ("else", T_else);  
  ("elsif", T_elsif);
  ("end", T_end);    
  ("exit", T_exit);  
  ("false", T_false);
  ("for", T_for);    
  ("head", T_head);  
  ("if", T_if);	    
  ("int", T_int);    
  ("list", T_list);  
  ("mod", T_mod);    
  ("new", T_new);    
  ("nil", T_nil);    
  ("nil?", T_nilq);  
  ("not", T_not);    
  ("or", T_or);	    
  ("ref", T_ref);    
  ("return", T_return);
  ("skip", T_skip);  
  ("tail", T_tail);  
  ("true", T_true)    				
]

}

let id    = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '?']*
let digit = ['0'-'9']
let hex   = ['0'-'9' 'a'-'f' 'A'-'F']
let white = [' ' '\t' '\r']

(* Main entrypoint *)
rule lexer = parse
  | id as id           { try Hashtbl.find keywords id
			 with Not_found -> T_id (id) }
       
  | digit+ as int      { T_iconst (int_of_string int) }
  | "\'"               { let pos = lexbuf.Lexing.lex_curr_p in
			 parse_char pos lexbuf }
  | "\""               { let pos = lexbuf.Lexing.lex_curr_p in
			 parse_string [] pos lexbuf }
	   
  | "+"                { T_plus }
  | "-"                { T_minus }
  | "*"                { T_times }
  | "/"                { T_div }
  | "#"                { T_cons }
  | "="                { T_eq }
  | "<>"               { T_neq }
  | "<"                { T_less }
  | ">"                { T_greater }
  | "<="               { T_le }
  | ">="               { T_ge }
	   
  | "("                { T_lparen }
  | ")"                { T_rparen }
  | "["                { T_lbracket }
  | "]"                { T_rbracket }
  | ","                { T_comma }
  | ";"                { T_semic }
  | ":"                { T_colon }
  | ":="               { T_assign }

  | white+             { lexer lexbuf }
  | '\n'               { incr_linenum lexbuf; lexer lexbuf }
  | "%"                { singleline_comment lexbuf }
  | "<*"               { let pos = lexbuf.Lexing.lex_curr_p in
			 multiline_comment pos 0 lexbuf }
	 
  | eof                { T_eof }
  | _ as chr           { let pos = position_point lexbuf.Lexing.lex_curr_p in
			 print_position err_formatter pos; 
			 error "Invalid character %c" chr;
	                 lexer lexbuf }




(* Character parsing *)
and parse_char pos_start = parse 
    '\\' 'n' '\''      { T_cconst ("\n") }
  | '\\' 't' '\''      { T_cconst ("\t") }
  | '\\' 'r' '\''      { T_cconst ("\r") }
  | '\\' '0' '\''      { T_cconst ("\000") }
  | '\\' '\\' '\''     { T_cconst ("\\") }
  | '\\' '\'' '\''     { T_cconst ("\'") }
  | '\\' '\"' '\''     { T_cconst ("\"") }
  | '\\' "x" hex hex '\'' as s
	               { T_cconst (String.make 1 (parse_hex s)) }
  | _ as chr "\'"      { T_cconst (String.make 1 chr) }
  | _                  { dispose_char pos_start lexbuf }
and dispose_char pos_start = parse
  | '\n'               { incr_linenum lexbuf; dispose_char pos_start lexbuf }
  | eof                { let pos_end = lexbuf.Lexing.lex_curr_p in
		         let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 fatal "Character constant terminated with EOF";
			 T_eof }
  | '\''               { let pos_end = lexbuf.Lexing.lex_curr_p in
		         let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 error "Invalid character constant";
			 lexer lexbuf }
  | _                  { dispose_char pos_start lexbuf } 




(* String parsing *)
and parse_string acc pos_start = parse
  (* Line feeds and EOF are invalid characters (Line feeds can be escaped) *)
  | '\n'               { incr_linenum lexbuf; dispose_string pos_start lexbuf }
  | eof                { let pos_end = lexbuf.Lexing.lex_curr_p in
		         let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 fatal "String terminated with EOF";
			 T_eof }
  (* End of string *)		       
  | '\"'               { T_sconst (implode (List.rev acc)) }
  (* Match next string character *)
  | '\\' '\n'          { incr_linenum lexbuf; parse_string acc pos_start lexbuf }
  | _ as chr           { parse_string (chr::acc) pos_start lexbuf }
and dispose_string pos_start = parse
  | '\n'               { incr_linenum lexbuf; dispose_string pos_start lexbuf }
  | eof                { let pos_end = lexbuf.Lexing.lex_curr_p in
		         let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 fatal "String terminated with EOF";
			 T_eof }
  | '\"'               { let pos_end = lexbuf.Lexing.lex_curr_p in
		         let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 error "Multiline string";
			 lexer lexbuf }
  | _                  { dispose_string pos_start lexbuf }




(* Single-line comment parsing *)
and singleline_comment = parse
  | '\n'               { incr_linenum lexbuf; lexer lexbuf }
  | eof                { T_eof }
  | _                  { singleline_comment lexbuf }
 



(* Multi-line comment parsing (nested comments allowed) *)
and multiline_comment pos_start level = parse
  | "*>"               { if level = 0 then lexer lexbuf 
			 else multiline_comment pos_start (level-1) lexbuf }
  | "<*"               { multiline_comment pos_start (level+1) lexbuf }
  | '\n'               { incr_linenum lexbuf; multiline_comment pos_start level lexbuf }
  | _                  { multiline_comment pos_start level lexbuf }
  | eof                { let pos_end = lexbuf.Lexing.lex_curr_p in
			 let pos = position_context pos_start pos_end in
			 print_position err_formatter pos;
			 fatal "Unterminated comment section";
			 T_eof }
