{
  type token =
    | T_eof | T_id | T_iconst | T_cconst | T_sconst
    | T_and | T_bool | T_char | T_decl | T_def | T_else | T_elsif
    | T_end | T_exit | T_false | T_for | T_head | T_if | T_int
    | T_list | T_mod | T_new | T_nil | T_nilq | T_not | T_or
    | T_ref | T_return | T_skip | T_tail | T_true
    | T_plus | T_minus | T_times | T_div | T_cons 
    | T_eq | T_neq | T_greater | T_less | T_ge | T_le
    | T_lparen | T_rparen | T_lbracket | T_rbracket 
    | T_comma | T_semic | T_colon | T_assign

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
  ("nilq", T_nilq);  
  ("not", T_not);    
  ("or", T_or);	    
  ("ref", T_ref);    
  ("return", T_return);
  ("skip", T_skip);  
  ("tail", T_tail);  
  ("true", T_true)    				
]

}

let id    = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let digit = ['0'-'9']
let white = [' ' '\t' '\r']

rule lexer = parse
    id as id { try Hashtbl.find keywords id
               with Not_found -> T_id }
       
  | digit+   { T_iconst }
  | "\'"     { parse_char lexbuf }
  | "\""     { parse_string lexbuf }
	   
  | "+"      { T_plus }
  | "-"      { T_minus }
  | "*"      { T_times }
  | "/"      { T_div }
  | "#"      { T_cons }
  | "="      { T_eq }
  | "<>"     { T_neq }
  | "<"      { T_less }
  | ">"      { T_greater }
  | "<="     { T_le }
  | ">="     { T_ge }
	 
  | "("      { T_lparen }
  | ")"      { T_rparen }
  | "["      { T_lbracket }
  | "]"      { T_rbracket }
  | ","      { T_comma }
  | ";"      { T_semic }
  | ":"      { T_colon }
  | ":="     { T_assign }

  | white+             { lexer lexbuf }
  | '\n'               { incr_linenum lexbuf; lexer lexbuf }
  | "%" [^ '\n']* "\n" { incr_linenum lexbuf; lexer lexbuf }
  | "<*"               { comments 0 lexbuf }
	 
  | eof                { T_eof }
  | _ as chr           { Printf.eprintf "Invalid character: '%c' (ascii: %d)"
					chr (Char.code chr);
	                 T_eof }

and parse_char = parse 
    '\\' 'n' '\''      { T_cconst }
  | '\\' 't' '\''      { T_cconst }
  | '\\' 'r' '\''      { T_cconst }
  | '\\' '0' '\''      { T_cconst }
  | '\\' '\\' '\''     { T_cconst }
  | '\\' '\'' '\''     { T_cconst }
  | '\\' '\"' '\''     { T_cconst }
  | '\\' "xnn" '\''    { T_cconst }
  | _ "\'"             { T_cconst }
  | _ as chr           { Printf.eprintf "Invalid character constant: '%c' (ascii: %d)"
					chr (Char.code chr);
			 T_eof }

and parse_string = shortest
  | [^ '\n']* "\""     { T_sconst }
  | eof                { let pos = lexbuf.Lexing.lex_curr_p in
			 Printf.eprintf "String terminated with EOF at line %d"
					 pos.pos_lnum;
			 T_eof }
  | '\n'               { let pos = lexbuf.Lexing.lex_curr_p in
			 Printf.eprintf "Multiline string at line %d"
					 pos.pos_lnum;
			 T_eof }

and comments level = parse
  | "*>"               { if level = 0 then lexer lexbuf 
			 else comments (level-1) lexbuf }
  | "<*"               { comments (level+1) lexbuf }
  | '\n'               { incr_linenum lexbuf; comments level lexbuf }
  | _                  { comments level lexbuf }
  | eof                { Printf.eprintf "Comments are not closed";
			 T_eof }
		       
{
  let string_of_token token = 
    match token with
      | T_eof      ->  "T_eof"
      | T_id       ->  "T_id"
      | T_and      ->  "T_and"     
      | T_bool     ->  "T_bool"    
      | T_char     ->  "T_char"    
      | T_decl     ->  "T_decl"   
      | T_def      ->  "T_def"     
      | T_else     ->  "T_else"    
      | T_elsif	   ->  "T_elsif"	  
      | T_end      ->  "T_end"     
      | T_exit     ->  "T_exit"    
      | T_false	   ->  "T_false"	  
      | T_for      ->  "T_for"     
      | T_head     ->  "T_head"    
      | T_if	   ->  "T_if"	   
      | T_int      ->  "T_int"     
      | T_list     ->  "T_list"    
      | T_mod      ->  "T_mod"     
      | T_new      ->  "T_new"     
      | T_nil      ->  "T_nil"     
      | T_nilq 	   ->  "T_nilq"  	  
      | T_not      ->  "T_not"     
      | T_or	   ->  "T_or"	  
      | T_ref      ->  "T_ref"     
      | T_return   ->  "T_return"  
      | T_skip     ->  "T_skip"    
      | T_tail     ->  "T_tail"    
      | T_true     ->  "T_true"    
      | T_iconst   ->  "T_iconst"  
      | T_cconst   ->  "T_cconst"  
      | T_sconst   ->  "T_sconst"  
      | T_plus 	   ->  "T_plus"	  
      | T_minus    ->  "T_minus"   
      | T_times    ->  "T_times"   
      | T_div 	   ->  "T_div" 	  
      | T_cons 	   ->  "T_cons" 	  
      | T_eq 	   ->  "T_eq"	  
      | T_neq 	   ->  "T_neq" 	  
      | T_less     ->  "T_less"    
      | T_greater  ->  "T_greater" 
      | T_le       ->  "T_le"     
      | T_ge       ->  "T_ge"     
      | T_lparen   ->  "T_lparen"  
      | T_rparen   ->  "T_rparen"  
      | T_lbracket ->  "T_lbracket"
      | T_rbracket ->  "T_rbracket"
      | T_comma    ->  "T_comma"   
      | T_semic    ->  "T_semic"   
      | T_colon    ->  "T_colon"   
      | T_assign   ->  "T_assign"  

  let main = 
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () = 
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
		    (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}
