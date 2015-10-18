%{

open Quads
open Icode
open Semantics
open Symbol
open Types
open Identifier
open Error
open PrintUtils     

(* Function to declare variables *)
let rec declareVariables typ = function
  | [] -> ()
  | v :: vs -> ( ignore (newVariable (id_make v) typ true ); 
		 declareVariables typ vs )

(* Function to register the parameters of a function *)
let rec registerParameters f p_list =
  let rec registerVarGroup pass_mode typ = function
      | []      -> () 
      | p :: ps -> try ( ignore (newParameter (id_make p) typ pass_mode f true); registerVarGroup pass_mode typ ps )
		   with Pervasives.Exit -> raise Terminate in
  match p_list with
  | [] -> ()
  | (pass, typ, l) :: rest -> ( registerVarGroup pass typ l; registerParameters f rest )
				
(* Function to register functions *)
let registerFunction (ret_t, id, p_list) =
  let f = newFunction (id_make id) true in 
  ( openScope ret_t;
    registerParameters f p_list;
    endFunctionHeader f ret_t;
    f
  ) 

   

(* Function to declare a function *)
let declareFunction (ret_t, id, p_list) = (* evgala to isLib *)
  let f = newFunction (id_make id) true in
  ( openScope ret_t;
    registerParameters f p_list; (*des declaration sto symbtest gia sigouria*)
    forwardFunction f;
    endFunctionHeader f ret_t;
    closeScope ();
  )

(* Function to declare library functions *)    
let registerLibrary () =
  ignore(declareFunction (TYPE_none, "puti", [(PASS_BY_VALUE, TYPE_int, ["n"])]));
  ignore(declareFunction (TYPE_none, "putb", [(PASS_BY_VALUE, TYPE_bool, ["b"])]));
  ignore(declareFunction (TYPE_none, "putc", [(PASS_BY_VALUE, TYPE_char, ["c"])]));
  ignore(declareFunction (TYPE_none, "puts", [(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["s"])]));
  ignore(declareFunction (TYPE_int,  "geti", []));
  ignore(declareFunction (TYPE_bool, "getb", []));
  ignore(declareFunction (TYPE_char, "getc", []));
  ignore(declareFunction (TYPE_none, "gets", [(PASS_BY_VALUE, TYPE_int, ["n"]);
					       (PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["s"])]
			  ));
  ignore(declareFunction (TYPE_int,  "abs",  [(PASS_BY_VALUE, TYPE_int, ["n"])]));
  ignore(declareFunction (TYPE_int,  "ord",  [(PASS_BY_VALUE, TYPE_char, ["c"])]));
  ignore(declareFunction (TYPE_char, "chr",  [(PASS_BY_VALUE, TYPE_int, ["n"])]));
  ignore(declareFunction (TYPE_int, "strlen", [(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["s"])]));
  ignore(declareFunction (TYPE_int, "strcmp", [(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["s1"]);
						(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["s2"])]
			  ));
  ignore(declareFunction (TYPE_none, "strcpy", [(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["trg"]);
						 (PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["src"])]
			  ));
  ignore(declareFunction (TYPE_none, "strcat", [(PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["trg"]);
						 (PASS_BY_VALUE, TYPE_array(TYPE_char,0), ["src"])]
			  ));
;;


(* Function that extracts a function's return type (if there is one) from its header *)
let extract_fun_type fn_t = match fn_t with
  | Some x -> x
  | None   -> TYPE_none

(* Function that extracts a condition list from an if-elseif statement *)
let extract_conditions list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | ((_, e), _) :: rest -> aux (e :: acc) rest in
  aux [] list
    
%} 

%token T_and
%token T_bool
%token T_char
%token T_decl
%token T_def
%token T_else
%token T_elsif
%token T_end
%token T_exit
%token T_false
%token T_for
%token T_head
%token T_if
%token T_int
%token T_list
%token T_mod
%token T_new
%token T_nil
%token T_nilq
%token T_not
%token T_or
%token T_ref
%token T_return
%token T_skip
%token T_tail
%token T_true

%token <string> T_id
%token <string> T_iconst
%token <string> T_cconst
%token <string> T_sconst

%token T_plus
%token T_minus
%token T_times
%token T_div
%token T_cons
%token T_eq
%token T_neq
%token T_greater
%token T_less
%token T_ge
%token T_le

%token T_lparen
%token T_rparen
%token T_lbracket
%token T_rbracket
%token T_comma
%token T_semic
%token T_colon
%token T_assign

%token T_eof

%left T_or
%left T_and
%nonassoc T_not
%nonassoc T_eq T_neq T_greater T_less T_ge T_le
%right T_cons
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc T_unary

%start program
%type <unit> program
(*%type <Symbol.entry> func_def
%type <properties_variable> stmt
%type <properties_variable> call
%type <properties_variable> atom				
%type <properties_variable> expr*)

%%

program     : init; program_h = func_def; T_eof;
                        { check_program_header program_h ($startpos, $endpos); closeScope () }
 		
init        : { initSymbolTable 256; registerLibrary () }

		
func_def    : f = def_header; slist = stmt_list; T_end;
                        {
			  backpatch slist.next (nextQuad ());
			  add_quad (genQuad Q_endu (Q_entry f) Q_empty Q_empty);
			  closeScope ();
			  f
			}
	   (* (* func_def - parsing errors *)
	    | def_header; local_def*; stmt_list; error;
	                { missing_end_in_function ($startpos, $endpos); raise Terminate }*)

			    
def_header : T_def; h = header; T_colon; local_def*;
	                { let f = registerFunction h in 
			  add_quad (genQuad Q_unit (Q_entry f) Q_empty Q_empty);
			  f 
			}
	    (* def_header - parsing errors *)
	    | T_def; header; error;
	                { missing_colon_error ($startpos, $endpos); raise Terminate } 

%inline local_def :
	    | func_def  { () }
	    | func_decl { () } 
	    | var_def   { () }

			  
header      : fn_t = ttype?; id = T_id; T_lparen; params = separated_list(T_semic,formal); T_rparen;
	                { (extract_fun_type fn_t, id, params) }
	    (* header - parsing errors *)
	    | ttype?; error;
	                { id_expected_error ($startpos, $endpos); raise Terminate }
	    | ttype?; T_id; error;
	                { missing_lparen_error ($startpos, $endpos); raise Terminate }

		
formal      : by_ref = T_ref?; t = ttype; p_names = separated_nonempty_list(T_comma,T_id); 
	                {
			  if by_ref = Some () then
			    (PASS_BY_REFERENCE, t, p_names)
			  else
			    (PASS_BY_VALUE, t, p_names)
	                }
	    (* formal - parsing errors *)
	    | T_ref?; error;
	                { type_expected_error ($startpos, $endpos); raise Terminate }

		
ttype       : T_int     {  TYPE_int   }
	    | T_bool    {  TYPE_bool  }
	    | T_char    {  TYPE_char  }
	    | t = ttype; T_lbracket; T_rbracket; 
	                { TYPE_array (t,0) }
	    | T_list; T_lbracket; t = ttype; T_rbracket;
	                { TYPE_list t }

			  
func_decl   : T_decl; f = header;  
			{ declareFunction f }

			  
var_def     : t = ttype; vars = separated_nonempty_list(T_comma,T_id);  
			{ declareVariables t vars }
	    (* var_def - parsing errors *)
	    | error;
	                { var_def_error_2 ($startpos, $endpos); raise Terminate }

			  (* New rule, needed for I-code generation *)
stmt_list   : s = stmt
	                { icode_stmt s }
	    | slist = stmt_list; f = foo_stmt; s = stmt 
		        { icode_stmt_list slist f s }

foo_stmt    : (* empty *)
		        { nextQuad () }
			
stmt        : s = simple;
		       { s }
	    | T_exit;
	               { check_fun_ret_type TYPE_none ($startpos, $endpos);
			 default_properties
		       }
	    | T_return; e = expr;
	               { check_fun_ret_type e.typ ($startpos, $endpos);
			 icode_return_stmt e
		       }
	    | iff = if_stmt;
	               { { default_properties with next = iff.next } }
	    | forr = for_stmt;  
	               { { default_properties with next = forr.next } }	            
	    (*(* stmt - parsing errors *)	 
	    | T_return; error;
	               { missing_return_type ($startpos, $endpos); raise Terminate }
	    | T_if; error;
	               { expr_after_if_error ($startpos, $endpos); raise Terminate }
	    | T_if; expr; error;
	               { colon_after_expr_error ($startpos, $endpos); raise Terminate }
	    | T_if; expr; T_colon; stmt+; list(preceded(T_elsif,separated_pair(expr,T_colon,stmt+))); error;
	               { elsif_error ($startpos, $endpos); raise Terminate }
	    | T_if; expr; T_colon; stmt+; list(preceded(T_elsif,separated_pair(expr,T_colon,stmt+))); T_else; error;
	               { colon_after_else_error ($startpos, $endpos); raise Terminate }
	    | T_for; error;
	              { simple_after_for_error ($startpos, $endpos); raise Terminate }
	    | T_for; simple_list; error;
	              { semic_after_for_error ($startpos, $endpos); raise Terminate }
	    | T_for; simple_list; T_semic; error;
	              { expr_after_for_error ($startpos, $endpos); raise Terminate }
	    | T_for; simple_list; T_semic; expr; error;
	              { semic_after_cond_error ($startpos, $endpos); raise Terminate }
	    | T_for; simple_list; T_semic; expr; T_semic; error;
	              { simple_after_cond_error ($startpos, $endpos); raise Terminate }
	    | T_for; simple_list; T_semic; expr; T_semic; simple_list; error;
	              { colon_after_for_error ($startpos, $endpos); raise Terminate }*)

	    (* if <cond1> : <compound1> [elsif <cond2>: <compound2>] [else : <compound3>] end *)
if_stmt     : h = if_head; st = stmt_list; el = else_stmt; T_end;
	              { let cond1_false = h in
			let result = { default_properties with next = fst el } in
			match snd el with
			| e :: _ ->
			   ( backpatch cond1_false e; result )
			| [] ->
			   ( backpatch cond1_false (nextQuad ()); result ) (* Empty else clause *)
		      }
	    | h = if_head; slist = stmt_list; elif = elsif_stmt; els = else_stmt; T_end;
	              { let cond1_false = h in
			let elsif_next, cond2_q_false = elif in
			let else_next, compound3_q = els in 
			backpatch cond1_false !(List.hd cond2_q_false);
			let result = { default_properties with next = elsif_next @ else_next @ slist.next } in
			match compound3_q with
			| h :: _ -> ( backpatch (List.tl cond2_q_false) h; result )
			| [] -> { result with next = result.next @ (List.tl cond2_q_false) } 
		      }

            (* Backpatches <cond1>.TRUE and returns <cond>.FALSE *)
if_head     : T_if; e = expr; T_colon;
                      { check_bool_exp e.typ ($startpos, $endpos);
			backpatch e.true_l (nextQuad ());
			e.false_l
		      }

	    (* Returns (else next, <compound3>.NEXT) *) 		       
else_stmt   : (* empty *)
		      { ([], []) } 
	    | h = else_head; slist = stmt_list;
	              { ((fst h)::(slist.next), [snd h]) }

	    (* Returns (ref <jump2>.quad_num, <compound3>.quad_num) *)
else_head   : T_else; T_colon;
		      {
			let l = ref (nextQuad ()) in
			add_quad (genQuad Q_jump Q_empty Q_empty (Q_label l));
			(l, nextQuad ())
		      }

elsif_stmt  : h = elsif_head; slist = stmt_list;
		      {
			match h with
			| (jump1, cond2_false_q) -> (jump1 :: slist.next, cond2_false_q)
		      } 
	    | elif = elsif_stmt; h = elsif_head; slist = stmt_list;
	              {
			match elif, h with
			| (j_next, (cond_q :: cond_false)),
			  (j1_q, (cond2_q :: cond2_false)) -> 
			   ( backpatch cond_false !cond2_q;
			     (j_next @ (j1_q :: slist.next), cond_q :: cond2_false) )
			| _ ->
			   (* internal "Problem in elsif_stmt rule";*) raise Terminate 
		      }

elsif_head  : el = elsif; e = expr; T_colon;
		      {
			check_bool_exp e.typ ($startpos, $endpos);
			backpatch e.true_l (nextQuad ());
			(fst el, (snd el)::e.false_l)
		      }

elsif       : T_elsif; 
		      {
			let l = ref (nextQuad ()) in
			add_quad (genQuad Q_jump Q_empty Q_empty (Q_label l));
			(l, ref (nextQuad ()))
		      }
			

for_stmt    : h = for_head; slist = stmt_list; T_end;
		      { icode_for_stmt h slist }
				  
		      
for_head    : init = for_init; cond = for_cond; simple_list; T_colon;
                      {
			backpatch cond.true_l (nextQuad ());
			(init, cond)
		      }

for_init    : T_for; simple_list; T_semic;
                      { let q = ref (nextQuad ()) in q }

for_cond    : e = expr; T_semic;
		      { check_bool_exp e.typ ($startpos, $endpos);
			e
		      }


simple      : T_skip;
		      { default_properties }
            | atm = atom; T_assign; e = expr;
	              { check_assign atm e.typ ($startpos, $endpos);
			icode_assign_stmt atm e
	              }
	    | c = call;
	              { { default_properties with place = c.place } }
	    (* simple - parsing errors *)
	    | atom; T_assign; error;
	              { assignment_error ($startpos, $endpos); raise Terminate }

		       
simple_list : separated_nonempty_list(T_comma, simple)  { () }

				 				     
call        : id = T_id; T_lparen; args = separated_list(T_comma, expr); T_rparen;
	              { let (res, ent) = check_fun_call id args ($startpos, $endpos) in
			icode_fun_call res ent args
		      }
	    (* call - parsing errors *)
	    | T_id; T_lparen; error;
	              { wrong_function_call ($startpos, $endpos); raise Terminate }

			
atom        : id = T_id;
	              { let (ent,t) = check_lvalue id ($startpos, $endpos) in			
		        { default_properties with place = Q_entry ent; typ = t }
		      }
            | s = T_sconst;
	              { let size = String.length s + 1 in
			{ default_properties with place = Q_string s; typ = TYPE_array (TYPE_char, size) }
		      } 
	    | atm = atom; T_lbracket; e = expr; T_rbracket;
	              { check_array_lvalue atm.typ e.typ ($startpos, $endpos);
			icode_array_atom atm e
		      }		
	    | call_code = call;    
	              { call_code }
	    (* atom - parsing errors *)
	    | atom; T_lbracket; expr; error;
	              { missing_rbracket_error ($startpos, $endpos); raise Terminate }       
			
			
expr        : atm = atom;     (* 8elei douleia auto -- nomizw einai ok, pigaine sto call *)
		      { atm }
	    | i = T_iconst;
	              { { default_properties with place = Q_int (i); typ = TYPE_int }  }
	    | c = T_cconst;
		      { { default_properties with place = Q_char (c); typ = TYPE_char } }
	    | T_true;
	              { let x = ref (nextQuad ()) in
			{ default_properties with place = Q_bool ("true"); typ = TYPE_bool; true_l = [x] } }
	    | T_false;
	              { let x = ref (nextQuad ()) in (* einai skata, des mipws 8elei Q_jump (x) sto code *)
			{ default_properties with place = Q_bool ("false"); typ = TYPE_bool; false_l = [x] } }
	    | T_nil;
                      { { default_properties with place = Q_nil; typ = TYPE_list TYPE_none; } } (* 8ELEI DOULEIA EDW, des gia to list_cons ti paizei -- dokimes me listes *)
	    | T_lparen; e = expr; T_rparen
		      { e }
	    | op = unary_op; e = expr %prec T_unary
		      { check_unary_exp e.typ ($startpos, $endpos);
			icode_unary_exp op e
		      }
	    | T_not; e = expr;
	              { check_bool_exp e.typ ($startpos, $endpos); 
		        { e with true_l = e.false_l; false_l = e.true_l }
		      }
	    | e1 = expr; op = binary_op; e2 = expr;
	              { check_binary_exp op e1.typ e2.typ ($startpos, $endpos);
			icode_binary_exp op e1 e2
		      }
	    | e1 = expr; op = boolean_op; e2 = expr; (* dokimh *)
	              { check_binary_exp op e1.typ e2.typ ($startpos, $endpos);
			icode_boolean_exp op e1 e2
		      }
	    | e1 = expr; op = cmp_op; e2 = expr; (* dokimh *)
	              { check_binary_exp op e1.typ e2.typ ($startpos, $endpos);
		 	icode_relop_exp op e1 e2
		      }
	    | e1 = expr; T_cons; e2 = expr;
	              { check_list_cons e1.typ e2.typ ($startpos, $endpos);
			icode_list_cons e1 e2
		      }
	    | T_new; t = ttype; T_lbracket; e = expr; T_rbracket;
                      { check_new_array e.typ ($startpos, $endpos);
			icode_new_array t e
		      }
	    | T_nilq; T_lparen; e = expr; T_rparen;
	              { check_list_exp e.typ ($startpos, $endpos);
		        icode_list_nilq e
		      }
	    | T_head; T_lparen; e = expr; T_rparen;
 	              { check_list_exp e.typ ($startpos, $endpos);
			icode_list_head e
		      }
	    | T_tail; T_lparen; e = expr; T_rparen;
	              { check_list_exp e.typ ($startpos, $endpos);
			icode_list_tail e
		      }
	    (* expr parsing errors *)
	    | expr; binary_op; error;
	              { invalid_binexp_error ($startpos, $endpos); raise Terminate }
	    | expr; cmp_op; error;
	              { invalid_cmp_error ($startpos, $endpos); raise Terminate }
	    | T_lparen; expr; error; 
	              { missing_rparen_error ($startpos, $endpos); raise Terminate }
		  
%inline unary_op:
	    | T_minus   {  "-"  }
	    | T_plus    {  "+"  }

%inline binary_op:
	    | T_plus    {  "+"  }
	    | T_minus   {  "-"  }
	    | T_times   {  "*"  }
	    | T_div     {  "/"  }
	    | T_mod     { "mod" }

%inline cmp_op:
	    | T_eq      {  "="  }
	    | T_neq     { "<>"  }
	    | T_greater {  ">"  }
	    | T_less    {  "<"  }
	    | T_ge      { ">="  }
	    | T_le      { "<="  }

%inline boolean_op:
            | T_and     { "and" }
	    | T_or      { "or"  }
 
  
