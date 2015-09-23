%{

open Semantics
open Symbol
open Types
open Identifier
open Error
open PrintUtils

(* Return type for various grammar rules *)
type formal_t = pass_mode * typ * string list
type header_t = typ * string * formal_t list
type expr_t = string * typ

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
  ignore(declareFunction (TYPE_none, "puts", [(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["s"])]));
  ignore(declareFunction (TYPE_int,  "geti", []));
  ignore(declareFunction (TYPE_bool, "getb", []));
  ignore(declareFunction (TYPE_char, "getc", []));
  ignore(declareFunction (TYPE_none, "gets", [(PASS_BY_VALUE, TYPE_int, ["n"]);
					       (PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["s"])]
			  ));
  ignore(declareFunction (TYPE_int,  "abs",  [(PASS_BY_VALUE, TYPE_int, ["n"])]));
  ignore(declareFunction (TYPE_int,  "ord",  [(PASS_BY_VALUE, TYPE_char, ["c"])]));
  ignore(declareFunction (TYPE_char, "chr",  [(PASS_BY_VALUE, TYPE_int, ["n"])]));
  ignore(declareFunction (TYPE_int, "strlen", [(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["s"])]));
  ignore(declareFunction (TYPE_int, "strcmp", [(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["s1"]);
						(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["s2"])]
			  ));
  ignore(declareFunction (TYPE_none, "strcpy", [(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["trg"]);
						 (PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["src"])]
			  ));
  ignore(declareFunction (TYPE_none, "strcat", [(PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["trg"]);
						 (PASS_BY_REFERENCE, TYPE_array(TYPE_char,0), ["src"])]
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
%token <int>    T_iconst
%token <char>   T_cconst
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
%type <header_t> main_func_def
%type <unit> func_def 
%type <header_t> header
%type <formal_t> formal
%type <typ> ttype
%type <unit> func_decl
%type <unit> var_def
%type <unit> stmt
%type <unit> simple
%type <unit> simple_list
%type <expr_t> call
%type <expr_t> atom				
%type <expr_t> expr

%%

program     : init; program_h = main_func_def; T_eof;
              { check_program_header program_h ($startpos, $endpos); closeScope () }
		
init        : { initSymbolTable 256; openScope TYPE_none; }

		
main_func_def : T_def; h = header; T_colon; reg_lib; local_def*; stmt+; T_end;
	      { h }
            (* main_func_def - parsing errors *)
	    | T_def; header; T_colon; reg_lib; local_def*; stmt+; error; T_eof;
	      { missing_end_in_main ($startpos, $endpos); raise Terminate }
	    | error; header;
	      { missing_def_in_main ($startpos, $endpos); raise Terminate }

		
reg_lib     : { registerLibrary () }

		
func_def    : def_header; local_def*; stmt+; T_end;
                        { closeScope () }
	    (* func_def - parsing errors *)
	    | def_header; local_def*; stmt+; error;
	                { missing_end_in_function ($startpos, $endpos); raise Terminate }

%inline local_def :
	    | func_def  { () }
	    | func_decl { () } 
	    | var_def   { () }

			    
def_header : T_def; h = header; T_colon;
	                { registerFunction h }
	    (* def_header - parsing errors *)
	    | T_def; header; error;
	                { missing_colon_error ($startpos, $endpos); raise Terminate }


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

			  
stmt        : simple;  { () }
	    | T_exit;
	               { check_fun_ret_type TYPE_none ($startpos, $endpos);
			 ()
		       }
	    | T_return; e = expr;
	               { let (_, exp_t) = e in
			 check_fun_ret_type exp_t ($startpos, $endpos);
			 ()
		       }
	    | T_if; e = expr; T_colon; stmt+; elif_list = list(preceded(T_elsif,separated_pair(expr,T_colon,stmt+))); 
	      preceded(T_else,preceded(T_colon,stmt+))?; T_end;
	               { let conditions = extract_conditions ((e,[()]) :: elif_list) in
			 check_cond_list conditions ($startpos, $endpos);
			 () }
	    | T_for; simple_list; T_semic; e = expr; T_semic; simple_list; T_colon; stmt+; T_end;  
	               { let (_, exp_t) = e in
			 check_bool_exp exp_t ($startpos, $endpos);
			 ()
	               }
	    (* stmt - parsing errors *)	 
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
	              { colon_after_for_error ($startpos, $endpos); raise Terminate }
		
			
simple      : T_skip; { () }
            | atm = atom; T_assign; e = expr;
	              { let (_, exp_t) = e in
			check_assign atm exp_t ($startpos, $endpos);
			()
	              }
	    | call;   { () }
	    (* Simple - parsing errors *)
	    | atom; T_assign; error;
	              { assignment_error ($startpos, $endpos); raise Terminate }

		       
simple_list : separated_nonempty_list(T_comma, simple)  { () }

				 				     
call        : id = T_id; T_lparen; args = separated_list(T_comma, expr); T_rparen;
	              { let ret_t = check_fun_call id args ($startpos, $endpos) in
		        (id, ret_t)
		      }
	    | T_id; T_lparen; error;
	              { wrong_function_call ($startpos, $endpos); raise Terminate }

			
atom        : id = T_id;
	              { let t = check_lvalue id ($startpos, $endpos); in
		        (id, t)
		      }
            | s = T_sconst;
	              { let size = String.length s + 1 in (* edw alla3a to size logw tou semanticerr2 *)
			("", TYPE_array (TYPE_char, size))
		      } 
	    | atm = atom; T_lbracket; e = expr; T_rbracket;
	              { let (id, _) = atm in
			let (_, t) = e in
			let et = check_array_lvalue id t ($startpos, $endpos) in
			(id, et)
		      }		
	    | ret_t = call;    
	              { ret_t }
	    (* atom - parsing errors *)
	    | atom; T_lbracket; expr; error;
	              { missing_rbracket_error ($startpos, $endpos); raise Terminate }				       
			
			
expr        : atm = atom;     
		      { atm }
	    | T_iconst;
		      { ("", TYPE_int) }
	    | T_cconst;
		      { ("", TYPE_char) }
	    | T_true;
		      { ("", TYPE_bool) }
	    | T_false;
		      { ("", TYPE_bool) }
	    | T_nil;
                      { ("", TYPE_list TYPE_none) }
	    | T_lparen; e = expr; T_rparen
		      { e }
	    | unary_op; e = expr %prec T_unary
		      { let (id, t) = e in
		        check_unary_exp t ($startpos, $endpos);
			("", t)
		      }
	    | T_not; e = expr;
	              { let (id, t) = e in
			check_bool_exp t ($startpos, $endpos);
		        ("", t)
		      }
	    | e1 = expr; op = binary_op; e2 = expr;
	              { let (id_1, t1) = e1 in
			let (id_2, t2) = e2 in
			check_binary_exp op t1 t2 ($startpos, $endpos);
			("", t1)
		      }
	    | e1 = expr; op = boolean_op; e2 = expr;
	              {
			let (id_1, t1) = e1 in
			let (id_2, t2) = e2 in
			check_binary_exp op t1 t2 ($startpos, $endpos);
			("", t1)
		      }
	    | e1 = expr; op = cmp_op; e2 = expr;
	              { let (id_1, t1) = e1 in
			let (id_2, t2) = e2 in
			check_binary_exp op t1 t2 ($startpos, $endpos);
		 	("", TYPE_bool)
		      }
	    | e1 = expr; T_cons; e2 = expr;
	              { let (id_1, t1) = e1 in
			let (id_2, t2) = e2 in
			check_list_cons t1 t2 ($startpos, $endpos);
			("", TYPE_list t1)
		      }
	    | T_new; t = ttype; T_lbracket; e = expr; T_rbracket;
                      { let (id, idx) = e in
			check_new_array idx ($startpos, $endpos);
			("", TYPE_array (t, 0))
		      }
	    | T_nilq; T_lparen; e = expr; T_rparen;
	              { let (id, t) = e in
			check_list_exp t ($startpos, $endpos);
		        ("", TYPE_bool)
		      }
	    | T_head; T_lparen; e = expr; T_rparen;
 	              { let (id, t) = e in
			check_list_exp t ($startpos, $endpos);
			("", extractType t)
		      }
	    | T_tail; T_lparen; e = expr; T_rparen; 
	              { let (id, t) = e in
			check_list_exp t ($startpos, $endpos);
			("", t)
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
