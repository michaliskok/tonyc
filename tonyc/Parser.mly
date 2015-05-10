%{

open Semantics

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
%token <int> T_iconst
%token <char> T_cconst
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
%type <unit> func_def
%type <unit> header
%type <unit> formal
%type <unit> ttype
%type <unit> func_decl
%type <unit> var_def
%type <unit> stmt
%type <unit> simple
%type <unit> simple_list
%type <unit> call
%type <unit> atom
%type <unit> expr

%%

program     : func_def T_eof { () }

func_def    : T_def header T_colon func_optional* stmt+ T_end { () }

%inline func_optional :
	    | func_def
	    | func_decl
	    | var_def { () }

header      : ttype? T_id T_lparen separated_list(T_semic,formal) T_rparen { () }

formal      : T_ref? ttype separated_nonempty_list(T_comma,T_id) { () }

ttype       : T_int 
	    | T_bool
	    | T_char
	    | ttype T_lbracket T_rbracket 
	    | T_list T_lbracket ttype T_rbracket { () }

func_decl   : T_decl header { () }

var_def     : ttype separated_nonempty_list(T_comma,T_id)  { () }

stmt        : simple
	    | T_exit
	    | T_return expr
	    | T_if expr T_colon stmt+ list(preceded(T_elsif,separated_pair(expr,T_colon,stmt+))) 
	      preceded(T_else,preceded(T_colon,stmt+))? T_end
	    | T_for simple_list T_semic expr T_semic simple_list T_colon stmt+ T_end  { () }

simple      : T_skip
            | atom T_assign expr
	    | call  { () }

simple_list : separated_nonempty_list(T_comma,simple)  { () }

call        : T_id T_lparen separated_list(T_comma,expr) T_rparen  { () }

atom        : T_id
            | T_sconst
	    | atom T_lbracket expr T_rbracket
	    | call  { () }

expr        : atom
	    | T_iconst
	    | T_cconst
	    | T_true
	    | T_false
	    | T_lparen expr T_rparen
	    | unary_op expr %prec T_unary
	    | T_not expr
	    | expr binary_op expr
	    | T_new ttype T_lbracket expr T_rbracket
	    | T_nil
	    | T_nilq T_lparen expr T_rparen
	    | expr T_cons expr
	    | T_head T_lparen expr T_rparen
	    | T_tail T_lparen expr T_rparen  { () }
	      
%inline unary_op:
	    | T_minus 
	    | T_plus { () }

%inline binary_op:
	    | T_plus
	    | T_minus
	    | T_times
	    | T_div
	    | T_mod
	    | T_eq
	    | T_neq
	    | T_greater
	    | T_less
	    | T_ge
	    | T_le
	    | T_and
	    | T_or { () }
