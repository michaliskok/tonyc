open Types
open Error
open Format
open Symbol
open Identifier

let rec string_of_type t = match t with
  | TYPE_none         -> "no type"
  | TYPE_int          -> "int"
  | TYPE_bool         -> "bool"
  | TYPE_char         -> "char"
  | TYPE_array (t, _) -> String.concat " " [(string_of_type t); "array"]
  | TYPE_list t       -> String.concat " " [(string_of_type t); "list"]



let show_offsets = true

let rec pretty_typ ppf typ =
  match typ with
  | TYPE_none ->
      fprintf ppf "none"
  | TYPE_int ->
      fprintf ppf "int"
  | TYPE_char ->
     fprintf ppf "char"
  | TYPE_bool ->
     fprintf ppf "bool"
  | TYPE_array (et, sz) ->
      pretty_typ ppf et;
      if sz > 0 then
        fprintf ppf " [%d]" sz
      else
        fprintf ppf " []"
  | TYPE_list t ->
     pretty_typ ppf t;
     fprintf ppf "list"

let pretty_mode ppf mode =
  match mode with
  | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
  | _ ->
      ()

let printSymbolTable () =
  let rec walk ppf scp =
    if scp.sco_nesting <> 0 then begin
      fprintf ppf "scope: ";
      let entry ppf e =
        fprintf ppf "%a" pretty_id e.entry_id;
        match e.entry_info with
        | ENTRY_none ->
            fprintf ppf "<none>"
        | ENTRY_variable inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.variable_offset
        | ENTRY_function inf ->
            let param ppf e =
              match e.entry_info with
                | ENTRY_parameter inf ->
                   fprintf ppf "%a%a : %a"
                      pretty_mode inf.parameter_mode
                      pretty_id e.entry_id
                      pretty_typ inf.parameter_type
                | _ ->
                    fprintf ppf "<invalid>" in
            let rec params ppf ps =
              match ps with
              | [p] ->
                  fprintf ppf "%a" param p
              | p :: ps ->
                  fprintf ppf "%a; %a" param p params ps;
              | [] ->
                  () in
            fprintf ppf "(%a) : %a"
              params inf.function_paramlist
              pretty_typ inf.function_result
        | ENTRY_parameter inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.parameter_offset
        | ENTRY_temporary inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.temporary_offset in
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
      match scp.sco_parent with
      | Some scpar ->
          fprintf ppf "%a\n%a"
            entries scp.sco_entries
            walk scpar
      | None ->
          fprintf ppf "<impossible>\n"
    end in
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      walk ppf scp in
  printf "%a----------------------------------------\n"
    scope !currentScope


    




(* Various error messages for parsing errors *)
let main_program_error () =
  fatal "Invalid main program definition."

let missing_colon_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing colon in function definition."

let invalid_binexp_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid binary expression syntax."

let invalid_unexp_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid unary expression syntax."

let missing_rparen_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "No matching closing parenthesis found."

let missing_lparen_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "No matching opening parenthesis found."

let invalid_expr_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid expression syntax."

let invalid_cmp_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid syntax in comparison expression."

let missing_rbracket_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing ']' character in expression."

let missing_lbracket_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing ']' character in expression."

let wrong_function_call (pos_start, pos_end) = 
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Wrong syntax in function call. Expected form:
	 \"<identifier> ( <arguments_separated_by_commas> )\"."

let assignment_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Expression expected in assignment."

let assignment_error_2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Expression has to be assigned in an l-value (atom)."

let missing_return_type (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "A return statement has to be followed by the respective type."

let missing_end_in_main (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Main function has to be terminated by the \"end\" keyword."

let missing_def_in_main (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Main function has to start with the \"def\" keyword."

let missing_end_in_function (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "A function has to be terminated by the \"end\" keyword."

let expr_after_if_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Boolean expression is expected after \"if\" keyword."

let colon_after_expr_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing colon after expression in \"if\" statement."

let colon_after_else_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing colon after \"else\" in \"if\" statement."

let expr_after_elsif_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Boolean expression if expected after \"elsif\" keyword."
	
let missing_end_in_if (pos_start, pos_end) = 
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing \"end\" keyword in \"if\" statement."

let simple_after_for_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing initialization statement(s) after \"for\" statement."

let semic_after_for_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing semicolon in \"for\" statement."

let semic_after_cond_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing semicolon after condition in \"for\" statement."

let expr_after_for_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing boolean condition in \"for\" statement."

let simple_after_cond_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing incremental statement(s) after condition in\
	 \"for\" statement."

let colon_after_for_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing colon in \"for\" statement."

let missing_end_in_for (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Missing \"end\" keyword in \"for\" statement."

let var_def_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Variables of the same type must be separated by commas."

let var_def_error_2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Variable declaration: type must be followed by \
	 variable names separated by commas."

let type_expected_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Valid type expected before argument list. \
	 Variable lists of different types must be \
	 separated by commas."

let invalid_type_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid type."

let id_expected_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Identifier expected in function header."

let missing_lparen_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "No opening parenthesis found in function header."

let parameters_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Parameter definition: Parameter groups of different \
	 types must be separated by semicolons (;), while \
	 parameters of the same type must be separated by commas."
  
	





	
    
(* Various error messages for semantic errors *)
let binop_int_error e1 e2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Operator and operand don't agree.\n\
	 int * int was expected, but found %s * %s instead."
	(string_of_type e1) (string_of_type e2)

let binop_comp_error e1 e2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Operator and operand don't agree.\n\
	 Comparisons can be performed only between variables \
	 of the same, basic type."
	
let binop_bool_error e1 e2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Operator and operand don't agree.\n\
	 bool * bool was expected, but found %s * %s instead."
	(string_of_type e1) (string_of_type e2)

let cons_error e1 e2 (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type miscmatch: Operator and operand don't agree.\n\
	 Lists can be constructed only from basic types - \
	 %s * %s is invalid for list construction."
	(string_of_type e1) (string_of_type e2)
	
let bool_error exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Boolean expression was expected, \
	 but found %s instead."
	(string_of_type exp)

let unary_error exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Integer expression was expected, \
	 but found %s instead."
	(string_of_type exp)

let atom_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Invalid array expression!"

let assign_error atom_t expr_t (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Assignment error: %s expected, but found %s instead."
	(string_of_type atom_t) (string_of_type expr_t)

let array_lvalue_error exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: The index to be accessed \
	 has to be a valid integer (found \"%s\" instead)."
	(string_of_type exp)

let array_lvalue_error_2 exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Type \"%s\" is not an array type and \
	 cannot be accessed by indexing."
	(string_of_type exp)	
	
let dynarr_error exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: The size of the array to be created \
	 has to be a valid integer (found \"%s\" instead)."
	(string_of_type exp)

let list_fn_error fn exp (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Type mismatch: Built-in function \"%s\" can be applied \
	 only to lists. Invalid operand: %s."
	fn (string_of_type exp)

let id_error id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Wrong or undeclared identifier: The given identifier \"%s\" does \
	 not correspond to a declared variable or a parameter." id

let assignment_id_error id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Assignment error: The left-hand side identifier \"%s\" is not \
	 a valid l-value for assigment." id
	
let fn_param_error id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Too few arguments for function \"%s\"." id

let fn_arg_error id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Too many arguments for function \"%s\"." id

let fn_id_error id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "The given identifier \"%s\" does not correspond to \
	 a function." id

let param_type_error e_typ typ p_num valid_pass id (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  if valid_pass then
    error "Type mismatch for parameter %d, in function \"%s\". 
	   %s was expected, found %s instead."
	  p_num id (string_of_type e_typ) (string_of_type typ)
  else
    error "Wrong passing mode for parameter %d, in function \"%s\". 
	   (Argument is not an l-value?)."
	  p_num id
	  
let ret_type_error ret_t (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  let exp_t = !currentScope.sco_ret_type in
  print_position err_formatter pos;
  error "Invalid function return type! %s was expected, \
	 found %s instead."
	(string_of_type exp_t) (string_of_type ret_t)

let program_error (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "Main program should not have a return type, \
	 or take any parameters!"

let fun_stmt_error typ (pos_start, pos_end) =
  let pos = position_context pos_start pos_end in
  print_position err_formatter pos;
  error "This statement should not have a return type, \
	 but this function call returns %s instead."
	(string_of_type typ)
