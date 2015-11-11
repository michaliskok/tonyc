open Symbol
open Types
open Error
open PrintUtils
open Identifier
open Quads

let basic_types = [TYPE_int; TYPE_bool; TYPE_char]

(* Ensure that the unary operators (+,-) are applied on integers *)
let check_unary_exp e pos = match e with
  | TYPE_int -> ()
  | _        -> ( unary_error e pos; raise Terminate )

(* Ensure that the unary operator "not" is applied to booleans *)
let check_bool_exp e pos = match e with
  | TYPE_bool -> ()
  | _         -> ( bool_error e pos; raise Terminate )

(* Ensure that a list is constructed with the right operands *)
let check_list_cons hd tl pos =
  if TYPE_list (hd) = tl || TYPE_list TYPE_none = tl then ()
  else ( cons_error (TYPE_list hd) tl pos; raise Terminate )

(* Ensure that "nil?" operator is applied to lists only *)
let check_list_exp e pos = match e with
  | TYPE_list _ -> ()
  | _           -> ( list_fn_error "nil?" e pos; raise Terminate )

(* Check the integrity of any binary expression (lists are handled separately) *)
let check_binary_exp op e1 e2 pos =
  let equal_types = equalType e1 e2 in
  match op with
  | "+" | "-" | "*" | "/" | "mod"        -> if equal_types && e1 = TYPE_int then ()
					    else ( binop_int_error e1 e2 pos; raise Terminate )
  | "=" | "<>" | ">" | "<" | ">=" | "<=" -> if equal_types && List.mem e1 basic_types then ()
                                            else ( binop_comp_error e1 e2 pos; raise Terminate )
  | "and" | "or"                         -> if equal_types && e1 = TYPE_bool then ()
					    else ( binop_bool_error e1 e2 pos; raise Terminate )
  | _                                    -> internal "Not a binary expression?"
  
(* Perform semantic check for simple lvalues *)
let check_lvalue id pos =
  try 
    let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
    match ent.entry_info with
      | ENTRY_variable  varInfo -> (ent, varInfo.variable_type)
      | ENTRY_parameter parInfo -> (ent, parInfo.parameter_type)
      | _                       -> ( id_error id pos; raise Terminate )
   with Pervasives.Exit -> ( id_error id pos; raise Terminate )

(* Perform semantic check for array lvalues *)
let check_array_lvalue t e pos = match t with
  | TYPE_array _ when e = TYPE_int -> ()
  | TYPE_array _ -> ( array_lvalue_error e pos; raise Terminate )
  | _ -> ( array_lvalue_error_2 t pos ; raise Terminate )

(* Perform semantic check for array creation *)
let check_new_array index_t pos = match index_t with
  | TYPE_int -> ()
  | _        -> ( dynarr_error index_t pos; raise Terminate )

								
(* Perform semantic check for assigments in simple statements *)
(* check_lvalue is locally redefined for better error handling *)
let check_assign (atm : properties_variable) (exp_t: Types.typ) pos =
  let check_lvalue place = match place with
    | Q_entry ent -> (
      match ent.entry_info with
      | ENTRY_variable  varInfo -> ()
      | ENTRY_parameter parInfo -> ()
      | _                       -> ( assignment_id_error (id_name ent.entry_id) pos; raise Terminate )
    )
    (* If place is Q_pointer, then it has passed semantic analysis from check_array_lvalue *)
    | Q_pointer _ -> ()
    | _ -> ( assignment_id_error "" pos; raise Terminate ) in

  check_lvalue atm.place;
  if not (equalType atm.typ exp_t) then
    ( assign_error atm.typ exp_t pos; raise Terminate )
  else ()
;;

(* Takes a list of conditions and checks for each one if condition is a valid
   boolean expression. If not, the function raises a Terminate exception *)
let rec check_cond_list (cs : (properties_variable * properties_variable) list) pos = match cs with
  | [] -> ()
  | (c,_) :: rest -> ( check_bool_exp c.typ pos; check_cond_list rest pos )

(* Check if the return type of a function matches the expected *)
let check_fun_ret_type return_type pos = 
  if equalType !currentScope.sco_ret_type return_type then
    ()
  else
    begin
      ret_type_error return_type pos;
      raise Terminate
    end

(* Checks the program header declaration *)
let check_program_header (f : properties_variable) pos = match f.place with
  | Q_entry f ->
     begin
       match f.entry_info with
       | ENTRY_function funInfo ->
	  if (funInfo.function_result <> TYPE_none) || (funInfo.function_paramlist <> []) then
	    begin
	      program_error pos;
	      raise Terminate
	    end
       | _ ->
	  internal "Main function is not a function?";
	  raise Terminate
     end
  | _ ->
     internal "Something went wrong with header and def_header functions!";
     raise Terminate

(* Check that a function call in a simple statement has no return type *)
let check_fun_stmt call pos = match call.typ with
  | TYPE_none ->
     ()
  | _ ->
     fun_stmt_error call. typ pos;
     raise Terminate
  




      
(* Perform semantic check for function calls *)
let check_fun_call (id : string) (args : properties_variable list) pos =
  try (
    let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
    
    (* --- Checks if an argument is an lvalue -- boolean result --- *)
     let check_arg_lvalue place = match place with
    | Q_entry ent ->
       ( match ent.entry_info with
	 | ENTRY_variable  varInfo -> true
	 | ENTRY_parameter parInfo -> true
	 | _                       -> false
       )
    (* If place is Q_pointer, then it has passed semantic analysis from check_array_lvalue *)
    | Q_pointer _ -> true
    | _ -> false in
    (* ---------------- end of check_arg_lvalue ----------------- *)
    
    (* Takes a tuple consisting of the registered parameter for
     this function (symbol table form) and the parameters 
     with which the function call was made. *)
    let rec check_params (p_num : int) (valid_call : bool) = function
	
      | ([], []) -> if valid_call then
		      match ent.entry_info with
		      | ENTRY_function funInfo ->
			 (funInfo.function_result, ent)
		      | _ ->
			 ( internal "Program should have terminated! Exiting.."; raise Terminate )
		    else
		      raise Terminate
			    
      | ([], _)  -> ( fn_arg_error id pos; raise Terminate )
		      
      | (_, [])  -> ( fn_param_error id pos; raise Terminate )
		      
      | (fp1::fps, ar1::ars) -> 
	 match fp1.entry_info with
	 | ENTRY_parameter parInfo ->
	    let fp_t = parInfo.parameter_type in
	    let pass_mode = parInfo.parameter_mode in
	    let is_lvalue = check_arg_lvalue ar1.place in
	    let valid_pass = (pass_mode = PASS_BY_VALUE) || (is_lvalue) in
	    if (equalType fp_t ar1.typ && valid_pass) then
	      check_params (p_num+1) valid_call (fps,ars)
	    else (
	      param_type_error fp_t ar1.typ p_num valid_pass id pos;
	      check_params (p_num+1) false (fps,ars) )
		   
	 | _ ->
	    ( internal "Parameter is not a parameter?!"; raise Terminate ) in
    (* -------------- end of check_params -------------------- *)
    
    (* Main function body *)
    match ent.entry_info with
    | ENTRY_function funInfo -> check_params 1 true (funInfo.function_paramlist, args)
    | _                      -> ( fn_id_error id pos; raise Terminate )     
				  
  ) with Pervasives.Exit -> ( id_error id pos; raise Terminate )
  
  
