open Types
open Symbol
open Error
open Format
       
(* Type for all the different operands in a quad *)
type operand_t =
  | Q_none                         (* For error handling and initialization *)
  | Q_int     of int
  | Q_char    of string
  | Q_string  of (string * string) (* string * escaped string *)
  | Q_bool    of bool
  | Q_label   of int ref           (* int ref -> When true_l or false_l is modified, label is also modified! *)
  | Q_pass    of Symbol.pass_mode
  | Q_empty
  | Q_result  of Types.typ
  | Q_nil
  | Q_pointer of (Symbol.entry * Types.typ)
  | Q_entry   of Symbol.entry
 (* External function calls (no entry in Symbol Table *)
  | Q_head | Q_tail | Q_consp | Q_consv | Q_isNil
  | Q_newp | Q_newv | Q_extend | Q_shrink
		 
(* Type for all the different operators in a quad *)
and operator_t =
   | Q_unit   
   | Q_endu   
   | Q_array
   | Q_plus
   | Q_minus
   | Q_mult
   | Q_div
   | Q_mod       
   | Q_assign
   | Q_eq
   | Q_neq
   | Q_greater
   | Q_less
   | Q_leq
   | Q_geq
   | Q_ifb
   | Q_jump
   | Q_call
   | Q_par
   | Q_ret

(* Quad type *)
and quad = {
  mutable label : int;
  mutable op : operator_t;
  mutable x  : operand_t;
  mutable y  : operand_t;
  mutable z  : operand_t;
}

(* Semantic properties variable *)
and properties_variable = {
  mutable code    : quad list;
  mutable place   : operand_t;
  mutable typ     : Types.typ;
  mutable true_l  : int ref list;
  mutable false_l : int ref list;
  mutable next    : int ref list;
}

			 (***************************************************)
		         (* Basic functions and variables for quad handling *)
                         (***************************************************)

let default_properties = {
  code    = [];
  place   = Q_none;
  typ     = TYPE_none;
  true_l  = [];
  false_l = [];
  next    = [];
}

(* Number of next quad *)
let quadNext = ref 1

(* Function that returns the number of the next quad *)
let nextQuad () = !quadNext

(* Function that generates a new quad and increases next quad number *)
let genQuad op x y z =
  let q = {
    label = nextQuad ();
    op = op;
    x = x;
    y = y;
    z = z;
  } in
  incr quadNext;
  q

(* Function that backpatches quads in quads list 
   Backpatching is done by using references (true_l and false_l contain the same references
   with the quad) for better performance.
   When true_l or false_l is backpatched, quad is also backpatched (because of the references!) *)
let backpatch (list : int ref list) (label : int) =
  List.iter (fun x -> x := label) list
   
(* Get operator_t type from operator (string) *)
let operator_of_str op = match op with
  | "+"   -> Q_plus
  | "-"   -> Q_minus
  | "*"   -> Q_mult
  | "/"   -> Q_div
  | "mod" -> Q_mod
  | "="   -> Q_eq
  | "<>"  -> Q_neq
  | ">"   -> Q_greater
  | "<"   -> Q_less
  | "<="  -> Q_leq
  | ">="  -> Q_geq
  | _ -> ( internal "Pattern matching went wrong!"; raise Terminate )

let str_of_operator = function
  | Q_unit    -> "unit"
  | Q_endu    -> "endu"
  | Q_array   -> "array"
  (**)
  | Q_plus    -> "+"
  | Q_minus   -> "-"
  | Q_mult    -> "*"
  | Q_div     -> "/"
  | Q_mod     -> "%"
  | Q_assign  -> ":="
  | Q_eq      -> "="
  | Q_neq     -> "<>"
  | Q_greater -> ">"
  | Q_less    -> "<"
  | Q_leq     -> "<="
  | Q_geq     -> ">="
  (**)
  | Q_ifb     -> "ifb"
  | Q_jump    -> "jump"
   (**)
  | Q_call    -> "call"
  | Q_par     -> "par"
  | Q_ret     -> "ret"

let str_of_passmode = function
  | PASS_BY_VALUE     -> "V"
  | PASS_BY_REFERENCE -> "R"
  | PASS_RET          -> "RET"

let str_of_operand = function
  | Q_int i         -> string_of_int i
  | Q_char c        -> c
  | Q_string  (s,_) -> "\"" ^ s ^ "\""
  | Q_bool b        -> string_of_bool b
  | Q_label i       -> string_of_int (!i)
  | Q_pass pm       -> str_of_passmode pm
  | Q_empty         -> "-"
  | Q_result _      -> "$$"
  | Q_nil           -> "nil"
  | Q_pointer (e,_) -> "[" ^ (Identifier.id_name e.entry_id) ^ "]" 
  | Q_entry e       -> Identifier.id_name e.entry_id
  | Q_none          -> "_INVALID_"
  (**)
  | Q_head   -> "head"
  | Q_tail   -> "tail"
  | Q_isNil  -> "nilq" (* doesn't exist! *)
  | Q_consp  -> "consp"
  | Q_consv  -> "consv"
  (**)
  | Q_newp   -> "newarrp"
  | Q_newv   -> "newarrv"
  | Q_extend -> "extend"
  | Q_shrink -> "shrink"

(* Find the reverse operator of any comparison operator *)
let rev_relop op = match op with
  | Q_eq      -> Q_neq
  | Q_neq     -> Q_eq
  | Q_less    -> Q_geq
  | Q_greater -> Q_leq
  | Q_geq     -> Q_less
  | Q_leq     -> Q_greater
  | _ -> internal "Not a relop!"; raise Terminate

(* Find the function that corresponds to a binary operator *)
let fun_of_operator = function
  | Q_plus  -> ( + )
  | Q_minus -> ( - )
  | Q_mult  -> ( * )
  | Q_div   -> ( / )
  | Q_mod   -> (mod)
  | _ -> internal "Not a binary operator"; raise Terminate

(* Find the size of an operand *)
let size_of_operand ?(sizeof = sizeOfType) op = match op with
  | Q_int _        -> sizeOfType TYPE_int
  | Q_char _       -> sizeOfType TYPE_char
  | Q_string (s,_) -> sizeof (TYPE_array (TYPE_char, String.length s + 1))
  | Q_bool _       -> sizeOfType TYPE_bool
  | Q_entry e ->
     begin
       match e.entry_info with
       | ENTRY_variable v -> sizeof v.variable_type
       | ENTRY_parameter p -> sizeof p.parameter_type
       | ENTRY_temporary t -> sizeof t.temporary_type
       | _ -> internal "This should never be matched!"; raise Terminate
     end
  | Q_pointer (e, t) -> sizeof t
  | Q_nil            -> sizeOfType TYPE_int
  | Q_result r       -> sizeOfType r
  | _ -> internal "This rule should never be matched!"; raise Terminate

(* Extracts the label of a quad operand *)
let getLabel operand = match operand with
  | Q_label l -> !l
  | _ -> internal "Cannot extract label from a non-jump quad!"; raise Terminate 
									
(* Prints a quad, if the quad is not invalid. 
   Invalid quads have their third operand equal to Q_none  *)
let print_quad ch q =
  if q.z <> Q_none then
    Printf.fprintf ch "%d: %s, %s, %s, %s\n"
    q.label (str_of_operator q.op) (str_of_operand q.x)
    (str_of_operand q.y) (str_of_operand q.z)
  else ()

(* Print quads to file *)
let print_quads channel quads = 
  ignore (Array.iter (print_quad channel) quads)

(* Normalizes quads' numbers *)
let normalize quads =
  let replace_quad old neww =
    let aux quad = 
      if quad.label = old then
	quad.label <- neww;
      match quad.z with
      | Q_label label ->
	 if !label = old then
	   label := neww (* No further correction will be needed :) *)
      | _ ->
	 ()
    in Array.iter aux quads
  in
  let next_quad = ref 1 in
  let needs_fix quad =
    if quad.label <> !next_quad && quad.z <> Q_none then
      (* if quad needs fix and it's valid fix it *)
      begin
	replace_quad quad.label !next_quad;
	next_quad := !next_quad + 1
      end
  in
  Array.iter needs_fix quads
