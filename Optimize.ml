open Quads
open Error
open Symbol

(* ------------------------------------------------------------------------ *)
(*                    HELPER FUNCTIONS FOR OPTIMIZATIONS                    *)
(* ------------------------------------------------------------------------ *)

(* Returns true if quad is jump of any kind *)
let isJump quad = match quad.op with
  | Q_jump | Q_eq | Q_neq | Q_greater
  | Q_less | Q_geq | Q_leq | Q_ifb -> true
  | _ -> false

(* Returns true if quad is a direct jump *)
let isDirectJump quad = match quad.op with
  | Q_jump -> true
  | _ -> false

(* Returns true if quad operator is a relative one *)
let isRelop quad = match quad.op with
  | Q_eq | Q_neq | Q_greater | Q_less | Q_geq | Q_leq -> true
  | _ -> false

(* Returns true if quad operator is a binary one *)
let isBinop quad = match quad.op with
  | Q_plus | Q_mult | Q_div | Q_mod -> true
  | Q_minus when quad.y <> Q_empty -> true (* avoid unary minus *)
  | _ -> false

(* Returns true if quad is an assignment quad *)
let isAssign quad = match quad.op with
  | Q_assign -> true
  | _ -> false

(* Extracts the label of a jump quad *)
let getLabel quad = match quad.z with
  | Q_label reff -> !reff
  | _ -> internal "Trying to extract label from a non-jump quad"; raise Terminate

(* Checks if a quad has been invalidated *)
let isValid quad = match quad.z with
  | Q_none -> false
  | _ -> true

(* Helper function for comparison between operators in a quad, 
   in order to avoid complex equality tests, which lead to Out_of_memory exception *)
let entries_match op1 op2 = match op1, op2 with
  | Q_entry a, Q_entry b when a == b -> true
  | _ -> false


(* ------------------------------------------------------------------------ *)
(*                     MAIN FUNCTIONS FOR OPTIMIZATIONS                     *)
(* ------------------------------------------------------------------------ *)

	   
(* Function that performs reverse copy propagation *)
let reverse_copy_propagation i (quads : quad array) =
  if i > 0 then
    begin
      let q1 = quads.(i-1) in
      let q2 = quads.(i) in
      if (isValid q1) && (isValid q2) && (isBinop q1) && (isAssign q2) then
	if (entries_match q1.z q2.x) && (entries_match q2.z q1.x || entries_match q2.z q1.y) then
	  begin
	    q1.z <- q2.z;
	    q2.z <- Q_none;
	  end
    end (* imperative style -- skipping else parts *)

(* Function that reverses conditions. Transforms <relop, x, y, L>, <jump, -, -, L1> to
   <not_relop, x, y, L1> *)
let reverse_jconditions i (quads : quad array) =
  if i > 0 then
    begin
      let q1 = quads.(i-1) in
      let q2 = quads.(i) in
      if (isValid q1) && (isValid q2) && (isRelop q1) && (isDirectJump q2) then 
	if (getLabel q1) = (q1.label + 2) then
	  begin
	    q1.op <- rev_relop q1.op; (* reverse relop *)
	    q1.z  <- q2.z;            (* fix jump point *)
	    q2.z  <- Q_none;          (* invalidate quad *)
	  end 
    end

(* Hashtable thas constant values are stored for constant folding *)
let constants_bucket = Hashtbl.create 42
(* Function that performs constant folding *)
let constant_folding i (quads : quad array) =
  let quad = quads.(i) in
  let extract_hash_entry operand = match operand with
    | Q_entry e -> e
    | _ -> print_quad stdout quad; internal "Quad stores in something other than entry?!"; raise Terminate in
  let get_value vall = match vall with
    | Q_int i -> Some i
    | Q_entry e -> (try Some (Hashtbl.find constants_bucket e)
		   with Not_found -> None)
    | _ -> None
  in
  match quad.op with
  | Q_plus | Q_minus | Q_mult | Q_div | Q_mod ->
     if quad.z <> Q_none then 
       begin		  	   
	 let entry = extract_hash_entry quad.z in
	 let op = fun_of_operator quad.op in
	 let v1 = get_value quad.x in
	 let v2 = get_value quad.y in
	 match (v1,v2) with
	 | (Some v1, Some v2) ->
	    begin
	      let res = op v1 v2 in
	      Hashtbl.replace constants_bucket entry res;
	      match entry.entry_info with
	      | ENTRY_temporary _ -> quad.z <- Q_none
	      | ENTRY_variable  _
	      | ENTRY_parameter _ -> quads.(i) <- genQuad Q_assign (Q_int res) Q_empty (Q_entry entry)
	      | _ -> internal "Entry cannot be a function"; raise Terminate
	    end
	 | (Some v1, None) ->
	    Hashtbl.remove constants_bucket entry;
	    quad.x <- Q_int v1
	 | (None, Some v2) ->
	    Hashtbl.remove constants_bucket entry;
	    quad.y <- Q_int v2
	 | (None, None) ->
	    Hashtbl.remove constants_bucket entry
       end
  | Q_eq | Q_neq | Q_greater | Q_less | Q_geq | Q_leq ->
     if quad.z <> Q_none then 						 
       begin
	 let v1 = get_value quad.x in
	 let v2 = get_value quad.y in
	 match (v1,v2) with
	 | (Some v1, Some v2) ->
	    quads.(i) <- genQuad quad.op (Q_int v1) (Q_int v2) quad.z
	 | (Some v1, None) ->
	    quad.x <- Q_int v1
	 | (None, Some v2) ->
	    quad.y <- Q_int v2
	 | (None, None) ->
	    ()
       end
  | Q_assign ->
     if quad.z <> Q_none then 
       begin
	 let v = get_value quad.x in
	 match v with
	 | Some v ->
	    quad.x <- Q_int v
	 | None ->
	    ()
       end
  | Q_array ->
     if quad.z <> Q_none then
       begin
	 let v = get_value quad.y in
	 match v with
	 | Some v ->
	    quad.y <- Q_int v
	 | None ->
	    ()
       end
  | Q_par ->
     if quad.z <> Q_none then
       begin
	 let v = get_value quad.x in
         match v with
	 | Some v ->
	    quad.x <- Q_int v
	 | None ->
	    ()
       end
  | _ ->
     ()
	   
					      
  

(* Perform all optimizations, if optimizations are enabled by user *)
let optimize allowed quads =
  if allowed then
    for i = 0 to (Array.length quads - 1) do
      constant_folding i quads;
      reverse_jconditions i quads;
      reverse_copy_propagation i quads;
    done
  
     
       
     
  
