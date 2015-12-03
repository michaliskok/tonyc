open Quads
open Types
open Symbol
open Error
open Identifier

(* Convert a (boolean) expression to a condition *)
let exprToCond expr =
  let true_ref = ref (nextQuad ()) in
  let true_quad = genQuad Q_ifb expr.place Q_empty (Q_label true_ref) in
  let false_ref = ref (nextQuad ()) in
  let false_quad = genQuad Q_jump Q_empty Q_empty (Q_label false_ref) in
  expr.true_l <- [true_ref];
  expr.false_l <- [false_ref];
  expr.code <- [false_quad; true_quad] @ expr.code

(* Convert a condition to a (boolean) expression *)
let condToExpr cond =
  let w = newTemporary TYPE_bool in
  backpatch cond.true_l (nextQuad ());
  let true_quad = genQuad Q_assign (Q_bool true) Q_empty (Q_entry w) in
  let q = ref (nextQuad () + 2) in
  let jump_quad = genQuad Q_jump Q_empty Q_empty (Q_label q) in
  backpatch cond.false_l (nextQuad ());
  let false_quad = genQuad Q_assign (Q_bool false) Q_empty (Q_entry w) in
  cond.place <- Q_entry w;
  cond.true_l <- [];
  cond.false_l <- [];
  cond.code <- [false_quad; jump_quad; true_quad] @ cond.code
  

       (**************************************************************)
       (*      Functions to handle Intermediate Code generation      *)
       (* ---------------------------------------------------------- *)
       (* NOTE : Lists containing intermediate code must be reversed *)
       (**************************************************************)

(* Intermediate code for unary expressions *)
let icode_unary_exp op exp = 
  if op = "+" then
    exp
  else 
    let w = newTemporary TYPE_int in
    let new_quad = genQuad Q_minus exp.place Q_empty (Q_entry w) in
    exp.code <- new_quad :: exp.code;
    exp.place <- Q_entry w;
    exp
      
	 
(* Intermediate code for binary expressions *)
let icode_binary_exp op e1 e2 =
  let op = operator_of_str op in
  let w = newTemporary TYPE_int in
  let new_quad = genQuad op e1.place e2.place (Q_entry w) in
  e2.code <- new_quad :: (e2.code @ e1.code);
  e2.place <- Q_entry w;
  e2


(* Intermediate code for boolean NOT operation *)
let icode_not_exp expr =
  (* Check if (boolean) expression needs conversion to condition *)
  if expr.true_l = [] && expr.false_l = [] then
    exprToCond expr;
  let temp_l = expr.true_l in
  expr.true_l <- expr.false_l;
  expr.false_l <- temp_l;
  expr
  
(* Intermediate code for relative operations *)
(* Backpatching is done by using references for better performance. 
   When true_l or false_l is backpatched, quad is also backpatched (because of the references!) *)
let icode_relop_exp op e1 e2 =
  (* Check if any of the two conditions needs conversion to expression *)
  if e1.true_l <> [] || e1.false_l <> [] then
    condToExpr e1;
  if e2.true_l <> [] || e2.false_l <> [] then
    condToExpr e2;
  let op = operator_of_str op in
  let true_ref = ref (nextQuad ()) in
  let quad_1 = genQuad op e1.place e2.place (Q_label true_ref) in
  let false_ref = ref (nextQuad ()) in
  let quad_2 = genQuad Q_jump Q_empty Q_empty (Q_label false_ref) in
  { default_properties with code = quad_2 :: quad_1 :: e2.code @ e1.code; typ = TYPE_bool; true_l = [true_ref]; false_l = [false_ref] }

  
(* Intermediate code for binary boolean expressions (and, or) *)
let icode_boolean_exp op e1 e2 =
  (* Check if any of the two expressions needs conversion to condition *)
  if e1.true_l = [] && e1.false_l = [] then
    exprToCond e1;
  if e2.true_l = [] && e2.false_l = [] then
    exprToCond e2;
  let e2first = List.hd (List.rev e2.code) in
  if op = "and" then  (* boolean AND operation *)
    begin
      backpatch e1.true_l (e2first.label);
      e2.code <- e2.code @ e1.code; e2.false_l <- e1.false_l @ e2.false_l;
      e2
    end
  else (* boolean OR operation *)
    begin
      backpatch e1.false_l (e2first.label);
      e1.code <- e2.code @ e1.code; e1.true_l <- e1.true_l @ e2.true_l;
      e1.false_l <- e2.false_l;
      e1
    end
      
(* Intermediate code for dynamic memory allocation (array creation) *)
let icode_new_array arr_t exp =
  let size = sizeOfType arr_t in
  let temp = newTemporary TYPE_int in
  let w = newTemporary TYPE_int in
  if isPointerType arr_t then (
    (* newarrayp is called, takes as parameter: size in WORDS *)
    let quad_1 = genQuad Q_assign exp.place Q_empty (Q_entry temp) in
    let quad_2 = genQuad Q_par (Q_entry temp) (Q_pass PASS_BY_VALUE) Q_empty in
    let quad_3 = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
    let quad_4 = genQuad Q_call Q_empty Q_empty Q_newp in
    let newp_code = [quad_4; quad_3; quad_2; quad_1] in
    exp.code <- newp_code @ exp.code;
    exp.typ <- TYPE_array (arr_t, size);
    exp.place <- Q_entry w;
    exp
  ) else (
    (* newarrayv is called, takes as parameter: size in BYTES *)
    let quad_1 = genQuad Q_mult exp.place (Q_int size) (Q_entry temp) in
    let quad_2 = genQuad Q_par (Q_entry temp) (Q_pass PASS_BY_VALUE) Q_empty in
    let quad_3 = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
    let quad_4 = genQuad Q_call Q_empty Q_empty Q_newv in
    let newv_code = [quad_4; quad_3; quad_2; quad_1] in
    exp.code <- newv_code @ exp.code;
    exp.typ <- TYPE_array (arr_t, size);
    exp.place <- Q_entry w;
    exp
  )
	     

(* Intermediate code for array element access *)
let icode_array_atom arr e =
  let w = newTemporary TYPE_int in
  let new_quad = genQuad Q_array arr.place e.place (Q_entry w) in
  (* Place is a dereferenced pointer *)
  e.code <- new_quad :: e.code @ arr.code;
  e.place <- Q_pointer (w, extractType arr.typ);
  e.typ <- extractType arr.typ;
  e

  
(* Intermediate code for list construction *)
let icode_list_cons elem list =
  let result = { default_properties with typ = TYPE_none } in (* in order to create a new record *)
  (* Checks if list's head needs extension from BYTE to WORD *)
  if needSizeExtend elem.typ then
    begin
      (* Extend head to word *)
      let y = newTemporary TYPE_int in
      let q1 = genQuad Q_par elem.place (Q_pass PASS_BY_VALUE) Q_empty in
      let q2 = genQuad Q_par (Q_entry y) (Q_pass PASS_RET) Q_empty in
      let q3 = genQuad Q_call Q_empty Q_empty Q_extend in
      (* pass extended var *)
      let q4 = genQuad Q_par (Q_entry y) (Q_pass PASS_BY_VALUE) Q_empty in
      result.code <- [q4; q3; q2; q1]
    end
  else
    begin
      let q = genQuad Q_par elem.place (Q_pass PASS_BY_VALUE) Q_empty in
      result.code <- [q]
    end;
  let quad_1 = genQuad Q_par list.place (Q_pass PASS_BY_VALUE) Q_empty in
  let w = newTemporary (TYPE_list elem.typ) in
  let quad_2 = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
  begin
    (* Make appropriate function call *)
    if isPointerType elem.typ then 
      (* Head element is a pointer, consp needs to be called *)
      let quad_3 = genQuad Q_call Q_empty Q_empty Q_consp in
      Final.add_extrn_call_table "_consp";
      result.code <- quad_3 :: quad_2 :: quad_1 :: result.code @ list.code @ elem.code;
      result.typ <- TYPE_list elem.typ;
      result.place <- Q_entry w
    else 
      (* Head element is a variable, consv needs to be called *)
      let quad_3 = (genQuad Q_call Q_empty Q_empty Q_consv) in
      Final.add_extrn_call_table "_consv";
      result.code <- quad_3 :: quad_2 :: quad_1 :: result.code @ list.code @ elem.code;
      result.typ <- TYPE_list elem.typ;
      result.place <- Q_entry w
  end;	     
  result

			   
(* Intermediate code for list's head extraction *)
let icode_list_head (list : properties_variable) =
  let w = newTemporary TYPE_int in
  match list.typ with
  | TYPE_list tp ->
     let quad_1 = genQuad Q_par list.place (Q_pass PASS_BY_VALUE) Q_empty in
     let quad_2 = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
     let quad_3 = genQuad Q_call Q_empty Q_empty Q_head in
     let head_code = [quad_3; quad_2; quad_1] in
     (* Head function returns WORD, check if shrink to BYTE is necessary *)
     if needSizeExtend tp then (
       let y = newTemporary tp in
       let q1 = genQuad Q_par (Q_entry w) (Q_pass PASS_BY_VALUE) Q_empty in
       let q2 = genQuad Q_par (Q_entry y) (Q_pass PASS_RET) Q_empty in
       let q3 = genQuad Q_call Q_empty Q_empty Q_shrink in
       let size_code = [q3; q2; q1] in
       { default_properties with code = size_code @ head_code @ list.code; place = Q_entry y; typ = tp }
     ) else 
       { default_properties with code = head_code @ list.code; place = Q_entry w; typ = tp }
  | _ ->
     internal "Semantic check failure! Head extraction from something that is not a list!";
     raise Terminate

(* Intermediate code for list's tail extraction *)
let icode_list_tail list =
  match list.typ with
  | TYPE_list tp ->
     let w = newTemporary list.typ in
     let quad_1 = genQuad Q_par list.place (Q_pass PASS_BY_VALUE) Q_empty in
     let quad_2 = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
     let quad_3 = genQuad Q_call Q_empty Q_empty Q_tail in
     let tail_code = [quad_3; quad_2; quad_1] in
     { default_properties with code = tail_code @ list.code; place = Q_entry w; typ = list.typ }
  | _ ->
     internal "Semantic check failure! Requested tail from something that is not a list!";
     raise Terminate
    
(* Intermediate code for nil? function *)
let icode_list_nilq list =
  match list.typ with
  | TYPE_list tp -> 
     let true_ref = ref (nextQuad ()) in
     let true_quad = genQuad Q_eq list.place Q_nil (Q_label true_ref) in
     let false_ref = ref (nextQuad ()) in
     let false_quad = genQuad Q_jump Q_empty Q_empty (Q_label false_ref) in
     { default_properties with code = false_quad :: true_quad :: list.code; typ = TYPE_bool; true_l = [true_ref]; false_l = [false_ref] }
  | _ ->
     internal "Semantic analysis failure! nil? function takes list as parameter!";
     raise Terminate

(* Intermediate code for assignments *)
let icode_assign_stmt atom expr =
  (* Check if condition needs conversion to expression *)
  if expr.true_l <> [] || expr.false_l <> [] then
    condToExpr expr;
  let new_quad = genQuad Q_assign expr.place Q_empty atom.place in
  { default_properties with code = new_quad :: expr.code @ atom.code }

(* Intermediate code for function calls
   This function generates intermediate code quads for parameters, 
   result (if any), and for the function call *)
let icode_fun_call (res: Types.typ) (ent : Symbol.entry)  (args : properties_variable list) =
  (* Function that takes the argument list and the par code generated by
     gen_par_code function as argument, and returns proper code for function call *) 
  let rec gen_arg_code acc = function
    | ([], []) -> acc
    | (h::t, hp::tp) -> gen_arg_code (hp::h.code @ acc) (t,tp)
    | _ -> internal "Semantic failure, check gen_par_code function"; raise Terminate
  in 
  (* Function that generates I-code ONLY for argument passing (par quads) *)
  let rec gen_par_code acc = function
    | ([], []) -> 
       List.rev acc
    | (fp1::fps, ar1::ars) ->
       begin
	 match fp1.entry_info with
	 | ENTRY_parameter parInfo ->
	    let pass_mode = parInfo.parameter_mode in
	    let new_quad = genQuad Q_par ar1.place (Q_pass pass_mode) Q_empty in
	    gen_par_code (new_quad::acc) (fps,ars)
	 | _ ->
	    internal "Semantic failure: wrong parameter entry type in function call!";
	    raise Terminate
       end						       
    | _ ->
       internal "Semantic failure: wrong number of arguments in function call!";
       raise Terminate
  in    
  match ent.entry_info with
  | ENTRY_function funInfo ->
     let par_quads = gen_par_code [] (funInfo.function_paramlist, args) in
     let arg_quads = gen_arg_code [] (args, par_quads) in
     if res <> TYPE_none then
       let w = newTemporary res in
       let res_quad = genQuad Q_par (Q_entry w) (Q_pass PASS_RET) Q_empty in
       let call_quad = genQuad Q_call Q_empty Q_empty (Q_entry ent) in
       { default_properties with code = call_quad :: res_quad :: arg_quads; place = Q_entry w; typ = res }
     else
       let call_quad = genQuad Q_call Q_empty Q_empty (Q_entry ent) in
       { default_properties with code = call_quad :: arg_quads; typ = TYPE_none }
  | _ ->
     internal "Semantic failure: function entry in function calls!";
     raise Terminate 

	   
(* Intermediate code for return stmt *)
let icode_return_stmt expr =
  let res_quad = genQuad Q_assign expr.place Q_empty (Q_result expr.typ) in
  let ret_quad = genQuad Q_ret Q_empty Q_empty Q_empty in
  expr.code <- ret_quad :: res_quad :: expr.code;
  expr


(* Intermediate code for statements *)
let icode_stmt s = 
  backpatch s.next (nextQuad ());
  s

  
(* Intermediate code for compound statements (i.e. stmt+) *)
let icode_stmt_list (stmt_list : properties_variable) (stmt : properties_variable) =
  if stmt.code <> [] then
    begin
      let stmt_first = List.hd (List.rev stmt.code) in
      backpatch stmt_list.next stmt_first.label;
    end;
  { default_properties with code = stmt.code @ stmt_list.code; next = stmt.next }


(* Intermediate code for if_head (head of IF statement) *)
let icode_if_head cond =
  (* Convert (boolean) expression to condition, if needed *)
  if cond.true_l = [] && cond.false_l = [] then
    exprToCond cond;
  backpatch cond.true_l (nextQuad ());
  cond

(* Intermediate code for else_head (head of ELSE statement) *)
let icode_else_head () =
  let next_ref = ref (nextQuad ()) in
  let jump_quad = genQuad Q_jump Q_empty Q_empty (Q_label next_ref) in
  { default_properties with code = [jump_quad]; next = [next_ref] }

(* Intermediate code for else_stmt (ELSE statement) *)
let icode_else_stmt head body =
  body.code <- body.code @ head.code;
  body.next <- body.next @ head.next;
  body

(* Intermediate code for ELSIF statement (used at elsif keyword) *)
let icode_elsif () =
  let next_ref = ref (nextQuad ()) in
  let jump_quad = genQuad Q_jump Q_empty Q_empty (Q_label next_ref) in
  { default_properties with code = [jump_quad]; next = [next_ref] }

(* Intermediate code for elsif_head (head of ELSIF statement *)
let icode_elsif_head elsif cond =
  (* Convert (boolean) expression to condition, if needed *)
  if cond.true_l = [] && cond.false_l = [] then
    exprToCond cond;
  (* Backpatch cond.true_l with the next quad *)
  backpatch cond.true_l (nextQuad ());
  cond.code <- cond.code @ elsif.code;
  cond.next <- elsif.next;
  cond

(* Intermediate code for elsif_simple (only one ELSIF statement) *)
let icode_elsif_simple head body =
  body.code <- body.code @ head.code;
  body.next <- body.next @ head.next;
  body.false_l <- head.false_l;
  body

(* Intermediate code for elsif_stmt (ELSIF statement with more than one parts) *)
let icode_elsif_stmt elsif_stmt head body =
  (* Backpatch elsif_stmt.false_l with the next condition *)
  let next_cond_first = List.nth (List.rev head.code) 1 in
  backpatch elsif_stmt.false_l next_cond_first.label;
  body.code <- body.code @ head.code @ elsif_stmt.code;
  body.next <- body.next @ head.next @ elsif_stmt.next;
  body.false_l <- head.false_l;
  body
    
(* Intermediate code for IF statement (optional ELSIF argument) *)
let icode_if_stmt ?elsif if_head body else_stmt = match elsif with
  | None ->
     (* If ELSE body is not empty backpatch if_head.false_l with ELSE body *)
     if List.length else_stmt.code > 1 then
       (* First element of ELSE body is the 2nd in the list 
          (because of the jump at the start) *)
       let else_first = List.nth (List.rev else_stmt.code) 1 in
       backpatch if_head.false_l else_first.label
     else
       backpatch if_head.false_l (nextQuad ());
     { default_properties with code = else_stmt.code @ body.code @ if_head.code; next = else_stmt.next }
  | Some elsif ->
     (* Backpatch if_head.false_l with ELSIF condition *)
     let elsif_first = List.nth (List.rev elsif.code) 1 in
     backpatch if_head.false_l elsif_first.label;
     (* Backpatch elsif.false_l with ELSE body, if it is not empty *)
     if List.length else_stmt.code > 1 then
       (* First element of ELSE body is the 2nd in the list 
          (because of the jump at the start) *)
       let else_first = List.nth (List.rev else_stmt.code) 1 in
       backpatch elsif.false_l else_first.label
     else
       backpatch elsif.false_l (nextQuad ());
     { default_properties with code = else_stmt.code @ elsif.code @ body.code @ if_head.code; next = else_stmt.next @ elsif.next }
       
(* Intermediate code for for_head (head of a FOR statement) *)
let icode_for_head init cond incr =
  (* Convert (boolean) expression to condition, if needed *)
  if cond.true_l = [] && cond.false_l = [] then
    exprToCond cond;
  (* Add a jump from the loop increments to loop condition *)
  let cond_first = List.hd (List.rev cond.code) in
  let cond_jump = genQuad Q_jump Q_empty Q_empty (Q_label (ref cond_first.label)) in
  incr.code <- cond_jump :: incr.code;
  (init, cond, incr)
    
(* Intermediate code for FOR statements *)
let icode_for_stmt (init, cond, incr) body =
  let incr_first = List.hd (List.rev incr.code) in
  (* Add a jump quad from the loop body to loop increments *)
  let incr_jump = genQuad Q_jump Q_empty Q_empty (Q_label (ref incr_first.label)) in
  body.code <- incr_jump :: body.code;
  (* Backpatch condition's true_l with the body's first quad *)
  let body_first = List.hd (List.rev body.code) in
  backpatch cond.true_l body_first.label;
  (* Backpatch body's next with the increment's quad *) 
  backpatch body.next incr_first.label;
  { default_properties with code = body.code @ incr.code @ cond.code @ init.code; next = cond.false_l }
    
