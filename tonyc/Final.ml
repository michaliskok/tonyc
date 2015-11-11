open Types
open Identifier
open Symbol
open Quads
open Error

let asm_file = ref stdout
let printasm formatter = Printf.fprintf !asm_file formatter

				       
(* ------------------------------------------------------------------------ *)
(*                              STRING HANDLING                             *)
(* ------------------------------------------------------------------------ *)

let str_id = ref 0
let str_list = ref []

(* List with all the string constants and their labels *)
let add_string s =
  incr str_id;
  let str = Printf.sprintf "@str_%d" !str_id in
  str_list := (str, s) :: !str_list;
  str

(* Prints in a buffer a string in proper assembly form *)
let print_str str =
  let len = String.length str in
  let tmp_buf = Buffer.create len in (* buffer for regular chars *)
  let str_buf = Buffer.create len in (* buffer for the whole string *)
  let concat_bufs () =
    if (Buffer.length tmp_buf) <> 0 then
	Printf.bprintf str_buf "\t\tdb\t'%s'\n" (Buffer.contents tmp_buf);
    Buffer.clear tmp_buf in
  let print_str_char c = match c with
    | '\n'   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t10\n" 
    | '\r'   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t13\n" 
    | '\t'   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t09\n" 
    | '\000' -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t0\n" 
    | '\''   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t39\n"
    | '\"'   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t34\n" 
    | '\\'   -> concat_bufs (); Buffer.add_string str_buf "\t\tdb\t92\n"
    | _ as c -> Buffer.add_char tmp_buf c 
  in
  String.iter print_str_char str;
  concat_bufs ();
  Buffer.add_string str_buf "\t\tdb\t0\n";
  Buffer.contents str_buf

(* Prints all the strings in str_list with their labels*)
let print_strings () =
  printasm "\n\t; String constants\n";
  List.iter (fun (hd, msg) -> printasm "%s%s" hd (print_str msg)) (List.rev !str_list)

    
(* ------------------------------------------------------------------------ *)
(*                           AUXILIARY FUNCTIONS                            *)
(* ------------------------------------------------------------------------ *)	       

type entry_category = BY_VAL_LOCAL | BY_REF
				       
let current_nesting_lvl = ref 0
				       
let entry_scope ent = ent.entry_scope.sco_nesting
let is_local_entry ent = ((entry_scope ent) = !current_nesting_lvl)

let entry_offset ent = match ent.entry_info with
  | ENTRY_variable  v -> v.variable_offset
  | ENTRY_parameter p -> p.parameter_offset
  | ENTRY_temporary t -> t.temporary_offset
  | _ -> internal "Request for offset for something that is not a variable, a parameter or a temporary?"; raise Terminate

let entry_size ent =
  let aux t = match t with
    | TYPE_int -> "word"
    | TYPE_bool | TYPE_char -> "byte"
    | TYPE_list _ | TYPE_array _ -> "word"
    | _ -> internal "Invalid type for size"; raise Terminate
  in
  match ent.entry_info with
  | ENTRY_variable  v -> aux v.variable_type
  | ENTRY_parameter p -> aux p.parameter_type
  | ENTRY_temporary t -> aux t.temporary_type
  | _ -> internal "Request for size for something that is not a variable, a parameter or a temporary?"; raise Terminate

let categorize_entry ent = match ent.entry_info with
  | ENTRY_temporary _ | ENTRY_variable _ -> BY_VAL_LOCAL
  | ENTRY_parameter p ->
     begin
       match p.parameter_mode with
       | PASS_BY_VALUE -> BY_VAL_LOCAL
       | PASS_BY_REFERENCE -> BY_REF
       | _ -> internal "This rule should not be matched"; raise Terminate
     end
  | _ -> 
    internal "Cannot categorize non-variable, non-parameter, non-temporary entity."; raise Terminate
													      
													      
let get_function_uid ent = match ent.entry_info with
  | ENTRY_function f -> f.function_uid
  | _ -> internal "Cannot get function ID from something that is not a function!"; raise Terminate

let is_extern_function ent = (get_function_uid ent = 0)

   
(* ------------------------------------------------------------------------ *)
(*                     FINAL CODE AUXILIARY FUNCTIONS                       *)
(* ------------------------------------------------------------------------ *)
												      
let getAR a =
  let n_a = match a with
    | Q_entry e -> entry_scope e
    | _ -> internal "Cannot find nesting level of something that is not a Symbol Entry";
	   raise Terminate in
  let n = !current_nesting_lvl -n_a - 1 in
  printasm "\t\tmov\tsi, word ptr [bp+4]\n";
  for i = 1 to n do
    printasm "\t\tmov\tsi, word ptr [si+4]\n"
  done

let updateAL a = match a with
  | Q_entry x ->
     let n_p = !current_nesting_lvl in
     let n_x = if is_extern_function x then Pervasives.max_int else entry_scope x + 1 in
     if n_p < n_x then
       printasm "\t\tpush\tbp\n"
     else if n_p = n_x then
       printasm "\t\tpush\tword ptr [bp+4]\n"
     else
       begin
	 let n = n_p - n_x -1 in
	 printasm "\t\tmov\tsi, word ptr [bp+4]\n";
	 for i = 1 to n do
	   printasm "\t\tmov\tsi, word ptr [si+4]\n"
	 done;
	 printasm "\t\tpush\tword ptr [si+4]\n"
       end
  | _ ->
     internal "Cannot update Access Link of something that is not a Symbol Entry!";
     raise Terminate

let rec load r a = match a with
  | Q_int i          -> printasm "\t\tmov\t%s, %d\n" r i
  | Q_bool b         -> if b then printasm "\t\tmov\t%s, %d\n" r 1
			else printasm "\t\tmov\t%s, %d\n" r 0
  | Q_char c         -> printasm "\t\tmov\t%s, %d\n" r (Char.code c.[0])
  | Q_string (_,s)   -> printasm "\t\tlea\t%s, byte ptr %s\n" r (add_string s)
  | Q_nil            -> printasm "\t\tmov\t%s, byte ptr %d\n" r 0
  | Q_pointer (e, t) -> load "di" (Q_entry e);
			if (sizeOfType t = 2) then printasm "\t\tmov\t%s, word ptr [di]\n" r
			else printasm "\t\tmov\t%s, byte ptr [di]\n" r
  | Q_entry e        ->
     if is_local_entry e then
       begin
	 if categorize_entry e = BY_VAL_LOCAL then
	   if entry_offset e < 0 then
	     printasm "\t\tmov\t%s, %s ptr [bp%d]\n" r (entry_size e) (entry_offset e)
	   else
	     printasm "\t\tmov\t%s, %s ptr [bp+%d]\n" r (entry_size e) (entry_offset e)
	 else
	   begin
	     if entry_offset e < 0 then
	       printasm "\t\tmov\tsi, %s ptr [bp%d]\n" (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\tsi, %s ptr [bp+%d]\n" (entry_size e) (entry_offset e);
	     printasm "\t\tmov\t%s, %s ptr [si]\n" r (entry_size e)
	   end
       end
     else (* not a local entity *)
       begin
	 if categorize_entry e = BY_VAL_LOCAL then
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tmov\t%s, %s ptr [si%d]\n" r (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\t%s, %s ptr [si+%d]\n" r (entry_size e) (entry_offset e)
	   end
	 else
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tmov\tsi, %s ptr [si%d]\n" (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\tsi, %s ptr [si%d]\n" (entry_size e) (entry_offset e);
	     printasm "\t\tmov\t%s, %s ptr [si]\n" r (entry_size e)
	   end
       end
  | _ -> internal "Problem with load asm function!";
	 raise Terminate

let loadAddr r a = match a with
  | Q_string (_, s)  -> printasm "\t\tlea\t%s, byte ptr %s\n" r (add_string s)
  | Q_pointer (e, _) -> load r (Q_entry e)
  | Q_nil            -> load r (Q_int 0)
  | Q_entry e        ->
     if is_local_entry e then
       begin
	 if (categorize_entry e = BY_VAL_LOCAL) then
	   if entry_offset e < 0 then
	     printasm "\t\tlea\t%s, %s ptr [bp%d]\n" r (entry_size e) (entry_offset e)
	   else
	     printasm "\t\tlea\t%s, %s ptr [bp+%d]\n" r (entry_size e) (entry_offset e)
	 else
	   if entry_offset e < 0 then
	     printasm "\t\tmov\t%s, word ptr [bp%d]\n" r(entry_offset e)
	   else
	     printasm "\t\tmov\t%s, word ptr [bp+%d]\n" r (entry_offset e)
       end
     else (* not a local entity *)
       begin
	 if (categorize_entry e = BY_VAL_LOCAL) then
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tlea\t%s, %s ptr [si%d]\n" r (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tlea\t%s, %s ptr [si+%d]\n" r (entry_size e) (entry_offset e)
	   end
	 else
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tmov\t%s, %s ptr [si%d]\n" r (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\t%s, %s ptr [si+%d]\n" r (entry_size e) (entry_offset e)
	   end
       end
  | _ -> internal "Problem with loadAddr asm function!";
	 raise Terminate

let store r a = match a with
  | Q_entry e ->
     if is_local_entry e then
       begin
	 if (categorize_entry e = BY_VAL_LOCAL) then
	   if entry_offset e < 0 then
	     printasm "\t\tmov\t%s ptr [bp%d], %s\n" (entry_size e) (entry_offset e) r
	   else
	     printasm "\t\tmov\t%s ptr [bp+%d], %s\n" (entry_size e) (entry_offset e) r
	 else
	   begin
	     if entry_offset e < 0 then
	       printasm "\t\tmov\tsi, %s ptr [bp%d]\n" (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\tsi, %s ptr [bp+%d]\n" (entry_size e) (entry_offset e);
	     printasm "\t\tmov\t%s ptr [si], %s\n" (entry_size e) r
	   end
       end
     else (* not a local entity *)
       begin
	 if (categorize_entry e = BY_VAL_LOCAL) then
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tmov\t%s ptr [si%d], %s\n" (entry_size e) (entry_offset e) r
	     else
	       printasm "\t\tmov\t%s ptr [si+%d], %s\n" (entry_size e) (entry_offset e) r
	   end
	 else
	   begin
	     getAR a;
	     if entry_offset e < 0 then
	       printasm "\t\tmov\tsi, %s ptr [si%d]\n" (entry_size e) (entry_offset e)
	     else
	       printasm "\t\tmov\tsi, %s ptr [si+%d]\n" (entry_size e) (entry_offset e);
	     printasm "\t\tmov\t%s ptr [si], %s\n" (entry_size e) r
	   end
       end
  | Q_pointer (e, t) ->
     load "di" (Q_entry e);
     if sizeOfType t = 2 then
       printasm "\t\tmov\tword ptr [di], %s\n" r
     else
       printasm "\t\tmov\tbyte ptr [di], %s\n" r
  | Q_result t ->
     printasm "\t\tmov\tsi, word ptr [bp+6]\n";
     if sizeOfType t = 1 then
       printasm"\t\tmov\tbyte ptr [si], al\n"
     else
       printasm "\t\tmov\tword ptr [si], ax\n"
  | _ -> internal "Problem with store asm function";
	 raise Terminate

let name p = match p with
  | Q_entry f ->
     Printf.sprintf "_%s_%d" (id_name f.entry_id) (get_function_uid f)
  | _ ->
     internal "Problem in 'name' asm function (name for a non-function entry?)";
     raise Terminate

let endof p = match p with
  | Q_entry f ->
     Printf.sprintf "@%s_%d" (id_name f.entry_id) (get_function_uid f)
  | _ ->
     internal "Problem in 'endof' asm function (end of a non-function entry?)";
     raise Terminate

let label n = Printf.sprintf "@%d" n
   

(* ------------------------------------------------------------------------ *)
(*                        EXTERNAL FUNCTION HANDLING                        *)
(* ------------------------------------------------------------------------ *)

let (extrn_functions : (string, string) Hashtbl.t) = Hashtbl.create 42
								    
let print_extrns () =
  printasm "\n\t; Run-time library functions \n";
  Hashtbl.iter (fun name typ -> printasm "\t\textrn\t_%s : %s\n" name typ) extrn_functions


(* ------------------------------------------------------------------------ *)
(*                     FUNCTION STACK AUXILIARY FUNCTIONS                   *)
(* ------------------------------------------------------------------------ *)

(* Function entry, negative offset and AR entries are stored in a Hashtbl *)
let func_stack = Hashtbl.create 42
let func_stack_push (name, negofs, entries) = Hashtbl.add func_stack name (negofs, entries)
let func_stack_pop name = Hashtbl.find func_stack name

(* Stack that holds information about current unit (entry_name * uid) *)
let current_unit_stack = Stack.create ()

(* ------------------------------------------------------------------------ *)
(*                   GARBAGE COLLECTION - AUXILIARY FUNCTIONS               *)
(* ------------------------------------------------------------------------ *)

(* Functions to register call tables for external functions *)
let extrn_call_tables = ref []
let add_extrn_call_table func = if not (List.mem func !extrn_call_tables) then
				  extrn_call_tables := func :: !extrn_call_tables
let register_extrn_call_tables print_fun = List.iter print_fun !extrn_call_tables

(* Function that registers all call tables *)
let register_call_tables () =
  let reg_call_table func =
    printasm "\t\tmov\tax, OFFSET %s_call_table\n" func;
    printasm "\t\tcall\tnear ptr _register_call_table\n"
  in
  printasm "\t;;  Register call tables\n";
  Hashtbl.iter (fun entry offset -> reg_call_table (name (Q_entry entry))) func_stack;
  register_extrn_call_tables reg_call_table

(* Functions that handle function calls from the current unit *)
let call_id = ref 0
let call_list = ref []

let push_call params =
  (* There is another call in this unit *)
  call_list := begin match !call_list with
	       | [] -> []
	       | (x, y, z) :: t -> let another_call = true in
				   (x, another_call, z) :: t
	       end;
  incr call_id;
  call_list := (!call_id, false, params) :: !call_list

let get_call_list () = List.rev !call_list
let reset_call_list () = call_id := 0; call_list := []
let get_next_call_id () = !call_id
		  
				    
(* ------------------------------------------------------------------------ *)
(*                             CODE GENERATION                              *)
(* ------------------------------------------------------------------------ *)

let header =
  "xseg\t\tsegment\tpublic 'code'\n\
   \t\tassume\tcs:xseg, ds:xseg, ss:xseg\n\
   \t\torg\t100h\n\n\
   main\t\tproc\tnear\n\
   \t;; initialize memory: 2/3 heap and 1/3 stack\n\
   \t\tmov\tcx, OFFSET DGROUP:_start_of_space\n\
   \t\tmov\tword ptr _space_from, cx\n\
   \t\tmov\tword ptr _next, cx\n\
   \t\tmov\tax, 0FFFEh\n\
   \t\tsub\tax, cx\n\
   \t\txor\tdx, dx\n\
   \t\tmov\tbx, 3\n\
   \t\tidiv\tbx\n\
   \t\tand\tax, 0FFFEh ; even number!\n\
   \t\tadd\tcx, ax\n\
   \t\tmov\tword ptr _limit_from, cx\n\
   \t\tmov\tword ptr _space_to, cx\n\
   \t\tadd\tcx, ax\n\
   \t\tmov\tword ptr _limit_to, cx\n"

let header_end name =
  printasm "\t;; call main\n\
   \t\tcall\tnear ptr %s\n\
   _ret_of_main:\n\
   \t;; exit with code 0\n\
   \t\tmov\tax, 4C00h\n\
   \t\tint\t21h\n\
	    main\t\tendp\n\n\n" name

let gc_footer = 
  "\n\t;;public gc vars\n\
   \t\tpublic\t_next\n\
   \t\tpublic\t_space_from\n\
   \t\tpublic\t_space_to\n\
   \t\tpublic\t_limit_from\n\
   \t\tpublic\t_limit_to\n\
   \t\tpublic\t_ret_of_main\n\
   \n_next\t\tdw\t?\n\
   _space_from\tdw\t?\n\
   _space_to\tdw\t?\n\
   _limit_from\tdw\t?\n\
   _limit_to\tdw\t?\n"

let footer =
  "\n\t\txseg\tends\n\
   \n_DATA_END\tsegment\tbyte public 'stack'\n\
    _start_of_space\tlabel\tbyte\n\
    _DATA_END\tends\n\
    \nDGROUP\t\tgroup\txseg, _DATA_END\n\
    \n\t\tend\tmain\n"

let code_generation quad =
  let (no, op, x, y, z) = (quad.label, quad.op, quad.x, quad.y, quad.z) in
  printasm "\t;; "; print_quad !asm_file quad;
  printasm "%s:\n" (label no);
  match op with
  | Q_assign ->
     if size_of_operand x = 1 then
       begin
	 load "al" x;
	 store "al" z
       end
     else
       begin
	 load "ax" x;
	 store "ax" z
       end
  | Q_array ->
     let size_of_array t = match t with
       | TYPE_array (t, _) -> sizeOfType t
       | _ -> internal "size_of_array called with non-array argument"; raise Terminate
     in
     load "ax" y;
     printasm "\t\tmov\tcx, %d\n" (size_of_operand ~sizeof:size_of_array x);
     printasm "\t\timul\tcx\n";
     load "cx" x;
     printasm "\t\tadd\tax, cx\n";
     store "ax" z
  | Q_plus ->
     load "ax" x;
     load "dx" y;
     printasm "\t\tadd\tax, dx\n";
     store "ax" z
  | Q_minus ->
     if y = Q_empty then (* unary minus *)
       begin
	 load "ax" x;
	 printasm "\t\tneg\tax\n";
	 store "ax" z
       end
     else
       begin
	load "ax" x;
	load "dx" y;
	printasm "\t\tsub\tax, dx\n";
	store "ax" z
       end
  | Q_mult ->
     load "ax" x;
     load "cx" y;
     printasm "\t\timul\tcx\n";
     store "ax" z
  | Q_div ->
     load "ax" x;
     printasm "\t\tcwd\n";
     load "cx" y;
     printasm "\t\tidiv\tcx\n";
     store "ax" z
  | Q_mod ->
     load "ax" x;
     printasm "\t\tcwd\n";
     load "cx" y;
     printasm "\t\tidiv\tcx\n";
     store "dx" z
  | Q_eq | Q_neq | Q_greater | Q_less | Q_leq | Q_geq ->
     load "ax" x;
     load "dx" y;
     printasm "\t\tcmp\tax, dx\n";
     let asm_op = match op with
       | Q_eq      -> "je"
       | Q_neq     -> "jne"
       | Q_greater -> "jg"
       | Q_less    -> "jl" 
       | Q_leq     -> "jle"
       | Q_geq     -> "jge"
       | _         -> internal "This cannot possibly happen."; raise Terminate in
     printasm "\t\t%s\t\t%s\n" asm_op (label (getLabel z))
  | Q_ifb ->
     load "al" x;
     printasm "\t\tor\tal, al\n";
     printasm "\t\tjnz\t%s\n" (label (getLabel z))
  | Q_unit ->
     begin
       match x with
       | Q_entry ent ->
	  current_nesting_lvl := entry_scope ent + 1;
	  Stack.push (ent, get_function_uid ent) current_unit_stack;
	  printasm "%s\t\tproc\tnear\n" (name x);
	  printasm "\t\tpush\tbp\n";
	  printasm "\t\tmov\tbp, sp\n";
	  reset_call_list ();
	  (* Do we have to initialize stack to 0 *)
	  
	  begin
	    try
	      let (negofs, _) = func_stack_pop ent in
	      printasm "\t\tsub\t\tsp, %d\n" (-negofs)
	    with Not_found ->
	      Printf.printf "Failed to find entry %s\n" (id_name ent.entry_id);
	      Hashtbl.iter (fun x y -> Printf.printf "%s" (id_name x.entry_id)) func_stack
	  end
       | _ -> internal "Never matched -- Unit quad without the respective Symbol.entry";
	      raise Terminate
     end
  | Q_endu ->
     begin
       match x with
       | Q_entry ent ->
	  printasm "%s:\n\t\tmov\tsp, bp\n" (endof x);
	  printasm "\t\tpop\tbp\n";
	  printasm "\t\tret\n";
	  printasm "%s\tendp\n\n" (name x);
	  (* Print Function Call Table *)
	  let print_call (num, hasNext, addSP) =
	    let (active_function, active_function_id) = Stack.top current_unit_stack in
	    let (negofs, entries) = func_stack_pop ent in
	    let is_pointer e = match e.entry_info with
	      | ENTRY_variable  e -> isPointerType e.variable_type
	      | ENTRY_parameter e -> isPointerType e.parameter_type
	      | ENTRY_temporary e -> isPointerType e.temporary_type
	      | _ -> false in
	    let print_heap_pointer e = if is_pointer e then printasm "\t\tdw\t %d\n" (entry_offset e) in
	    printasm "@call_%d_%d\tdw\t @%s_%d_call_%d\n" active_function_id num (id_name active_function.entry_id) active_function_id num;
	    if hasNext then printasm "\t\tdw\t @call_%d_%d\n" active_function_id (num+1)
	    else printasm "\t\tdw\t 0\n";
	    printasm "\t\tdw\t %d\n" (addSP - negofs + 4);
	    List.iter print_heap_pointer (List.rev entries);
	    printasm "\t\tdw\t 0\n";
	  in
	  printasm "%s_call_table:\n" (name x);
	  List.iter print_call (get_call_list ());
	  ignore (Stack.pop current_unit_stack)
       | _ -> internal "Never matched -- Endu quad without the respective Symbol.entry";
	      raise Terminate
     end
  | Q_par ->
     begin
       match y with
       | Q_pass p ->
	  if p = PASS_BY_VALUE then
	    if size_of_operand x <> 1 then
	      begin
		load "ax" x;
		printasm "\t\tpush\tax\n"
	      end
	    else
	      begin
		load "al" x;
		printasm "\t\tmov\tsi, sp\n";
		printasm "\t\tmov\tbyte ptr [si-1], 0\n";
		printasm "\t\tmov\tbyte ptr [si-2], al\n";
		printasm "\t\tsub\tsp, 2\n"
	      end
	  else (* p = PASS_BY_REFERENCE | PASS_RET *)
	    begin
	      loadAddr "si" x;
	      printasm "\t\tpush\tsi\n"
	    end
       | _ -> internal "Never matched -- Q_pass operand without valid pass mode";
	      raise Terminate
     end
  | Q_ret ->
     let (unit, _) = Stack.top current_unit_stack in
     printasm "\t\tjmp\t%s\n" (endof (Q_entry unit))
  | Q_jump ->
     printasm "\t\tjmp\t%s\n" (label (getLabel z))
  | Q_call ->
     begin
       let size_of_parameter p =
	 match p.parameter_mode with
	 | PASS_BY_VALUE     -> sizeOfType p.parameter_type
	 | PASS_BY_REFERENCE -> 2
	 | _ -> internal "Pattern matching went wrong -- PASS_RET"; raise Terminate
       in
       let size_of_params f = match f.function_paramlist with
	 | [] -> 0
	 | h :: t ->
	    begin
	      match h.entry_info with
	      | ENTRY_parameter p -> p.parameter_offset - 8 + size_of_parameter p
	      | _ -> internal "Never matched -- paramlist without parameters";
		     raise Terminate
	    end
       in
       match z with
       | Q_entry e -> (* User defined function || standard library function *)
	  begin
	    match e.entry_info with
	    | ENTRY_function f ->
	       if f.function_result = TYPE_none then
		 printasm "\t\tsub\tsp, 2\n";
	       updateAL z;
	       if f.function_uid < 0 then (* standard library function *)
		 begin
		   printasm "\t\tcall\tnear ptr _%s\n" (id_name e.entry_id);
		   (* Add function call to call_list -- for GC *)
		   push_call (4 + size_of_params f);
		   let (active_function, active_function_id) = Stack.top current_unit_stack in
		   printasm "@%s_%d_call_%d:\n" (id_name active_function.entry_id) active_function_id (get_next_call_id ());
		   try
		     ignore (Hashtbl.find extrn_functions (id_name e.entry_id))
		   with
		     Not_found -> Hashtbl.add extrn_functions (id_name e.entry_id) "proc"
		 end
	       else
		 begin
		   printasm "\t\tcall\tnear ptr %s\n" (name z);
		   (* Add function to call_list -- for GC *)
		   push_call (4 + size_of_params f);
		   let (active_function, active_function_id) = Stack.top current_unit_stack in
		   printasm "@%s_%d_call_%d:\n" (id_name active_function.entry_id) active_function_id (get_next_call_id ())
		 end;
	       printasm "\t\tadd\tsp, %d\n" (4 + size_of_params f)
	    | _ -> internal "Never matched -- Call quad without the respective entry.";
		   raise Terminate
	  end
       | Q_head | Q_tail ->
          let name = str_of_operand z in
	  printasm "\t\tpush\tbp\n";
	  printasm "\t\tcall\tnear ptr _%s\n" name;
	  (* Add function call to call_list -- for GC *)
	  push_call (4 + 2);
	  let (active_function, active_function_id) = Stack.top current_unit_stack in
	  printasm "@%s_%d_call_%d:\n" (id_name active_function.entry_id) active_function_id (get_next_call_id ());
	  begin
	    try
	      ignore (Hashtbl.find extrn_functions name)
	    with 
	      Not_found -> Hashtbl.add extrn_functions name "proc"
	  end;
	  printasm "\t\tadd\tsp, %d\n" (4 + 2)
       | Q_consv | Q_consp ->
          let name = str_of_operand z in
	  printasm "\t\tpush\tbp\n";
	  printasm "\t\tcall\tnear ptr _%s\n" name;
	  (* Add function call to call_list -- for GC *)
	  push_call (4 + 2 + 2);
	  let (active_function, active_function_id) = Stack.top current_unit_stack in
	  printasm "@%s_%d_call_%d:\n" (id_name active_function.entry_id) active_function_id (get_next_call_id ());
	  begin
	    try
	      ignore (Hashtbl.find extrn_functions name)
	    with
	      Not_found -> Hashtbl.add extrn_functions name "proc";
			   Hashtbl.add extrn_functions (name ^ "_call_table") "word"
	  end;
	  printasm "\t\tadd\tsp, %d\n" (4 + 2 + 2)
       | Q_newv | Q_newp | Q_extend | Q_shrink ->
          let name = str_of_operand z in
	  printasm "\t\tpush\tbp\n";
	  printasm "\t\tcall\tnear ptr _%s\n" name;
	  (* Add function call to call_list -- for GC *)
	  push_call (4 + 2);
	  let (active_function, active_function_id) = Stack.top current_unit_stack in
	  printasm "@%s_%d_call_%d:\n" (id_name active_function.entry_id) active_function_id (get_next_call_id ());
	  begin
	    try
	      ignore (Hashtbl.find extrn_functions name)
	    with
	      Not_found ->
	      Hashtbl.add extrn_functions name "proc";
	      Hashtbl.add extrn_functions (name ^ "_call_table") "word"
	  end;
	  printasm "\t\tadd\tsp, %d\n" (4 + 2 + 2)
       | _ ->
	  internal "Never matched -- Call quad with wrong operands.";
	  raise Terminate
     end

let code_gen quad = if quad.z <> Q_none then code_generation quad
       
(* Final code generation *)
let print_final out_channel (quads : quad array) =
  asm_file := out_channel;
  let len = Array.length quads in
  let main_quad = quads.(len-1) in
  let main = name main_quad.x in
  (* Print asm headers *)
  printasm "%s" header;
  register_call_tables ();
  header_end main;
  (* Print main program*)
  Hashtbl.add extrn_functions "register_call_table" "proc";
  Array.iter code_gen quads;
  (* Print asm footers *)
  print_extrns ();
  print_strings ();
  printasm "%s" gc_footer;
  printasm "%s" footer
