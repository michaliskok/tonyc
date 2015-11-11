open Format
open Error

exception Not_Hex_Digit of char

(* Checks if a character represents a hex value *)
let is_hex ch = (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'Z')

(* Returns the hexadecimal value of a character *)
let hex_value ch = match ch with
  | '0'..'9' -> Char.code ch - Char.code '0'
  | 'a'..'f' -> Char.code ch - Char.code 'a' + 10
  | 'A'..'F' -> Char.code ch - Char.code 'A' + 10
  | _ as c -> raise (Not_Hex_Digit c)

(* Returns the ASCII code of a hex char of the form: '\xhh' *)
let parse_hex str =
  let h1 = hex_value str.[2] in
  let h2 = hex_value str.[3] in
  Char.chr (16 * h1 + h2)

(* Creates a char list from a given string *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Creates a string from a given char list *)
let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

(* Returns a string without the escape sequences *)
let de_escape str pos =
  let len = String.length str in
  let rec loop i acc =
    if i < len then
      begin
	match str.[i] with
	| '\\' ->
	   begin
	     match str.[i+1] with
	      |'n' -> loop (i+2) ('\n'::acc)
              |'r' -> loop (i+2) ('\r'::acc)
              |'t' -> loop (i+2) ('\t'::acc)
              |'0' -> loop (i+2) ((Char.chr 0)::acc)
              |'\'' -> loop (i+2) ('\''::acc)
              |'"' -> loop (i+2) ('"'::acc)
              |'x' ->
		if ((is_hex str.[i+2]) && (is_hex str.[i+3])) then 
		  let h1 = hex_value str.[i+2] in
		  let h2 = hex_value str.[i+3] in
		  loop (i+4) ((Char.chr (h1*16+h2))::acc)
		else
		  begin
		    print_position err_formatter pos;
		    error "Invalid escape sequence!"; 
		    raise Terminate 
		  end
	      | _ ->
		 print_position err_formatter pos;
		 error "Invalid escape sequence!";
		 raise Terminate
	   end
	| _ ->
	   loop (i+1) (str.[i]::acc)
      end
    else
      implode (List.rev acc)
  in
  loop 0 []
