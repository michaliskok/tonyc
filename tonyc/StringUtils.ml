exception Not_Hex_Digit of char

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
  16 * h1 + h2

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
