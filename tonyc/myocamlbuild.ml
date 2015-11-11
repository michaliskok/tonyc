(* Open the ocamlbuild world... *)
open Ocamlbuild_plugin

(* We work with commands so often... *)
open Command
       
(* This dispatch call allows to control the execution order of your
   directives. *)
let () =
  dispatch begin function
    (* Add our rules after the standard ones. *)
  | After_rules ->
     flag ["ocaml"; "pp"] (A"camlp5o");
     (* Add pa_extend to the ocaml pre-processor when pa_extend set *)
     flag ["ocaml"; "pp"; "use_pa_extend"] (A"pa_extend.cmo");
     (* Add q_MLast to the ocaml pre-processor when q_MLast is set *)
     flag ["ocaml"; "pp"; "use_q_MLast"] (A"q_MLast.cmo");
     (* Add extend.cmo to the ocaml pre-processor when use_extend is set *)
     flag ["ocaml"; "pp"; "use_extend"] (A"./extend.cmo");
 
     (* Running ocamldep on ocaml code that is tagged with use_extend
        will require the cmo.  Note that you only need this
        declaration when the syntax extension is part of the sources
        to be compiled with ocamlbuild. *)
     dep ["ocaml"; "ocamldep"; "use_extend"] ["extend.cmo"];
  | _ -> ()
  end


