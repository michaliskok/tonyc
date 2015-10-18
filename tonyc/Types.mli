type typ = TYPE_none	      (* no type (should not be used)       *)
         | TYPE_int	      (* int                                *)
         | TYPE_char	      (* byte                               *)
         | TYPE_bool	      (* boolean                            *)
         | TYPE_array of      (* array                              *)
             typ *	      (*   element type                     *)
             int	      (*   size of array, if known, or zero *)
	 | TYPE_list of	      (* list                               *)
	     typ	      (*   element type                     *)
	 | TYPE_func          (* function                           *)

val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
val extractType : typ -> typ
val isPointerType : typ -> bool
val needSizeExtend : typ -> bool
