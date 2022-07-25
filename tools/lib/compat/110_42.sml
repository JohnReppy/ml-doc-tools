(* 110_42.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TextIO =
  struct

    open TextIO

    fun inputLine inStrm = (case TextIO.inputLine inStrm
	   of "" => NONE
	    | s => SOME s
	  (* end case *))

  end;
