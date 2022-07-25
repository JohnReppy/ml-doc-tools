(* latex-id-family.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure LaTeXIdFamily : sig

  (* given a function for adding escape sequences to strings, add the
   * formatting commands for ID-families to the argument string.
   *)
    val fmt : (string -> string) -> string -> string

  (* strip the ID-family braces from an identifier *)
    val strip : string -> string

  end = struct

    fun fmt cvtFn id = let
	  val toks = IdFamily.map cvtFn (IdFamily.parse id)
	  in
	    IdFamily.fmt (fn s => concat["\\mldArg{", s, "}"]) toks
	  end

    fun strip id = IdFamily.fmt (fn s => s) (IdFamily.parse id)

  end
