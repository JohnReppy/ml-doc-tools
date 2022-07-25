(* print-markup.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

functor PrintMarkup (M : MARKUP) : sig

    structure M : MARKUP

    val toString : M.markup -> string

    val print : {say : string -> unit, flush : unit -> unit}
	  -> M.markup list -> unit

  end = struct

    structure M = M
    structure E = M.Elem

    fun toString (M.ELEM{elem, body, ...}) = E.elemName elem
      | toString (M.DATA data) = if (size data > 20)
	  then concat ["PCDATA \"", substring(data, 0, 20), " ...\""]
	  else concat ["PCDATA \"", data, "\""]

    fun print {say, flush} l = let
	  fun prIndent 0 = ()
	    | prIndent 1 = say "  "
	    | prIndent 2 = say "    "
	    | prIndent 3 = say "      "
	    | prIndent n = (say "      "; prIndent(n-3))
	  fun prElem (indent, elem) = (
		prIndent indent; say(E.elemName elem); say "\n")
	  fun prData (indent, data) = (
		prIndent indent; say "PCDATA \"";
		if (size data > 20)
		  then (say(substring(data, 0, 20)); say " ...\n")
		  else (say data; say "\"\n"))
	  fun prMarkup indent (M.ELEM{elem, body, ...}) = (
		prElem(indent, elem);
		List.app (prMarkup(indent+1)) body)
	    | prMarkup indent (M.DATA data) = prData (indent, data)
	  in
	    List.app (prMarkup 0) l;
	    flush()
	  end

  end
