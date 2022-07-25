(* id-family.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parse the contents of a module name picking out ID-family tags.
 *)

structure IdFamily : sig

    datatype id_token = DATA of string | INDEX of string

    val parse : string -> id_token list

    val map : (string -> string) -> id_token list -> id_token list

    val fmt : (string -> string) -> id_token list -> string
    val toString : id_token list -> string

  end = struct

    structure SS = Substring

    datatype id_token = DATA of string | INDEX of string

    fun parse "" = [DATA ""]
      | parse name = let
	  val ss = SS.full name
	  fun mkData ss = DATA(SS.string ss)
	  fun mkIndex ss = INDEX(SS.string ss)
	  fun scanId (ss, start, n, toks) = (case SS.getc ss
		 of NONE => if (n > 0) then mkData start :: toks else toks
		  | SOME(#"{", ss') => let
		      val toks =  if n > 0
			    then mkData(SS.slice(start, 0, SOME n))::toks
			    else toks
		      in
			scanIx (ss', ss', 0, toks)
		      end
		  | SOME(_, ss') => scanId(ss', start, n+1, toks)
		(* end case *))
	  and scanIx (ss, start, n, toks) = (case SS.getc ss
		 of NONE => raise Fail "unclosed index"
		  | SOME(#"}", ss') => let
		      val toks =  if n > 0
			    then mkIndex(SS.slice(start, 0, SOME n))::toks
			    else toks
		      in
			scanId (ss', ss', 0, toks)
		      end
		  | SOME(#"{", _) => raise Fail "nested index"
		  | SOME(_, ss') => scanIx(ss', start, n+1, toks)
		(* end case *))
	  in
	    List.rev(scanId (ss, ss, 0, []))
	  end

    fun map f toks = let
	  fun mapf ([], toks) = List.rev toks
	    | mapf (DATA s ::  r, toks) = mapf(r, DATA(f s)::toks)
	    | mapf (INDEX s ::  r, toks) = mapf(r, INDEX(f s)::toks)
	  in
	    mapf (toks, [])
	  end

    fun fmt fmtIndex toks = let
	  fun cvt (DATA s, l) = s :: l
	    | cvt (INDEX s, l ) = fmtIndex s :: l
	  in
	    concat(List.foldr cvt [] toks)
	  end

    val toString = fmt (fn s => concat["{", s, "}"])

  end
