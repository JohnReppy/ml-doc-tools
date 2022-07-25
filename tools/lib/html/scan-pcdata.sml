(* scan-pcdata.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Support for finding entities in a PCDATA string.
 *)

structure ScanPCData : sig

    datatype token = PCDATA of string | ENTITY of Atom.atom

    val tokenize : string -> token list
	(* splits a string into a list of tokens, where a token is
	 * either a maximal sequence of characters or a single
	 * entity.
	 *)

  end = struct

    datatype token = PCDATA of string | ENTITY of Atom.atom

    structure SS = Substring

    fun tokenize s = let
	  fun isn'tAmp #"&" = false | isn'tAmp _ = true
	  fun isn'tSemi #";" = false | isn'tSemi _ = true
	  fun trim1 ss = SS.triml 1 ss
	  fun scan (ss, l) = if (SS.isEmpty ss)
		then rev l
		else let
		  val (s1, s2) = SS.splitl isn'tAmp ss
		  val (s2, s3) = SS.splitl isn'tSemi (trim1 s2)
		  val s3 = trim1 s3
		  val l = if (SS.isEmpty s1) then l else PCDATA(SS.string s1)::l
		  in
		    if (SS.isEmpty s2)
		      then scan (s3, l)
		      else scan (s3, ENTITY(Atom.atom' s2)::l)
		  end
	  in
	    scan (SS.full s, [])
	  end

  end

