(* sml-keywords.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

structure SMLKeywords : sig

    val isKW : substring -> bool

  end = struct

  (* the keyword hash table *)
    exception Keyword
    val keywords : bool AtomTable.hash_table = AtomTable.mkTable(64, Keyword)

  (* insert the reserved words into the keyword hash table *)
    val _ = let
	  val insert = AtomTable.insert keywords
	  fun ins (s, item) = insert (Atom.atom s, item)
	  in
	    app ins [
	      (* SML reserved words; change range value to false to turn off
	       * embolding.
	       *)
		("*",		true),
		("|",		true),
		(":",		true),
		(":>",		true),
		("=",		true),
		("#",		true),
		("->",		true),
		("=>",		true),
		("abstype",	true),
		("and",		true),
		("andalso",	true),
		("as",		true),
		("case",	true),
		("datatype",	true),
		("do",		true),
		("else",	true),
		("end",		true),
		("eqtype",	true),
		("exception",	true),
		("fn",		true),
		("fun",		true),
		("functor",	true),
		("funsig",	true),
		("handle",	true),
		("if",		true),
		("in",		true),
		("include",	true),
		("infix",	true),
		("infixr",	true),
		("let",		true),
		("local",	true),
		("nonfix",	true),
		("of",		true),
		("op",		true),
		("open",	true),
		("orelse",	true),
		("raise",	true),
		("rec",		true),
		("sharing",	true),
		("sig",		true),
		("signature",	true),
		("struct",	true),
		("structure",	true),
		("then",	true),
		("type",	true),
		("val",		true),
		("where",	true),
		("while",	true),
		("with",	true),
		("withtype",	true)
	      ]
	  end

    val peek = AtomTable.find keywords

    fun isKW text = (case (peek(Atom.atom' text))
	   of (SOME b) => b
	    | _ => false
	  (* end case *))

  end (* SMLKeywords *)

