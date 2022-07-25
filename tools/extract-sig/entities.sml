(* latex-entities.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The translation for the standard ML-Doc entities to HTML.
 *)

structure EntitiesToSML = EntitiesFn (
    val entityMap = [
	  ("LT",	"<"),
	  ("LTE",	"<="),
	  ("GT",	">"),
	  ("GTE",	">="),
	  ("NEQ",	"<>"),
	  ("AMP",	"&"),
	  ("DQUOTE",	"\""),
	  ("GREATER",	">"),
	  ("GREATEREQ",	">="),
	  ("LESS",	"<"),
	  ("LESSEQ",	"<="),
	  ("NOTEQ",	"<>")
	]
  );
