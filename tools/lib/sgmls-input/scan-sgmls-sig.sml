(* sgmls-scan-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A scanner for the output of the SGMLS tool.
 *)

signature SCAN_SGMLS =
  sig

    type name

    datatype entity_type = datatype SGMLS.entity_type
    datatype attr_val = datatype SGMLS.attr_val

    datatype sgmls_line
      = GI_START of name
	(* "(<GI>" plus any preceeding attributes *)
      | GI_END of name
	(* ")<GI>" *)
      | DATA of Substring.substring
	(* "-<data>" *)
      | ENTITY_REF of name
	(* "&<name>" (a reference to an external entity) *)
      | ATTR of (name * attr_val)
	(* "A<name> <value>" (an attribute definition) *)
      | DATTR of (name * name * attr_val)
	(* "D<ename> <name> <value>" (a data attribute for an external entity) *)
      | NOTATION of name
	(* "N<nname>" define a notation *)
      | EXTERN of (name * entity_type * name)
	(* "E<entity> <type> <nname>" (an external entity) *)
      | INTERN of (name * entity_type * string)
	(* "I<entity> <type> <data>" (an internal entity) *)
      | SUBDOC of name
	(* "S<ename>" a subdocument (ename is an entity) *)
      | TEXT of name
	(* "Tename" an external SGML text entity; only if -oentity option
	 * was specified.
	 *)
      | SYSID of name
	(* "s<sysid>" a system ID *)
      | PUBID of name
	(* "p<pubid>" a public ID *)
      | FILENAME of name
	(* "f<filename>" specifies a filename *)
      | SUBDOC_START of name
	(* "{<ename>" start of a subdocument (ename is an entity) *)
      | SUBDOC_END of name
	(* "}<ename>" end of a subdocument (ename is an entity) *)
      | LINE of (int * string option)
	(* "L<lineno> <file>" and "L<lineno>" *)
      | CORRECT
	(* the "C" output at the end, if the document is conforming *)
      | OTHER of string
	(* for things that are currently not handled *)
      | EOF
	(* end-of-file *)

    val lineToString : sgmls_line -> string

    val scanLine : string option -> sgmls_line

    type stream
    val mkStream : TextIO.instream -> stream
    val scanStrm : stream -> (sgmls_line * stream)

  end;
