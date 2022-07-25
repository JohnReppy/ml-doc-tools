(* sgmls-scan.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A scanner for the output of the SGMLS tool.
 *)

structure ScanSGMLS : SCAN_SGMLS =
  struct

    structure SS = Substring

    type name = Atom.atom

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
	(* "A<name> <value>" an attribute definition *)
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
	(* "s<pubid>" a public ID *)
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

    local
      fun fmt (kind, s) = concat[kind, " \"", s, "\""]
      fun fmtId (kind, id) = concat[kind, " ", Atom.toString id]
    in
    fun lineToString (GI_START id) = fmtId ("GI_START", id)
      | lineToString (GI_END id) = fmtId ("GI_END", id)
      | lineToString (DATA _) = "DATA"
      | lineToString (ENTITY_REF id) = fmtId ("ENTITY_REF", id)
      | lineToString (ATTR(id, _)) = fmtId ("ATTR", id)
      | lineToString (DATTR(id, _, _)) = fmtId ("DATTR", id)
      | lineToString (NOTATION id) = fmtId ("NOTATION", id)
      | lineToString (EXTERN(id, _, _)) = fmtId ("EXTERN", id)
      | lineToString (INTERN(id, _, _)) = fmtId ("INTERN", id)
      | lineToString (SUBDOC id) = fmtId ("SUBDOC", id)
      | lineToString (TEXT id) = fmtId ("TEXT", id)
      | lineToString (SYSID id) = fmt("SYSID", Atom.toString id)
      | lineToString (PUBID id) = fmt("PUBID", Atom.toString id)
      | lineToString (FILENAME id) = "FILENAME"
      | lineToString (SUBDOC_START id) = fmtId ("SUBDOC_START", id)
      | lineToString (SUBDOC_END id) =  fmtId ("SUBDOC_END", id)
      | lineToString (LINE _) = "LINE"
      | lineToString (CORRECT) = "CORRECT"
      | lineToString (OTHER s) = fmt("OTHER", s)
      | lineToString (EOF) = "EOF"
    end (* local *)

    val n_CDATA		= Atom.atom "CDATA"
    val n_SDATA		= Atom.atom "SDATA"
    val n_NDATA		= Atom.atom "NDATA"

  (* make a name from a substring; this should be part of the Atom structure
   * at some point!
   *)
    fun mkName ss = Atom.atom(SS.string ss)

    val dropWS = SS.dropl Char.isSpace

    fun scanName ss = let
	  val (name, rest) = SS.splitl (not o Char.isSpace) ss
	  in
	    if (SS.isEmpty name)
	      then raise Fail "scanName: empty name"
	      else (mkName name, dropWS rest)
	  end

    fun scanNames ss = let
	  fun scan (ss, l) = if (SS.isEmpty ss)
		then rev l
		else let
		  val (name, rest) = scanName ss
		  in
		    scan (rest, name::l)
		  end
	  in
	    scan (ss, [])
	  end

    local
      val tbl = let
	    val tbl = AtomTable.mkTable(8, Fail "scanAttrValue: strange attribute kind")
	    fun ins (s, f) = AtomTable.insert tbl (Atom.atom s, f)
	    in
	      List.app ins [
		  ("IMPLIED",	fn rest => IMPLIED_VAL),
		  ("CDATA",	fn rest => CDATA_VAL(SS.string rest)),
		  ("NOTATION",	fn rest => NOTATION_VAL(mkName(dropWS rest))),
		  ("ENTITY",	fn rest => ENTITY_VAL(scanNames rest)),
		  ("TOKEN",	fn rest => TOKEN_VAL(scanNames rest)),
		  ("ID",	fn rest => ID_VAL(mkName(dropWS rest)))
		];
	      tbl
	    end
    in
    fun scanAttrValue ss = let
	  val (valKind, rest) = scanName(dropWS ss)
	  in
	    (AtomTable.lookup tbl valKind) rest
	  end
    end (* local *)

    fun scanAttr ss = let
	  val (name, rest) = scanName ss
	  in
	    ATTR(name, scanAttrValue rest)
	  end

    fun scanDataAttr ss = let
	  val (ename, rest) = scanName ss
	  val (name, rest) = scanName(dropWS rest)
	  in
	    DATTR(ename, name, scanAttrValue rest)
	  end

    fun scanEntityType ss = let
	  val (ty, rest) = SS.splitl (not o Char.isSpace) ss
	  val tyName = mkName ty
	  in
	    if (Atom.sameAtom(tyName, n_CDATA))
	      then (CDATA, rest)
	    else if (Atom.sameAtom(tyName, n_SDATA))
	      then (SDATA, rest)
	    else if (Atom.sameAtom(tyName, n_NDATA))
	      then (NDATA, rest)
	      else raise Fail(String.concat[
		    "scanEntityType: bogus type (", SS.string ty, ")"
		  ])
	  end

    fun scanExtern ss = let
	  val (name, rest) = scanName ss
	  val (ty, rest) = scanEntityType rest
	  in
	    EXTERN(name, ty, #1(scanName rest))
	  end

    fun scanIntern ss = let
	  val (name, rest) = scanName ss
	  val (ty, rest) = scanEntityType rest
	  in
	    INTERN(name, ty, SS.string(dropWS rest))
	  end

    fun scanLineNum ss = let
	  val SOME(digits, rest) = Int.scan StringCvt.DEC SS.getc ss
	  val rest = dropWS rest
	  in
	    if (SS.isEmpty rest)
	      then LINE(digits, NONE)
	      else LINE(digits, SOME(SS.string rest))
	  end

    fun scanLine NONE = EOF
      | scanLine (SOME s) = let
	  val line = if (String.sub(s, String.size s - 1) = #"\n")
		then SS.substring(s, 0, String.size s - 1)
		else SS.full s
	  in
	    case SS.getc line
	     of (SOME(#"(", rest)) => GI_START(#1(scanName rest))
	      | (SOME(#")", rest)) => GI_END(#1(scanName rest))
	      | (SOME(#"-", rest)) => DATA rest
	      | (SOME(#"&", rest)) => ENTITY_REF(#1(scanName rest))
	      | (SOME(#"A", rest)) => scanAttr rest
	      | (SOME(#"D", rest)) => scanDataAttr rest
	      | (SOME(#"N", rest)) => NOTATION(#1(scanName rest))
	      | (SOME(#"E", rest)) => scanExtern rest
	      | (SOME(#"I", rest)) => scanIntern rest
	      | (SOME(#"S", rest)) => SUBDOC(#1(scanName rest))
	      | (SOME(#"T", rest)) => TEXT(#1(scanName rest))
	      | (SOME(#"s", rest)) => SYSID(#1(scanName rest))
	      | (SOME(#"p", rest)) => PUBID(#1(scanName rest))
	      | (SOME(#"f", rest)) => FILENAME(#1(scanName rest))
	      | (SOME(#"{", rest)) => SUBDOC_START(#1(scanName rest))
	      | (SOME(#"}", rest)) => SUBDOC_END(#1(scanName rest))
	      | (SOME(#"L", rest)) => scanLineNum rest
	      | (SOME(#"C", rest)) => CORRECT
	      | _ => OTHER(SS.string line)
	    (* end case *)
	  end

    datatype stream = STRM of stream_items ref
    and stream_items
      = REST of unit -> sgmls_line
      | S of {line : sgmls_line, rest : stream}

    fun mkStream inStrm = let
	  fun getLine () = scanLine(TextIO.inputLine inStrm)
	  in
	    STRM(ref(REST getLine))
	  end
    fun scanStrm (STRM strm) = (case !strm
	   of (S{line, rest}) => (line, rest)
	    | (REST f) => let
		val ln = f()
		val strm' = STRM(ref(REST f))
		in
		  strm := S{line=ln, rest = strm'};
		  (ln, strm')
		end
	  (* end case *))

  end;

