(* sml-token-scanner.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

structure SMLTokenScanner : TOKEN_SCANNER =
  struct

    structure SS = Substring
    structure M = MatchTree
    structure RS = RegExpSyntax
    structure E = MLDocElem

    structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = DfaEngine);

    type state = {
	comLevel : int,
	data : SS.substring
      }

    val initial = {comLevel = 0, data = SS.full ""}

    fun addString ({comLevel, data}, s) =
	  if SS.isEmpty data
	    then {comLevel=comLevel, data = SS.full s}
	    else raise Fail "addString to non-empty state"

    fun getc {comLevel, data} = (case SS.getc data
	   of NONE => NONE
	    | SOME(c, ss) => SOME(c, {comLevel=comLevel, data=ss})
	  (* end case *))

    fun prefix (len, pos : state) = SS.slice(#data pos, 0, SOME len)
    fun trim (len, {comLevel, data}) = {comLevel=comLevel, data=SS.triml len data}

    val comElem : E.element option = SOME E.IT

    fun startCom (M.Match({pos as {comLevel, data}, len}, _)) =
	  (comElem, prefix(len, pos), {comLevel=comLevel+1, data=SS.triml len data})

    fun endCom (M.Match({pos as {comLevel, data}, len}, _)) =
	  (comElem, prefix(len, pos), {comLevel=comLevel-1, data=SS.triml len data})

    fun ident (M.Match({pos, len}, _)) = let
	  val txt = prefix(len, pos)
	  val kind = if SMLKeywords.isKW txt then SOME E.KW else NONE
	  in
	    (kind, txt, trim(len, pos))
	  end

    fun mkToken (kind : E.element option) (M.Match({pos, len}, _)) =
	  (kind, prefix(len, pos), trim(len, pos))

    val tyvar = mkToken NONE
    val constant = mkToken NONE
    val delim = mkToken NONE
    val comment = mkToken comElem
    val entity = mkToken NONE

  (* string gets called after we have seen either #" or " *)
    fun string (M.Match({pos, len}, _)) = let
	  fun getS (ss, n) = (case SS.getc ss
		 of NONE => raise Fail "unclosed string"
		  | SOME(#"\\", ss') => getS'(ss', n+1)
		  | SOME(#"\"", ss') =>
		      (NONE, prefix(n+1, pos), {comLevel=0, data=ss'})
		  | SOME(_, ss') => getS(ss', n+1)
		(* end case *))
	  and getS' (ss, n) = (case SS.getc ss
		 of NONE => raise Fail "unclosed string"
		  | SOME(_, ss') => getS(ss', n+1)
		(* end case *))
	  in
	    getS (SS.triml len (#data pos), len)
	  end

    fun match tbl = RE.match tbl getc

    val matchTok = match [
	    ("\\(\\*",				startCom),
	    ("\\\\\\|@[A-Z]+\\\\\\|",		entity),
	    ("[a-zA-Z][a-zA-Z0-9'_]*",		ident),
	    ("[!%&$+/:<=>?@~|#*`^\\\\-]+",	ident),
	    ("'[a-zA-Z][a-zA-Z0-9'_]*",		tyvar),
	    ("~?[0-9]+",			constant),
	    ("~?0x[0-9a-fA-F]+",		constant),
	    ("~?[0-9]+(\\.[0-9]+(E~?[0-9]+)?|(\\.[0-9]+)?E~?[0-9]+)",
						constant),
	    ("_",				delim),
	    (",",				delim),
	    ("{",				delim),
	    ("}",				delim),
	    ("\\[",				delim),
	    ("\\]",				delim),
	    (";",				delim),
	    ("\\(",				delim),
	    ("\\)",				delim),
	    ("\\.",				delim),
	    ("\\.\\.\\.",			delim),
	    ("#\"",				string),
	    ("\"",				string)
	  ]

    val matchTokInCom = match [
	    ("\\*\\)",				endCom),
	    ("\\*",				comment),
	    ("[^*]+",				comment)
	  ]

    fun scan (st as {comLevel = 0, data}) = (case (matchTok st)
	   of NONE => (
print(concat["scan: comLevel = 0, data = \"", String.toString(SS.string data), "\"\n"]);
NONE
)
	    | (SOME(res, _)) => SOME res
	  (* end case *))
      | scan (st as {comLevel, data}) = (case (matchTokInCom st)
	   of NONE => (
print(concat["scan: comLevel = ", Int.toString comLevel,
", data = \"", String.toString(SS.string data), "\"\n"]);
NONE
)
	    | (SOME(res, _)) => SOME res
	  (* end case *))

  end

structure SMLScanner = CodeScannerFn (SMLTokenScanner);
