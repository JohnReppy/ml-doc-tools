(* code-style.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * The interface to a language specific translator from <CD> and <CODE>
 * element content to a list of stylized lines.
 *
 * The content model of <CD> and <CODE> is
 *
 *    <!ENTITY % MLREF.TOP	" SIGREF|FCTREF|FCTARGREF " >
 *    <!ENTITY % MLREF.SPEC	" STRREF|EXNREF|TYREF|CONREF|VALREF " >
 *    <!ENTITY % IDREF		" %MLREF.TOP | %MLREF.SPEC | IDREF " >
 *    <!ENTITY % CODE		"(#PCDATA | KW | ARG | %IDREF | INDEX)*" >
 *
 * NOTE: this API does not support multi-line comments or markup in comments!!!
 *)

signature CODE_STYLE =
  sig

    datatype style = COM | KW | SYMB | DELIM | ID | TYVAR | LIT

    datatype data
      = TEXT of (style * string)
      | ELEM of (MLDocElem.element * token list)
    withtype token = {sp : int, t : data}

    type line = token list

    val trimIndent : line list -> line list

    val toMarkup : CodeStyle.line list -> MLDocMarkup.markup list

  end

structure CodeStyle : CODE_STYLE =
  struct

    structure E = MLDocElem
    structure M = MLDocMarkup

    datatype style = COM | KW | SYMB | DELIM | ID | TYVAR | LIT

    datatype data
      = TEXT of (style * string)
      | ELEM of (MLDocElem.element * token list)
    withtype token = {sp : int, t : data}

    type line = token list

    fun trimIndent (lns : line list) = let
	  val minIndent =
		List.foldl
		  (fn ([], i) => i  | ({sp, t}::_, i) => Int.min(i, sp))
		    10000
		      lns
	  in
	    if (minIndent < 10000)
	      then let
		fun trim [] = []
		  | trim ({sp, t}::r) = {sp=sp-minIndent, t=t} :: r
		in
		  List.map trim lns
		end
	      else lns
	  end

    fun toMarkup lns = let
	  fun mkElem (elem, content) =
		M.ELEM{elem=elem, body=body, pos = ("", 0, 0)}
	  fun doToks ({sp, t=ELEM(elem, toks)}::r, ??, l) =
		val body = doToks (toks, ??, [])
		in
		  doToks (r, ??, mkElem(elem, body)::l)
		end
	    | doToks ({sp, t=TEXT(sty, txt)}::r, ??, l) =
	    | doToks ([], ??, l) = rev l
	  in
	  end

  end
