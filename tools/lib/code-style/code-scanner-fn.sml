(* code-scanner-fn.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

functor CodeScannerFn (S : TOKEN_SCANNER) : sig

  (* Given a chunk of code, add KW elements to stylize it.  If the trim
   * flag is true, then leading indentation is trimed uniformly (i.e.,
   * the minimum indentation is removed from all lines.
   *)
    val stylize : {
	    code : MLDocMarkup.markup list,
	    trim : bool
	  } -> MLDocMarkup.markup list

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup
    structure SS = Substring

    val tabWid = 8
    fun expandTab i = (tabWid - (i mod tabWid))

  (* scan leading whitespace in the substring ss.  pos is the character
   * position where ss starts; we use it in computing how to expand tabs.
   *)
    fun scanWS (start, ss) = let
	  fun scan (pos, ss) = (case S.getc ss
		 of SOME(#" ", ss) => scan(pos+1, ss)
		  | SOME(#"\t", ss) => let
		      val wid = tabWid - (pos mod tabWid)
		      in
		        scan(pos+wid, ss)
		      end
		  | _ => (pos - start, ss)
		(* end case *))
	  in
	    scan (start, ss)
	  end

    local
      val nSpaces = 128
      val spaces = SS.full(CharVector.tabulate(nSpaces, fn _ => #" "))
      fun indent (n, txt) = if (n > nSpaces)
	    then indent(n-nSpaces, spaces::txt)
	    else SS.slice(spaces, 0, SOME n) :: txt
      val pos = ("<from CodeScannerFn.stylize>", 0, 0)
    in
    fun space sp = M.DATA(SS.concat(indent(sp, [])))
    fun mkTok (0, NONE, txt) = [M.DATA(SS.string txt)]
      | mkTok (sp, NONE, txt) = [M.DATA(SS.concat(indent(sp, [txt])))]
      | mkTok (0, SOME elem, txt) =
	  [M.ELEM{elem = elem, body = [M.DATA(SS.string txt)], pos = pos}]
      | mkTok (sp, SOME elem, txt) = [
	  (* NOTE: tokens are in reverse order! *)
	    M.ELEM{elem = elem, body = [M.DATA(SS.string txt)], pos = pos},
	    space sp
	  ]
    val nl = M.DATA "\n"
    end (* local *)

    fun stylize {code, trim} = let
	  fun scanStr (pos, sp, ss, toks, lns) = let
		val (sp', ss) = scanWS (pos, ss)
		val sp = sp' + sp
		in
		  case S.getc ss
		   of NONE => (pos, sp, ss, toks, lns)
		    | SOME(#"\n", ss) =>
			scanStr(0, 0, ss, [], (rev(nl::toks)) :: lns)
		    | _ => (case S.scan ss
			 of NONE => raise Fail "unable to scan data"
			  | SOME(optElem, txt, ss) => let
			      val ts = mkTok(sp, optElem, txt)
			      in
				scanStr (pos+sp+(SS.size txt), 0, ss, ts@toks, lns)
			      end
			(* end case *))
		  (* end case *)
		end
	  and scanContent (pos, sp, st, [], toks, lns) =
		(pos, sp, st, rev((rev toks) :: lns))
	    | scanContent (pos, sp, st, M.DATA s :: r, toks, lns) = let
		val (pos, sp, st, toks, lns) =
		      scanStr(pos, sp, S.addString(st, s), toks, lns)
		in
		  scanContent (pos, sp, st, r, toks, lns)
		end
	    | scanContent (pos, sp, st, M.ELEM{elem, body, pos=p} :: r, toks, lns) = (
		case scanContent(pos+sp, 0, st, body, [], [])
		 of (pos, sp', st, [ln]) => let
		      val tok = M.ELEM{elem = elem, body = ln, pos = p}
		      val toks = if (sp = 0)
			    then tok :: toks
			    else tok :: space sp :: toks
		      in
			scanContent (pos, sp', st, r, toks, lns)
		      end
		  | _ => raise Fail "unexpected newline in code markup"
		(* end case *))
	(* first we stylize the text and group it into lines *)
	  val (_, _, _, lns) = scanContent (0, 0, S.initial, code, [], [])
	  in
	    if trim
	      then let
		val maxIndent = 10000
		fun indentOf (M.DATA s :: r) = let
		      fun cntSp (i, ss) = (case SS.getc ss
			     of SOME(#" ", ss) => cntSp(i+1, ss)
			      | SOME(#"\t", ss) => cntSp(i + expandTab i, ss)
			      | SOME(c, _) => i
			      | NONE => if List.null r then maxIndent else i
			    (* end case *))
		      in
			cntSp(0, SS.full s)
		      end
		  | indentOf [] = maxIndent
		  | indentOf _ = 0
		val minIndent =
		      List.foldl (fn (ln, mi) => Int.min(indentOf ln, mi)) maxIndent lns
		fun trimIndent (M.DATA s :: r) = let
		      fun trim (0, ss) = SS.string ss
			| trim (i, ss) = (case SS.getc ss
			     of SOME(#" ", ss) => trim(i-1, ss)
			      | SOME(#"\t", ss) => trim(i - expandTab i, ss)
			      | NONE => "" (* this case might happen because of blank lines *)
			      | _ => raise Fail(concat[
				    "trimIndent: minIndent = ", Int.toString minIndent,
				    ", s = \"", String.toString s, "\""
				  ])
			    (* end case *))
		      in
			case trim(minIndent, SS.full s)
			 of "" => r
			  | s => M.DATA s :: r
			(* end case *)
		      end
		  | trimIndent l = l
		in
		  if (minIndent = maxIndent) orelse (minIndent = 0)
		    then List.concat lns
		    else List.concat(List.map trimIndent lns)
		end
	      else List.concat lns
	  end
handle ex => raise ex

(* DEBUG
val stylize = fn arg => let val res = stylize arg in
print "CodeScannerFn.stylize:\n";
PrintMLDoc.print {say = print, flush = fn _ => TextIO.flushOut TextIO.stdOut} res;
print "*****\n";
res
end
DEBUG *)

  end
