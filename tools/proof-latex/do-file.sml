(* main.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A tool for extracting compact 'proof' latex output from ML-Doc documents
 *
 * Parameters controlling output are:
 *   commentSep
 *   indexFlag
 *)

structure DoFile : sig

    val error : string -> 'a
    val errprint : string -> unit
    val perror : (string * MLDocElem.element) -> unit

    val doFile : {
	    config : MLDocConfig.configuration,
	    standalone : bool,
	    index : bool,
	    outStrm : TextIO.outstream,
	    doc : MLDocMarkup.markup list,
            level : int
	  } -> unit

  end = struct

  structure F = Format

  structure E = MLDocElem
  structure P = MLDocParser
  structure M = P.Markup
  structure Math = MLDocMath
  structure PM = PrintMarkup(M)
  structure SS = Substring
  structure IO = TextIO

  
  fun error msg = (		(* look at: lib/mldoc/error.sml) *)
	TextIO.output(TextIO.stdErr, "Error(proof): " ^ msg ^ "\n"); 
	raise Fail "error") 

  fun errprint msg = TextIO.output(TextIO.stdErr, msg)

  fun perror(msg, name) = (
	TextIO.output(TextIO.stdErr, concat["Error(proof): ", msg, "\n"]);
	TextIO.output(TextIO.stdErr, concat[
	    "Error(proof): ", MLDocElem.elemName name, "\n"
	  ]);
	raise Fail "error")

  val printMarkup = 
    PM.print {say = fn mkup => IO.output(IO.stdOut, mkup),
	      flush = fn () => IO.flushOut IO.stdOut}

  val commentSep = 0.1			(* gap after comments in inches *)
  val preserveCRLF = ref false		(* preserve CRLF, e.g. in code *)
  val indexFlag = ref false		(* generate an index *)
  val protectFlag = ref false		(* Hack: protect CD in sect and chapt *)
 
  fun protect f = let 
    val old = !protectFlag
  in
    protectFlag := true; f(); protectFlag:= old
  end

  fun colChar (SOME E.ALIGN_CENTER) = "c"
    | colChar (SOME E.ALIGN_LEFT)   = "l"
    | colChar (SOME E.ALIGN_RIGHT)  = "r"
    | colChar NONE                  = "c"

  fun statusStr NONE = ""
    | statusStr (SOME E.REQUIRED) = " (* REQUIRED *)"
    | statusStr (SOME E.OPTIONAL) = " (* OPTIONAL *)"
    | statusStr (SOME E.PROPOSED) = " (* PROPOSED *)"

  (* latexString -- replace a string with latex string *)
  fun latexString s =
	(LaTeXTranslate.transData s) handle e => (errprint "exception 4\n"; raise e)

  fun emphString [M.DATA str] = let
        fun scanEmph (ss, l) =
              if SS.isEmpty ss then loop(ss,l)
              else let
                val (emph, rest) = SS.splitl (fn c => c <> #"}") ss
                val rest' = if SS.isEmpty rest then rest
                            else SS.triml 1 rest
                in
                  loop (rest', "}"::(SS.string emph)::"{\\it "::l)
                end
        and scanPlain (ss, l) = let
              val (plain, rest) = SS.splitl (fn c => c <> #"{") ss
              in
                loop (rest, (latexString (SS.string plain))::l)
              end
        and loop (ss, l) =
              case SS.getc ss of
                NONE => concat (rev l)
              | SOME (#"{",rest) => scanEmph (rest,l)
              | _ => scanPlain (ss,l)
        in
          loop (SS.full str, [])
        end

  fun getPCData[M.DATA t] = latexString t

  fun doFile{config, standalone, index, outStrm, doc, level} = let
    fun pr s = TextIO.output (outStrm, s)
    fun prf fmt items = pr (F.format fmt items)

  
    fun indent(env, f) = 
      (prf "\\begin{%s}\n" [F.STR env];
       f() before (prf "\\end{%s}\n" [F.STR env]))

    (* weird things happen when this is replaced by a macro. *)
    fun indentComment(k, f) = let
      val params = 
	  "{\\leftmargin %fin\\parsep 0in\\itemsep 0in" ^
	  "\\rightmargin 1in}\n"
    in		
      prf ("\\begin{list}{}" ^ params) [F.REAL k];
      f () before (pr "\\end{list}\n")
    end

    (* DOCHDR ::=  "TITLE & (AUTHOR?) & (VERSION?) & (COPYRIGHT* ) *)
    fun docHdr [] = error "doFile.docHdr"
      | docHdr(M.ELEM{elem=E.COPYRIGHT{owner,year}, ...}::rest) =
         (prf "\\COPYRIGHT{%d %s}\n\n" 
	      [F.INT year, F.STR (latexString owner)];
	  docHdr rest)
      | docHdr(M.ELEM{elem=E.TITLE, body, ...}::rest) =
	 (prf "\\TITLE{%s}\n\n" [F.STR (getPCData body)]; 
	  docHdr (rest))
      | docHdr(M.ELEM{elem=E.VERSION{verid, year, month, ...},...}::rest) =
	(case (verid, month) 
	 of (SOME id, SOME mm) =>
	     prf "\\VERSION{%s, %d/%d}\n\n" [F.STR id, F.INT mm, F.INT year]
          | (SOME id, NONE) => 
	     prf "\\VERSION{%s, %d}\n\n" [F.STR id, F.INT year]
	  | (NONE, NONE) => prf "\\VERSION{%d}"  [F.INT year]
	 (*esac*);
	 docHdr rest)
      | docHdr(M.ELEM{elem=E.AUTHOR{name, email, ...}, ...}::rest) = 
	(prf "\\AUTHOR{%s, %s}\n\n" 
	     [F.STR(latexString name), F.STR(latexString email)];
         docHdr rest)
      | docHdr doc = doc

    fun typeName ((m as M.ELEM {elem=E.ID,...})::_) = m
      | typeName (_::rest) = typeName rest

    fun getID(M.ELEM{elem=E.ID, body=[M.DATA id], ...}) = 
	  SS.string (SS.dropr Char.isSpace (SS.dropl Char.isSpace (SS.full (latexString id))))

  (* %SIGID = (ID | SIGREF) *)
    fun getSigId(elem as M.ELEM{elem=E.ID, ...}) = getID(elem)
      | getSigId(M.ELEM{elem=E.SIGREF _, body, ...}) = emphString body
      | getSigId _ = error "chkInclude.getSigId"

    (* IDREF ::= %MLREF.TOP | %MLREF.SPEC | IDREF
     * % MLREF.TOP  ::= SIGREF | FCTREF | FCTARGREF
     * % MLREF.SPEC ::= STRREF | EXNREF | TYREF | CONREF | VALREF
     *)
    fun chkIdRef(M.ELEM{elem, body, ...}) = let
	  fun xref cmd = 
	    (prf "%s{%s} " [F.STR cmd, F.STR(emphString body)];
	     true)
	in
	  case elem
	   of E.SIGREF _ => xref "\\SIGREF"
	    | E.FCTREF _ => xref "\\FCTREF"
	    | E.FCTARGREF _ => xref "\\FCTARGREF"
	    | E.STRREF _ => xref "\\STRREF"
	    | E.EXNREF _ => xref "\\EXNREF"
	    | E.TYREF _  => xref "\\TYREF"
	    | E.CONREF _ => xref "\\CONREF"
	    | E.VALREF _ => xref "\\VALREF"
	    | E.IDREF _  => xref "\\IDREF"
	    | _ => false
	  (*esac*) 
	end
      | chkIdRef _ = false

    fun getTy'(M.ELEM{elem=E.TY, body=[M.DATA ty], ...}) = ty
    val getTy = latexString o getTy' 

    fun doIdRef mkup = (chkIdRef mkup; ())

    (* XREF ::=  %IDREF | AREF | DOCREF | SECREF | FLOATREF | CITE *)
    and chkXref(mkup as M.ELEM{elem, body, ...}) = 
      (chkIdRef mkup orelse 
       (case elem
	 of E.AREF _ => (app (fn b => (chkInline b; ())) body; true)
	  | E.DOCREF{document} => 
	     (app (fn b => (chkInline b; ())) body; true)  (* ?? *)
	  | E.SECREF{label} => (prf "\\SECREF{%s}" [F.STR label]; true)
(* FIXME: need to resolve label to float kind! *)
	  | E.FLOATREF{label} => (prf "\\TBLREF{%s}" [F.STR label]; true)
	  | E.CITE{key} => (prf "\\CITE{%s}" [F.STR key]; true)
	  | _ => false
	(*esac*)))

    (* and doXref mkup = (chkXref mkup; ()) *)

    and doMathMarkup(mm) = let
      fun sum(mode, ll, ul, NONE) = 
	    (pr mode; doMathMarkup ll; pr "^"; doMathMarkup ul)
	| sum(mode, ll, ul, SOME mm) = 
	    (sum(mode, ll, ul, NONE); doMathMarkup mm)
    in
      case mm
       of Math.Data s => pr (latexString s)
	| Math.Text m => (List.map doEmph m; ())
	| Math.Group ml => (pr "{"; app doMathMarkup ml; pr "}")
	| Math.Sum{ll, ul, opd} => sum("\\sum_", ll, ul, opd)
	| Math.Prod{ll, ul, opd} => sum("\\prod_", ll, ul, opd)
	| Math.Union{ll, ul, opd} => sum("\\cup_", ll, ul, opd)
	| Math.Intersect{ll, ul, opd} => sum("\\cap_", ll, ul, opd)
	| Math.Frac{num, denom} => 
	   (pr "\\frac"; doMathMarkup num; doMathMarkup denom)
	| Math.Subscript(x,i) => 
	   (doMathMarkup x; pr "_"; doMathMarkup i)
	| Math.Superscript(x,i) => 
	   (doMathMarkup x; pr "^"; doMathMarkup i)
	| Math.Norm mm => (pr "|"; doMathMarkup mm; pr "|")
	| Math.Set ml => (pr "\\{"; app doMathMarkup ml; pr "\\}")
      (*esac*)
    end

    and chkMath(M.ELEM{elem=E.MATH, body, ...}) = 
	  (pr "$"; 
	   app doMathMarkup (Math.getMathContents body); 
	   pr "$";
	   true)
      | chkMath _ = false

    and chkInline(M.DATA s) = (pr(latexString s); true)
      | chkInline m = 
	   doEmph m orelse chkXref m orelse chkMath m
	   orelse chkAnchor m

    and emph(em, body) = 
      (prf "{\\%s " [F.STR em]; 
       app (fn b => (chkInline b; ())) body; pr "}"; 
       true)

    and emph'(em, body) = 
      (prf "\\%s{" [F.STR em];
       app (fn b => (chkInline b; ())) body;
       pr "}";
       true)

    and cd body = 
       if !protectFlag then 
	 (pr "\\protect{"; emph'("tt", body); pr "}"; true)
       else 
	 emph'("cd", body)

    and doEmph(M.ELEM{elem=E.EM, body, ...}) = emph("em", body)
      | doEmph(M.ELEM{elem=E.IT, body, ...}) = emph("it", body)
      | doEmph(M.ELEM{elem=E.BF, body, ...}) = emph("bf", body)
      | doEmph(M.ELEM{elem=E.TT, body, ...}) = emph("tt", body)
      | doEmph(M.ELEM{elem=E.CD _, body, ...}) = cd(body)
      | doEmph(M.ELEM{elem=E.KW, body, ...}) = cd(body)
      | doEmph(M.ELEM{elem=E.ARG, body, ...}) = emph("it", body)
      | doEmph _ = false

    and chkAnchor(M.ELEM{elem=E.ADEF _, body, ...}) = emph'("ADEF", body)
      (* | chkAnchor(M.ELEM{elem=E.AREF _, body, ...}) = emph'("AREF", body) *)
      | chkAnchor _ = false

    fun doInline body = (chkInline body; ())

    fun docBody level body = let
      local
	fun headingIO(sect, f) = (pr ("\t" ^ sect ^ "{"); f(); pr "}\n")
	fun part f = headingIO("\\part", f)
	fun chapter f = headingIO("\\chapter", f)
	fun section f = headingIO("\\section", f)
	fun subsection f = headingIO("\\subsection", f)
	fun subsubsection f = headingIO("\\subsubsection", f)
	fun boldface f = (pr "{\\bf "; f(); pr "}")
	fun heading "Part" level = 
	     (case level
	      of 0 => part
	       | 1 => chapter
	       | 2 => section
	       | 3 => subsection
	       | 4 => subsubsection
	       | _ => boldface
	      (*esac*))
	  | heading "Chapter" level = 
	     (case level
	      of 0 => chapter
	       | 1 => section
	       | 2 => subsection
	       | 3 => subsubsection
	       | _ => boldface
	      (*esac*))
	  | heading "Section" level = 
	     (case level
	      of 0 => section
	       | 1 => subsection
	       | 2 => subsubsection
	       | _ => boldface
	     (*esac*))
	  | heading _ _ = error "heading"
	val topLvlSect = 
	  valOf(MLDocConfig.getStr(config, ["TopLevelSection"]))
      in
	val doHeading = heading topLvlSect
      end (*local*)

      fun doCode(M.DATA s::rest) = (pr(latexString s); doCode rest)
	| doCode(M.ELEM{elem=E.ARG, body, ...}::rest) = 
	   (pr(getPCData body); doCode rest)
	| doCode(elem::rest) = chkIdRef elem andalso doCode rest
	| doCode [] = true

      fun chkCODE(M.ELEM{elem=E.CODE _, body, ...}) = let
	    val oldF = !preserveCRLF
	  in
	    pr "\n\\begin{code}\n"; 
	    preserveCRLF := true;
	    doCode body; 
	    pr "\n\\end{code}\n";
	    preserveCRLF := oldF;
	    true
	  end
	| chkCODE _ = false 

      fun chkPrototy(M.ELEM{elem=E.PROTOTY, body, ...}) = let
	    fun doEvalto(M.ELEM{elem=E.EVALTO, body, ...}) = doCode body
	    fun doit ((el as M.ELEM{elem=E.EVALTO, body, ...})::rest) = 
		 (pr " = "; doEvalto el; doit rest)
	      | doit (el::rest) = (doCode [el]; doit rest)
	      | doit [] = ()
	    fun doProto [M.ELEM{elem=E.PROTO, body, ...}] = 
		 (pr "{\\tt "; doit body; pr "} --- "; true)
	      | doProto(M.ELEM{elem=E.PROTO, body, ...}::rest) = 
		 (pr "{\\tt "; doit body; pr "}\n\\item "; doProto rest)
	      | doProto [] = true
	  in
	    doProto body
	  end
	| chkPrototy _ = false

      fun blockEnv(env, emph, body) = 
	(prf "\\begin{%s}%s\n" [F.STR env, F.STR emph];
	 app doPP body;
	 prf "\\end{%s}\n" [F.STR env])

      (* WHERETYPE ::= (TYPARAM?, ID, TY) *)
      and doWhere(arg as (M.ELEM{elem=E.WHERETYPE, body, ...}::_)) = let
	    fun split((e as M.ELEM{elem=E.WHERETYPE, ...})::rest, acc) = 
		  split(rest, e::acc)
	      | split([], acc) = (acc, [])
	      | split(rest, acc) = (rev acc, rest)
	    val (wheretys, rest) = split(arg, [])
	    fun tyParam(M.ELEM{elem=E.TYPARAM, body, ...}::rest) =
		  (pr (getPCData body); pr " "; rest)
	      | tyParam mkup = mkup
	    fun whereBody (id::ty::_) = (
		  pr (getID id); pr " = "; pr (getTy ty))
	    fun f () = 
	      app (fn M.ELEM{body, ...} => 
		      (pr "\\item {\\WHERE} {\\TYPE} "; 
		       whereBody (tyParam body)))
		  wheretys
	  in
	    indent("wheretype", f);
	    rest
	  end
	| doWhere mkup = mkup

	(* STRSIG ::= OPAQUE?, ((ID, WHERETYPE* ) | SIGBODY) *)
	and doStrsig (name, mkup) = let
	  fun do_strsig((node as M.ELEM{elem=E.ID, body, ...})::rest) = 
	       (prf "%s\n" [F.STR (getID node)];
		doWhere rest)
	    | do_strsig(M.ELEM{elem=E.SIGBODY{sigid, ...}, body, ...}::rest) =
	       (case sigid
		of NONE =>
		    (pr "{\\SIG}\n";
		     doSigBody(name, body);
		     pr "\\item {\\END}\n";
		     rest)
		 | SOME id => let
		     val sigName = latexString id
		   in
		     prf "%s\n" [F.STR(sigName)];
		     prf "\\item {\\SIGNATURE} %s = \\SIG\n" [F.STR(sigName)];
		     doSigBody(sigName, body);
		     pr "\\item {\\END}\n";
		     rest
		   end
		(*esac*))
	    | do_strsig _ = error "doStrsig"
	in
	  case hd mkup 
	  of M.ELEM{elem=E.OPAQUE,...} => (pr " :> "; do_strsig (tl mkup))
	   | _ => (pr " : "; do_strsig mkup)
	end
      (* LIST ::= " ITEMIZE | ENUM | DESCRIP *)
      and chkList(M.ELEM{elem, body, ...}) = let
	    fun doItem(M.ELEM{elem=E.ITEM, body, ...}) = app doPP body

	    fun doItemize () =
	      (pr "\\begin{itemize}\n";
	       app (fn item => (pr "\\item "; doItem item)) body;
	       pr "\\end{itemize}\n";
	       true)

	    fun doEnumerate () = 
	      (pr "\\begin{enumerate}\n";
	       app (fn item => (pr "\\item "; doItem item)) body;
	       pr "\\end{enumerate}\n";
	       true)

	    fun doDescription () = let
	      fun doDtag(M.ELEM{elem=E.DTAG, body, ...}) = 
		app (fn b => (chkInline b; ())) body
	      fun descrip (dtag::item::rest) =
		    (pr "\\item["; doDtag dtag; pr "] ";
		     doItem item;
		     descrip rest)
		| descrip [] = ()
	    in
	      pr "\\begin{description}\n";
	      descrip body;
	      pr "\\end{description}\n";
	      true
	    end
	  in
	    case elem
	     of E.ITEMIZE => doItemize ()
	      | E.ENUM => doEnumerate ()
	      | E.DESCRIP => doDescription()
	      | _ => false
	  end
	| chkList _ = false

      (* PP ::= ((%INLINE | %LIST | %BLOCK)* )	*)
      and chkPP(M.ELEM{elem=E.PP, body, ...}) = let
	    fun f(elem::rest) =
		 if chkInline elem orelse 
		    chkList elem orelse 
		    chkBlock elem
		 then f rest
		 else error "chkPP.f"
	      | f [] = true
	  in
	    pr "\n\n";
	    f body
	  end
	| chkPP _ = false

      and doPP mkup = (chkPP mkup; ())


      and chkExample(M.ELEM{elem=E.EXAMPLE, body, ...}) = 
	  (blockEnv("example", "", body); true)
	| chkExample _ = false

      and chkQuestion (M.ELEM{elem=E.QUESTION, body, ...}) = 
	  (blockEnv("question", "\\bf", body); true)
	| chkQuestion _ = false

      and chkRationale (M.ELEM{elem=E.RATIONALE, body, ...}) = 
	  (blockEnv("rationale", "", body); true)
	| chkRationale _ = false

      and chkImplNote (M.ELEM{elem=E.IMPLNOTE, body, ...}) = 
	  (blockEnv("implnote", "", body); true)
	| chkImplNote _ = false

      and chkSysNote (M.ELEM{elem=E.SYSNOTE{opsys, arch}, body, ...}) =
	  (blockEnv("sysnote", "", body); true)
	| chkSysNote _ = false

      and chkDisplaymath(M.ELEM{elem=E.DISPLAYMATH, body, ...}) = let
	    val oldCrlf = !preserveCRLF
	  in
	    preserveCRLF := true;
	    pr "\\begin{displaymath}\n";
	    app doMathMarkup (Math.getMathContents body);
	    pr "\\end{displaymath}\n";
	    preserveCRLF := oldCrlf;
	    true
	  end
	| chkDisplaymath _ = false

    (* FLOAT ::= (CAPTION, (FIGURE | TABLE))
     * CAPTION ::= (%INLINE)*
     *)
      and chkFloat (M.ELEM{elem=E.FLOAT{label, capalign}, body, ...}) = let
	    val prInl = app (fn b => (chkInline b; ()))
	    val (optCaption, body') = (case body
		   of (M.ELEM{elem=E.CAPTION,body=cbody,...})::rest =>
			(SOME cbody, rest)
		    | _ => (NONE, body)
		  (* end case *))
	    fun doCaption () = 
	      (case optCaption
	       of NONE => ()
	        | (SOME cbody) => 
		    (pr "  \\caption{"; prInl cbody; pr "}\n";
		     prf "  \\label{%s}\n" [F.STR label])
	       (*esac*))
	    in
	      case body'
	       of ((tbl as M.ELEM{elem=E.TABLE{long, small}, ...})::_) => (
		    pr "\\begin{table}[tbp]\n";
		    if (capalign = SOME E.BOTTOM)
		      then (doTable true tbl; doCaption())
		      else (doCaption(); doTable true tbl);
		    pr "\\end{table}%\n")
		| ((fig as M.ELEM{elem=E.FIGURE _, ...})::_) => (
		    pr "\\begin{figure}[tbp]\n";
		    if (capalign = SOME E.TOP)
		      then (doCaption(); doFigure fig)
		      else (doFigure fig; doCaption());
		    pr "\\end{figure}%\n")
	      (* end case *);
	      true
	    end
	| chkFloat _ = false

      and doFigure (M.ELEM{elem=E.FIGURE{file}, ...}) =
	   prf "\\INPUTEPS{%s}" [F.STR file]
	| doFigure _ = error "doFigure"

      (* TABLE ::= (COL+, TR+) 
       * COL ::= EMPTY
       * TR ::= (%TBLCELL)*
       * TH ::= (%INLINE)*
       * TD ::= (%INLINE)*
       *)
      and chkTable inFloat (M.ELEM{elem=E.TABLE{long, small}, body, ...}) = let
	    val prInl = app (fn b => (chkInline b; ()))
	    fun prHdr body = (emph("bf", body); ())
	    fun cell (NONE,_,body,prb) = prb body
	      | cell (SOME n,align,body,prb) = (
		  prf "\\multicolumn{%d}{%s}{" [F.INT n, F.STR(colChar align)];
		  prb body;
		  pr "}\n"
		)
	    fun doCell (M.ELEM{elem=E.TH{colspan,align},body,...})
		  = cell (colspan, align, body, prHdr)
	      | doCell (M.ELEM{elem=E.TD{colspan,align},body,...})
		  = cell (colspan, align, body, prInl)
	    fun doRow [cell] = (doCell cell; pr " \\\\\n")
	      | doRow (cell::rest) = (doCell cell; pr " & "; doRow rest)
	    fun doTR (M.ELEM{elem=E.TR, body, ...}) = doRow body
	    fun doCols (M.ELEM{elem=E.COL{parbox=SOME s, ...},...}::rest, cols) =
		  doCols(rest, "}"::s::"p{"::cols)
	      | doCols (M.ELEM{elem=E.COL{align, ...},...}::rest, cols) =
		  doCols(rest, (colChar align)::cols)
	      | doCols (rest,cols) = (
		    prf "{%s}\n" [F.STR(concat(rev cols))];
		    rest
		  )
	    in
	      if not inFloat then pr "  \\begin{center}\n" else ();
	      pr "  \\begin{tabular}\n";
	      app doTR (doCols (body, []));
	      pr "  \\end{tabular}\n";
	      if not inFloat then pr "  \\end{center}\n" else ();
	      true
	    end
	| chkTable _ _ = false

      and doTable inFloat mkup = (chkTable inFloat mkup; ())

     (* %BLOCK ::= " DISPLAYMATH | FLOAT | TABLE | EXAMPLE | QUESTION |
      *              IMPLNOTE | SYSNOTE | RATIONALE | CODE | GRAMMAR"
      *)
      and chkBlock(elem as M.ELEM _) =
	     chkDisplaymath elem orelse 
	     chkFloat elem orelse
	     chkTable false elem orelse
	     chkExample elem orelse
	     chkQuestion elem orelse 
	     chkImplNote elem orelse 
	     chkSysNote elem orelse 
	     chkRationale elem orelse
	     chkCODE elem 
	| chkBlock _ = false 


      and chkSharing(M.ELEM{elem=E.SHARING{ty}, body, ...}) = let
	    fun sharingBody(elem::M.ELEM{elem=E.EQU, ...}::rest) = 
		  (pr (getID elem ^ " = "); 
		   sharingBody rest)
	      | sharingBody([elem]) = pr (getID elem ^ "\n")
	    fun sharingKind true = pr "{\\SHARING} {\\TYPE} "
	      | sharingKind false = pr "{\\SHARING} {\\STRUCTURE} ";

	    fun f () = 
	      (pr "\\item "; sharingKind ty; sharingBody body)
	  in
	    indent("sharing", f);
	    true
	  end
	| chkSharing _ = false 

      and doSharing mkup = (chkSharing mkup; ())

     (* COMMENT ::= ((PROTOTY, PP+)+ | PP+) *)
      and chkComment(M.ELEM{elem=E.COMMENT, body, ...}) = let
	    fun f(elem::rest) = 
		 (pr "\\item "; 
		  if chkPrototy elem orelse chkPP elem then f rest
		  else error ("chkComment "))
	      | f [] = ()
	  in
	    indentComment(0.4, fn () => f body); true
	  end
	| chkComment _ = false 

      (* SIGBODY ::=  (SPEC+)	*)
      and doSigBody(moduleName, body) = let
	(* mkIndexPair --- double entry index. *)
	fun mkIndexPair spec = 
	  if !indexFlag then 
	    if spec = "!" 
	      then prf "{\\index{\"!!%s}}" (* " *) [F.STR moduleName]
	      else prf "{\\index{%s!%s}}" [F.STR spec, F.STR moduleName]
	  else ()

	(* mkIndexDT -- for datatype constructors *)
	fun mkIndexDT spec = let
	  val string = Substring.string
	  val ss = Substring.full spec
	  val tok = 
	       case (Substring.sub(ss, 0), Substring.size ss > 2)
		of (#"=", true) => Substring.triml 2 ss
		 | (#"$", true) => Substring.triml 4 ss
		 | _ => ss
	in
	  mkIndexPair(string tok)
	end

	(* doExn --- process EXN element *)
	fun chkExn(M.ELEM{elem=E.EXN, body=[id], ...}) = 
	      (mkIndexPair (getID id);
	       prf "{\\EXCEPTION} %s\n\n" [F.STR (getID id)]; 
	       true)
	  | chkExn(M.ELEM{elem=E.EXN, body=[id, ty], ...}) = 
	      (mkIndexPair (getID id);
	       prf "{\\EXCEPTION} %s of %s\n\n" 
		   [F.STR(getID id), F.STR(getTy ty)];
	       true)
	  | chkExn _ = false

	(* doRaises --- process RAISES element *)
	fun doRaises(M.ELEM{elem=E.RAISES, body, ...}) = let
	  fun f () = 
	    (pr "\\item {\\tt Raises:~~}"; 
	     app (fn el => (chkIdRef el; pr " ")) body)
	in
	  indent("raises", f)
	end

	(* chkVal --- process VAL element *)
	fun chkVal(M.ELEM{elem=E.VAL, body=[id,ty], ...}) = 
	     (mkIndexPair(getID id);
	      prf "{\\VAL} {\\tt %s : %s} \n\n" 
		  [F.STR(getID id), F.STR(getTy ty)];
	      true)
	  | chkVal(M.ELEM{elem=E.VAL, body=[id,ty,raises], ...}) = 
	     (mkIndexPair (getID id);
	      prf "{\\VAL} {\\tt %s : %s} \n\n" 
		  [F.STR(getID id), F.STR(getTy ty)];
	      doRaises raises;
	      true)
	  | chkVal _ = false

	(* (%SIGID), (WHERETYPE* ) *)
	fun chkInclude(M.ELEM{elem=E.INCLUDE, body=sigId::rest, ...}) = let
	     val sid = getSigId sigId
	    in
	      mkIndexPair sid;
	      prf "{\\INCLUDE} %s\n" [F.STR sid];
	      doWhere rest;
	      true
	    end
	  | chkInclude _ = false

	(* SUBSTRUCT:: (ID, %STRSIG)
	 * %STRSIG::   (OPAQUE?, ((ID, WHERETYPE* ) | SIGBODY) 
	 *) 
	fun chkSubstruct(M.ELEM{elem=E.SUBSTRUCT, body=str::rest, ...}) = let
	      val strName = getID str
	    in
	      mkIndexPair strName;
	      prf "{\\STRUCTURE} %s " [F.STR strName];
	      doStrsig(strName, rest);
	      true
	    end
	  | chkSubstruct _ = false

	(* (TYPARAM?, ID, TY?) *)
	fun doTyparamId(M.ELEM{elem=E.TYPARAM, body, ...}::rest) =
	     (pr (getPCData body ^ " "); doTyparamId rest)
	  | doTyparamId(M.ELEM{elem=E.ID, body, ...}::rest) =
	     (pr (getPCData body ^ " "); doTyparamId rest)
	  | doTyparamId((ty as M.ELEM{elem=E.TY, body, ...})::rest) =
	     (pr "= "; pr (getTy ty); rest)
	  | doTyparamId mkup = mkup

	fun chkType(M.ELEM{elem=E.TYPE, body, ...}) = 
	      (mkIndexPair (getID (typeName body));
	       pr "{\\TYPE} ";  
	       doTyparamId body;  pr "\n";   
	       true)
	  | chkType _ = false

	fun chkEqtype(M.ELEM{elem=E.EQTYPE, body, ...}) = 
	      (mkIndexPair (getID (typeName body));
	       pr "{\\EQTYPE} ";  
	       doTyparamId body;  
	       pr "\n";   
	       true)
	  | chkEqtype _ = false

	fun chkDatatype(M.ELEM{elem=E.DATATYPE{compact, recursive}, 
			      body, ...}) = let
	      (* Categorize --- datatype components. *)
	      fun categorize(M.ELEM{elem=E.CONS, body=[id,ty,comm], ...}) = 
		   (getID id, SOME(getTy' ty), SOME comm)
		| categorize(M.ELEM{elem=E.CONS, body=[id], ...}) = 
		   (getID id, NONE, NONE)
		| categorize(M.ELEM{elem=E.CONS, body=[id,tc], ...}) = 
		   (case tc
		     of M.ELEM{elem=E.COMMENT, ...} => (getID id, NONE, SOME tc)
		      | M.ELEM{elem=E.TY, ...} => (getID id, SOME(getTy' tc), NONE)
		    (*esac*))

	      fun bars((x,y,z)::rest) = 
		   ("= " ^ x, y, z):: map (fn (u,v,w) => ("$|$ " ^ u, v, w)) rest

	      val _ = mkIndexPair (getID (typeName body))
	      val _ = if recursive then pr "{\\AND} " else pr "{\\DATATYPE} "
	      val rest = doTyparamId body
	      val constrs = bars (map categorize rest)

	      (* oneLine --- put everything on one line *)
	      fun oneLine () = let
		fun emit [] = NONE
		  | emit((id, SOME ty, NONE)::rest) = 
		     (mkIndexDT id;
		      prf "\\mbox{\\tt %s of %s}\n" 
			   [F.STR id, F.STR(latexString ty)];
		      emit rest)
		  | emit [(id, SOME ty, SOME comm)] = 
		     (mkIndexDT id;
		      prf "\\mbox{\\tt %s of %s}\n" 
			  [F.STR id, F.STR(latexString ty)];
		      SOME comm)
		  | emit((id, NONE, NONE)::rest) = 
		     (mkIndexDT id;
		      prf "\\mbox{\\tt %s}\n" [F.STR id];
		      emit rest)
		  | emit [(id, NONE, SOME comm)] = 
		     (mkIndexDT id;
		      prf "\\mbox{\\tt %s}\n" [F.STR id];
		      SOME comm)
	      in
		pr "\\begin{minipage}[t]{4in}\n";
		let val comm = emit constrs
		in
		  pr "\\end{minipage}\n";
		  case comm of NONE  => () | SOME c => (chkComment c; ())
		end
	      end

	      (* onePerLine --- components on a new line *)
	      fun onePerLine () = let
		fun doTy s = let
		  fun skipWS(#" "::cs) = skipWS cs
		    | skipWS cs = cs

		  fun fields(#"\\" :: #"n"::cs, acc, done) =
			fields(skipWS cs, [], String.implode(rev acc)::done)
		    | fields(c::cs, acc, done) = fields(cs, c::acc, done)
		    | fields([], [], done) = rev done
		    | fields([], acc, done) = 
			fields([], [], String.implode(rev acc)::done)

		  fun f [] = ()
		    | f (s::rest) = 
			(prf "\\item %s\n" [F.STR(latexString s)]; 
			 f rest)
		in
		  case fields(String.explode s, [], [])
		   of [] => error "onePerLine.doTy"
		    | [s] => prf "%s\n" [F.STR s]
		    | flds => indent("record", fn () => f flds)
		end

		fun doCons(id, NONE, NONE) = 
		      (* (mkIndexDT id; prf "\\item %s\n" [F.STR id]) *)
		      (prf "\\item %s " [F.STR id]; mkIndexDT id; pr "\n")
		  | doCons(id, NONE, SOME comm) = 
		      ( (* mkIndexDT id; *)
		       (* prf "\\item {\\tt %s}\n" [F.STR id]; *)
		       prf "\\item {\\tt %s}" [F.STR id];
		       mkIndexDT id; pr "\n";
		       chkComment comm; ())
		  | doCons(id, SOME ty, NONE) = 
		      ( (* mkIndexDT id; *)
		       prf "\\item {\\tt %s "  [F.STR id]; 
		       mkIndexDT id;
		       pr " of ";
		       doTy ty;
		       pr "}")
		  | doCons(id, SOME ty, SOME comm) =
		      (* (mkIndexDT id; *)
		       (prf "\\item {\\tt %s "  [F.STR id];
		       mkIndexDT id;
		       pr " of ";
		       doTy ty;
		       pr "}";
		       chkComment comm; ())
	      in
		pr "\n";
		indent("datatype", fn () => app doCons constrs)
	      end

	      fun whiteSpace (#" "::cs) = whiteSpace cs
		| whiteSpace (_::cs) = false
		| whiteSpace [] = true

	      fun hasNewline(#"\\" :: #"n"::cs) = not(whiteSpace cs)
		| hasNewline(_::cs) = hasNewline cs
		| hasNewline [] = false

	      fun doOneLine [] = true
		| doOneLine [(id,SOME ty,_)] =
		  if hasNewline(String.explode ty) then false else true
		| doOneLine [(_,NONE,_)] = true
		| doOneLine((_,_,SOME comm)::_) = false
		| doOneLine((x,SOME ty,comm)::rest) = 
		  if hasNewline (String.explode ty) then false
		  else doOneLine rest
		| doOneLine(_::rest) = doOneLine rest
	    in
	      if doOneLine constrs then oneLine() else onePerLine();
	      true
	    end
	  | chkDatatype _ = false

      (* DATATYPEDEF ::= (ID, %CODE) *)
	fun chkDatatypeDef (M.ELEM{elem=E.DATATYPEDEF, body, ...}) = let
	      val _ = mkIndexPair (getID (typeName body))
	      val _ = pr "{\\DATATYPE} "
	    (* NOTE: the DTD guarantees that there is no type param *)
	      val rest = doTyparamId body
	      in
		pr " = {\\DATATYPE} ";
		cd rest;
		true
	      end
	  | chkDatatypeDef _ = false

	(* SPEC :=  
	 *   ((INCLUDE+ | SUBSTRUCT | SHARING+ | EXN+ |
	 *       (TYPE | EQTYPE)+ | DATATYPE+ | DATATYPEDEF+ | VAL+),
	 *     COMMENT?)
	 *)
	fun doSpec(M.ELEM{elem=E.SPEC, body, ...}) = let
	      fun elemName(M.ELEM{elem, ...}) = elem
	      fun f el = 
		(chkInclude el orelse 
		 chkSubstruct el orelse
		 chkSharing el orelse
		 chkExn el orelse 
		 chkType el orelse 
		 chkEqtype el orelse 
		 chkDatatype el orelse
		 chkDatatypeDef el orelse
		 chkVal el orelse 
		 chkComment el)
	      fun doall (el::rest) = 
		   (pr "\\item "; 
		    if f el then doall rest 
		    else (error ("doSpec" ^  E.elemName(elemName el))))
		| doall [] = ()
	    in
	      doall body
	    end

	fun f (el::rest) = (doSpec el; f rest)
	  | f [] = ()
      in
	indent("sigbody", fn () => (f body))
      end (* doSigBody *)


      (* SEEALSO ::=  ((%IDREF)+) *)
      fun doSeeAlso body = let
	fun f [] = ()
	  | f [x] = (doIdRef x; pr "\n")
	  | f (x::r) = (doIdRef x; pr ", "; f r)
      in indent("seealso", fn () => (pr "\\item "; f body))
      end

      fun chkSeeAlso(M.ELEM{elem=E.SEEALSO, body, ...}) = 
	   (doSeeAlso body; true)
	| chkSeeAlso _ = false


      
      (* SIGNATURE ::= (SIGBODY, (SIGINSTANCE* )) *)
      fun doSignature(mkup, sigId) = let
	fun doSigbody(M.ELEM{elem=E.SIGBODY{sigid, ...}, body, ...}::rest) = 
	    (case sigid
	      of NONE =>
		   (prf "\\item {\\SIGNATURE} %s : {\\SIG} =\n\n" [F.STR sigId];
		    doSigBody(sigId, body);
		    pr "\\item {\\END}\n";
		    rest)
	       | SOME s => 
		   (prf "\\item {\\SIGNATURE} %s : {\\SIG} = \n\n" [F.STR sigId];
		    doSigBody(sigId, body);
		    pr "\\item {\\END}\n";
		    rest)
	    (*esac*))

	(* SIGINSTANCE ::= (ID, (WHERETYPE* ), COMMENT?) *)
	fun chkSigInstances(M.ELEM{elem=E.SIGINSTANCE{status, opaque}, 
				   body, ...}::rest) = let
		(* mkIndex --- single entry index *)
		 fun mkIndex spec = 
		   if !indexFlag then prf "{\\index{%s}} " [F.STR spec] 
		   else ()

		 val sts = statusStr status

		 fun header (str, sep) = (
			mkIndex str;
			prf "\\item {\\STRUCTURE} %s%s%s%s\n" 
			    [F.STR str, F.STR sep, F.STR sigId, F.STR sts])

		 fun getName((M.ELEM{elem=E.ID, body, ...})::tail) =
		       (getPCData body, tail)

		 fun doColon (str, mkup) = if opaque
			then (header (str, " :> "); mkup)
			else (header (str, " : "); mkup)

		 fun comment [] = ()
		   | comment (c::_) = (chkComment c; ())

		 val doStructs = comment o doWhere o doColon o getName
	       in
		 indent("structs", fn () => doStructs body);
		 chkSigInstances rest
	       end
	  | chkSigInstances mkup = mkup

	val mkup' = indent("signature", fn () => doSigbody mkup)
      in
	chkSigInstances mkup'
      end (* doSignature *)


      (* FUNCTOR ::= (%FCTARG, %STRSIG)
       * FCTARG ::= ((ID, %SIGID) | SIGBODY)
       *) 
      fun doFunctor(mkup, fctid) = let
	fun f() = let
	  fun doFctarg(M.ELEM{elem=E.ID, body, ...}::sigId::rest) = (
		prf "(%s : %s) " [
		    F.STR(getPCData body), F.STR(getSigId sigId)
		  ];
		rest)
	    | doFctarg(M.ELEM{elem=E.SIGBODY{sigid, ...}, body, ...}::rest) =
	      (case sigid
		of NONE => 
		     (pr "(\n"; 
		      doSigBody(fctid, body) before pr ")";
		      rest)
		 | _ => error "doFunctor.doFctArg: SIGBODY"
	      (*esac*))
	in
	  prf "\\item {\\FUNCTOR %s}" [F.STR fctid];
	  doStrsig(fctid, doFctarg mkup)
	end (* f *)
      in indent("functor", f)
      end

      (* STRUCTURE ::= (%STRSIG) *)
      fun doStructure(mkup, strid) = let
	fun f () = (
	    prf "\\item {\\STRUCTURE} %s" [F.STR strid]; 
	    doStrsig(strid, mkup))
      in indent("structure", f)
      end

      (* HEAD := ((%INLINE)* ) *)
      fun doHead mkup = app doInline mkup

      fun chkHead(M.ELEM{elem=E.HEAD, body, ...}) = (doHead body; true)
	| chkHead _ = false

      (* % API         " STRUCTURE | FUNCTOR | SIGNATURE " *)
      fun doApi(M.ELEM{elem=E.FUNCTOR{fctid, ...}, body, ...}) = 
	    doFunctor(body, latexString fctid)
	| doApi(M.ELEM{elem=E.SIGNATURE{sigid, ...}, body, ...}) = 
	    doSignature(body, latexString sigid)
	| doApi(M.ELEM{elem=E.STRUCTURE{strid, ...}, body, ...}) =
	    doStructure(body, latexString strid)
	| doApi _ = error "doApi"

      fun chkApi(mkup as M.ELEM{elem, ...}) = 
	(case elem
	  of E.FUNCTOR _ => (doApi mkup; true)
	   | E.SIGNATURE _ => (doApi mkup; true)
	   | E.STRUCTURE _ => (doApi mkup; true)
	   | _ => false
	 (*esac*))

      (* PARA        ::= PP | FLOAT *)
      fun chkPara(mkup as M.ELEM{elem=E.PP, ...}) = (doPP mkup; true)
	| chkPara(mkup as M.ELEM{elem=E.FLOAT _, ...}) = (chkFloat mkup; true)
	| chkPara _ = false

      fun doPara mkup = (chkPara mkup; ())


      (* INTERFACE ::=  (HEAD, (SEEALSO?), (%PARA)*, (%API), (%PARA)* ) *)
      fun doInterface(label, M.ELEM{elem=E.HEAD, body, ...}::rest) = let
	fun f [] = ()
	  | f (elem::rest) = 
	     if chkSeeAlso elem orelse chkPara elem orelse chkApi elem then
	       f rest
	     else error "doInterface.f"
	fun eLabel NONE = ()
	  | eLabel (SOME label) = prf "  \\label{%s}\n" [F.STR label]
      in 
	protect(fn () => doHeading level (fn () => doHead body));
	eLabel label;
	f rest
      end

      (* SECTION ::= (HEAD, (%PARA)*, (%DOCSECT)* ) *)
      fun doSection(label, M.ELEM{elem=E.HEAD, body, ...}::rest) = let
	   fun doLabel NONE = ()
	     | doLabel (SOME lab) = prf "\\label{%s}\n" [F.STR lab]
	   fun doParas (nodes as (n::ns)) = 
	       if chkPara n then doParas ns else (docBody (level+1)) nodes
	     | doParas [] = ()
          in 
	    protect(fn () => doHeading level (fn () => doHead body));
	    doLabel label;
	    doParas rest
	  end
	| doSection _ = error "doSection"

      (* % DOCSECT ::= SECTION | INCLFILE | INTERFACE*)
      and doDocSect(M.ELEM{elem=E.SECTION{label, nonumber, notoc}, body, ...}) =
	   doSection(label, body)
	| doDocSect(M.ELEM{elem=E.INCLFILE{file}, ...}) =
	   prf "\n\\input{%s}\n" [F.STR file]
	| doDocSect(mkup as M.ELEM{elem=E.INTERFACE{label}, body, ...}) =
	   doInterface(label, body)
	| doDocSect mkup = (printMarkup [mkup]; error "doDocSect")
    in
      (* %DOCBODY ::= %DOCSECT+ *)
      app doDocSect body
    end (* docBody *)

    (* ML-DOC := %DOCHDR, (%DOCBODY) *)
    fun doMLDoc[M.ELEM{elem=E.ML_DOC, body, ...}] = 
         ((docBody level) o docHdr) body
      | doMLDoc _ = error "doMLDoc"
  in
    indexFlag := index;
    if standalone
      then (pr "\
       \\\documentclass{book}\n\
	    \\\usepackage{code}\n\
	    \\\usepackage{proofMLDoc}\n\
	    \\\begin{document}\n";
	    doMLDoc doc;
	    pr "\\end{document}\n")
      else (doMLDoc doc; ())
  end (*doFile*)
end

