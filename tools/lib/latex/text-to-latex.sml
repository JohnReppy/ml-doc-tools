(* text-to-latex.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TextToLaTeX :> sig

    type context = LaTeXContext.context

  (* set the following flag to true to include <QUESTION> elements in the
   * output (default = false).
   *)
    val includeQuestions : bool ref

    val doInline : context -> MLDocMarkup.markup -> unit
    val doCode : context -> MLDocMarkup.markup list -> unit

    val doBlock : context -> MLDocMarkup.markup -> unit
    val doPP : context -> MLDocMarkup.markup -> unit
    val doList : context -> MLDocMarkup.markup -> unit
    val doFloat : context -> MLDocFloat.float -> unit

    val doSeeAlso : context -> MLDocMarkup.markup -> unit

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup
    structure Math = MLDocMath
    structure Gram = MLDocGrammar
    structure Flt = MLDocFloat
    structure K = MLDocKind
    structure I = MLDocIndex
    structure C = LaTeXContext
    structure F = Format

    val includeQuestions = ref false

    type context = C.context
(*
    datatype context_kind
      = C_TEXT | C_HEAD | C_MATH | C_PROTO | C_CODE | C_INDEX

    datatype context = C of {
	info : static_info,
	ctx : context_kind,		(* current typesetting context *)
	bindCtx : I.binding_context	(* current binding context for free IDs *)
      }

    and static_info = Info of {
	out : TextIO.outstream,
	index : I.index,
	config : MLDocConfig.configuration,
	docCache : Document.doc_cache
      }

    fun context (strm, idx, config) = C{
	    info = Info{
		out = strm, index = idx,
		config = config,
		docCache = Document.mkCache config
	      },
	    ctx = C_TEXT,
	    bindCtx = I.TOPbound
	  }

    fun streamOf (C{info=Info{out, ...}, ...}) = out

    fun labelKind (C{info=Info{index, ...}, ...}, label) = (
	  case I.findLabel(index, Atom.atom label)
	   of NONE => Error.error'["unknown label \"", label, "\""]
	    | SOME entry => I.Label.kind entry
	  (* end case *))

    local
      fun updateCtx (C{info, bindCtx, ...}, ctx) =
	    C{info=info, ctx=ctx, bindCtx=bindCtx}
    in
    fun headingContext ctx = updateCtx (ctx, C_HEAD)
    fun codeContext ctx = updateCtx (ctx, C_CODE)
    fun protoContext ctx = updateCtx (ctx, C_PROTO)
    fun mathContext ctx = updateCtx (ctx, C_MATH)
    fun indexContext ctx = updateCtx (ctx, C_INDEX)
    end

    fun withContext (C{info as Info{index, ...}, ctx, ...}, bindCtx) = C{
	    info = info,
	    ctx = ctx,
	    bindCtx = I.canonicalContext(index, bindCtx)
	  }

    fun withSig (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val sigId = Atom.atom id
	  in
	    case I.findSig (index, sigId)
	     of (SOME _) => withContext(c, I.SIGbound[sigId])
	      | _ => raise Fail "withSig: undefined signature"
	    (* end case *)
	  end
      | withSig _ = raise Fail "withSig: not at top-level"

    fun withTopStr (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val strId = Atom.atom id
	  in
	    case I.findStr (index, strId)
	     of (SOME _) => withContext(c, I.STRbound[strId])
	      | _ => raise Fail "withTopStr: undefined structure"
	    (* end case *)
	  end
      | withTopStr _ = raise Fail "withTopStr: not at top-level"

    fun withFct (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.FCTbound[fctId])
	      | _ => raise Fail "withFct: undefined functor"
	    (* end case *)
	  end
      | withFct _ = raise Fail "withFct: not at top-level"

    fun withFctArg (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.ARGbound[fctId])
	      | _ => raise Fail "withFctArg: undefined functor"
	    (* end case *)
	  end
      | withFctArg _ = raise Fail "withFctArg: not at top-level"

    fun withSubstr (c as C{bindCtx, ...}, id) =
	  withContext (c, I.extendContext (bindCtx, Atom.atom id))

    fun withStrPath (c as C{info=Info{index, ...}, ...}, path) = let
	  val path = List.map Atom.atom path
	  in
	    case I.findContext (index, path)
	     of (SOME bindCtx) => withContext (c, bindCtx)
	      | _ =>
		  Error.error ("withSubstr: undefined structure " ^ joinPath path)
	    (* end case *)
	  end

    fun inHead (C{ctx=C_HEAD, ...}) = true
      | inHead _ = false
    fun inCode (C{ctx=C_CODE, ...}) = true
      | inCode (C{ctx=C_PROTO, ...}) = true
      | inCode _ = false
    fun pr (cxt : context, s) = TextIO.output(streamOf cxt, s)
    fun prc (cxt : context, c) = TextIO.output1(streamOf cxt, c)
    fun prl (cxt : context, items) = pr(cxt, concat items)
    fun prf (cxt : context, fmt, items) = pr(cxt, F.format fmt items)

  (* resolve cross references *)
    fun resolve (C{info=Info{index, docCache, ...}, ...}) = let
	  val resolve' = ResolveRef.resolve (index, docCache)
*)

    val pr = C.pr
    val prl = C.prl
    val prf = C.prf

  (******************** translating to LaTeX strings ********************)
    val transData = LaTeXTranslate.transData
    val transCodeData = LaTeXTranslate.transCodeData
    val transProtoData = LaTeXTranslate.transProtoData
    val transIndexView = LaTeXTranslate.transIndexView
    val translate = C.translate

  (* format a string that is being used as a LaTeX label *)
    fun fmtLabel str = F.STR str


  (******************** support for LaTeX commands ********************)

    fun withCmd cmd c (doBody : context -> unit) = (
	  prl(c, ["\\", cmd, "{"]); doBody c; pr(c, "}"))

    val emph = withCmd "emph"
    val texttt = withCmd "texttt"
    val textit = withCmd "textit"
    val textbf = withCmd "textbf"
    val text = withCmd "text"
    val texttt = withCmd "texttt"
  (* ml-doc style-specific commands *)
    val mldCD = withCmd "mldCD"
    val mldKW = withCmd "mldKW"
    val mldArg = withCmd "mldArg"
    val mldTyvar = withCmd "mldTyvar"

  (* stylized code text *)
    fun stylize (NONE, code) =
	  DefaultScanner.stylize{trim=true, code=code}
      | stylize (SOME("SML"|"sml"), code) =
	  SMLScanner.stylize{trim=true, code=code}
      | stylize (SOME("C"|"c"), code) =
	  DefaultScanner.stylize{trim=true, code=code}
      | stylize (SOME lang, _) =
	  raise Fail("unrecognized language "^String.toString lang)

  (* convert an %inline element to LaTeX *)
    fun doInline ctx elem = let
	  fun split id = String.tokens (fn #"." => true | _ => false) id
	  fun doRef (_, []) =
		Error.warning("empty cross-reference element", [])
	    | doRef (elem, content) = let
		fun compact [] = []
		  | compact (M.DATA s :: r) = s :: compact r
		  | compact _ = raise Fail "bad XREF contents"
		val id = String.concat(compact content)
		in
		  if not(C.inHead ctx)
		    then pr(ctx, LaTeXIndex.indexXRef (ctx, elem, id))
		    else ();
		  if (C.inCode ctx)
		    then pr (ctx, LaTeXIdFamily.fmt (C.dataToLaTeX ctx) id)
		    else prf (ctx, "\\texttt{%s}", [F.STR(LaTeXIdFamily.fmt C.protoString id)])
		end
	  fun doBody body ctx = List.app (doInline ctx) body
	  in
	    case elem
	     of (M.DATA data) => pr(ctx, C.dataToLaTeX ctx data)
	      | (M.ELEM{elem=E.MATH, body, ...}) => (
		  pr(ctx, "$"); doMath (ctx, false, body); pr(ctx, "$"))
	      | (M.ELEM{elem=E.RE, body, ...}) => (
		  pr(ctx, "$"); doRegExp ctx (false, body); pr(ctx, "$"))
	      | (M.ELEM{elem=E.EM, body, ...}) => emph ctx (doBody body) 
	      | (M.ELEM{elem=E.IT, body, ...}) => textit ctx (doBody body)
	      | (M.ELEM{elem=E.BF, body, ...}) => textbf ctx (doBody body)
	      | (M.ELEM{elem=E.TT, body, ...}) =>
		  texttt (C.protoContext ctx) (doBody body)
	      | (M.ELEM{elem=E.CD{lang}, body, ...}) => (
		  if C.inHead ctx then pr(ctx, "\\protect") else ();
		  texttt (C.protoContext ctx) (doBody(stylize(lang, body))))
(** NOTE: if we are not in a <CODE> element, then we should wrap that around
 ** the <B> element.
 **)
	      | (M.ELEM{elem=E.KW, body, ...}) => mldKW ctx (doBody body)
	      | (M.ELEM{elem=E.ARG, body, ...}) => let
		  fun arg ctx = mldArg ctx (doBody body)
		  in
		    if C.inCode ctx
		      then arg ctx
		      else texttt (C.protoContext ctx) arg
		  end
	      | (M.ELEM{elem as E.STRREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.FCTREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.SIGREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.TYREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.EXNREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.CONREF _, body, ...}) => doRef (elem, body)
	      | (M.ELEM{elem as E.VALREF _, body, ...}) => doRef (elem, body)
(* NOTE: we need to do something about indexing here *)
	      | (M.ELEM{elem as E.IDREF _, body, ...}) => mldCD ctx (doBody body)
(*
	      | (M.ELEM{elem as E.ID, body, ...}) => doData body
*)
	      | (M.ELEM{elem=E.INDEX{key, see, mark, which}, body, ...}) =>
		  doIndex ctx {
		      key = key, see = see, mark = mark, which = which,
		      content = body
		    }
	      | (M.ELEM{elem=E.ADEF _, body, ...}) => app (doInline ctx) body
	      | (M.ELEM{elem=E.AREF _, body, ...}) => app (doInline ctx) body
	      | (M.ELEM{elem=E.URL _, body, ...}) => app (doInline ctx) body
	      | (M.ELEM{elem=E.DOCREF _, body, ...}) => app (doInline ctx) body
	      | (M.ELEM{elem=E.SECREF{label}, ...}) => let
		  val level = C.labelLevel(ctx, label)
		  in
(* FIXME: the following test does not take into account any bias that might be
 * set by the TopLevelSection option (see latex-gen/do-file.sml).  This code
 * works for the Basis book.
 *)
		    if (level > 0)
		      then prf(ctx, "\\mldSecRef{%s}", [fmtLabel label])
		      else prf(ctx, "\\mldChapRef{%s}", [fmtLabel label])
		  end
	      | (M.ELEM{elem as E.FLOATREF{label}, ...}) => (
		  case C.labelKind(ctx, label)
		   of I.Label.Table =>
			prf(ctx, "\\mldTblRef{%s}", [fmtLabel label])
		    | I.Label.Figure =>
			prf(ctx, "\\mldFigRef{%s}", [fmtLabel label])
		    | I.Label.Code =>
			prf(ctx, "\\mldCodeRef{%s}", [fmtLabel label])
		    | _ => Error.error'[
			  "FLOATREF(", label, ") to non-float label"
			]
		  (* end case *))
	      | (M.ELEM{elem as E.CITE{key}, ...}) =>
		  prf(ctx, "\\mldCite{%s}", [fmtLabel key])
	      | _ => Error.bogusMarkup ("Inline", elem)
	    (* end case *)
	  end

    and mathToLaTeX (ctx0, isDisplay, contents) = let
	  val ctx = C.mathContext ctx0
	  fun toLaTeX (Math.Text txt) =
		text ctx0 (fn _ => List.app (doInline ctx0) txt)
	    | toLaTeX (Math.Data data) = pr(ctx, C.mathString data)
	    | toLaTeX (Math.Group l) = (
		pr (ctx, "{"); app toLaTeX l; pr(ctx, "}"))
	    | toLaTeX (Math.Sum{ll, ul, opd}) =
		raise Fail "Math.Sum"
	    | toLaTeX (Math.Prod{ll, ul, opd}) =
		raise Fail "Math.Prod"
	    | toLaTeX (Math.Union{ll, ul, opd}) =
		raise Fail "Math.Union"
	    | toLaTeX (Math.Intersect{ll, ul, opd}) =
		raise Fail "Math.Intersect"
	    | toLaTeX (Math.Frac{num, denom}) = (
		pr (ctx, "\\frac"); grpToLaTex num; grpToLaTex denom)
	    | toLaTeX (Math.Subscript(m, sub)) = (
		grpToLaTex m; pr(ctx, "_"); grpToLaTex sub)
	    | toLaTeX (Math.Superscript(m, sup)) = (
		grpToLaTex m; pr(ctx, "^"); grpToLaTex sup)
	    | toLaTeX (Math.Mod(m, sup)) = (
		grpToLaTex m; pr(ctx, "\\pmod"); grpToLaTex sup)
	    | toLaTeX (Math.Norm m) = (
		pr (ctx, "|"); toLaTeX m; pr (ctx, "|"))
	    | toLaTeX (Math.Floor m) = (
		pr (ctx, "\\lfloor{}"); toLaTeX m; pr (ctx, "\\rfloor{}"))
	    | toLaTeX (Math.Ceiling m) = (
		pr (ctx, "\\lceil{}"); toLaTeX m; pr (ctx, "\\rceil{}"))
	    | toLaTeX (Math.Set l) = (
		pr(ctx, "\\{"); app toLaTeX l; pr(ctx, "\\}"))
	  and grpToLaTex m = (pr(ctx, "{"); toLaTeX m; pr(ctx, "}"))
	  in
	    List.app toLaTeX contents
	  end

    and doMath (ctx, isDisplay, contents) =
	  mathToLaTeX (ctx, isDisplay, Math.getMathContents contents)

    and doRegExp ctx0 (isDisplay, contents) = let
	  val ctx = C.mathContext ctx0
	  fun symbol (fmt, {id=NONE, term, noindex}) =
		prf (ctx, "\\%s{%s}", [F.STR fmt, F.STR(C.mathString term)])
	    | symbol (fmt, {id=SOME i, term, ...}) =
		prf (ctx, "\\%s{%s}_{%d}", [F.STR fmt, F.STR(C.mathString term), F.INT i])
	  fun toLaTeX (Gram.NONTERM sym) = symbol ("mldreNTerm", sym)
	    | toLaTeX (Gram.TERM sym) = symbol ("mldreTerm", sym)
	    | toLaTeX (Gram.KW sym) =
		prf (ctx, "\\mldreKW{%s}", [F.STR(C.protoString sym)])
	    | toLaTeX (Gram.LIT l) =
		prf (ctx, "\\mldreLit{%s}", [F.STR(C.protoString l)])
	    | toLaTeX (Gram.CSET cs) = let
		fun f (Gram.CHARS l) =
		      prf (ctx, "\\mldreLit{%s}", [F.STR(C.protoString l)])
		  | f (Gram.RANGE(l1, l2)) =
		      prf (ctx, "\\mldreLit{%s}{-}\\mldreLit{%s}",
			[F.STR(C.protoString l1), F.STR(C.protoString l2)])
		in
		  pr (ctx, "[");
		  List.app f cs;
		  pr (ctx, "]")
		end
	    | toLaTeX (Gram.GRP elements) = listToLaTeX elements
	    | toLaTeX (Gram.OPT elem) = closure ("?", {elem=elem, sep=[]})
	    | toLaTeX (Gram.STAR clos) = closure ("*", clos)
	    | toLaTeX (Gram.PLUS clos) = closure ("+", clos)
	    | toLaTeX (Gram.ALT elems) = let
		fun f [] = ()
		  | f [elem] = toLaTeX elem
		  | f (elem :: r) = (
		      toLaTeX elem; pr(ctx, "\\mid{}"); f r)
		in
		  f elems
		end
	  and closure (rator, {elem, sep=[]}) = let
		val needsParens = (case elem
		       of (Gram.ALT _) => true
			| (Gram.LIT l) => (size l > 1)
			| (Gram.GRP(_::_::_)) => true
			| _ => false
		      (* end case *))
		in
		  if needsParens
		    then paren elem
		    else (pr (ctx, "{"); toLaTeX elem; pr (ctx, "}"));
		  prf(ctx, "^{%s}", [F.STR rator])
		end
	  and listToLaTeX [elem] = toLaTeX elem
	    | listToLaTeX l = let
		fun toLaTeX' (elem as Gram.ALT _) = paren elem
		  | toLaTeX' elem = toLaTeX elem
		in
		  List.app toLaTeX' l
		end
	  and paren elem = (pr (ctx, "("); toLaTeX elem; pr (ctx, ")"))
	  in
	    listToLaTeX (Gram.getREContents contents)
	  end

    and doIndex ctx0 arg = let
	  val ctx = C.indexContext ctx0
	  in
	    LaTeXIndex.index {inline = doInline ctx, pr = fn s => pr(ctx0, s)} arg
	  end

    fun doCode ctx l = app (doInline (C.codeContext ctx)) l

    fun doList ctx (elem : M.markup) = let
	  fun prList (kind, doItem) (body : M.markup list) = let
		fun f [] = ()
		  | f (item::r) = let val r = doItem (item, r) in f r end
		in
		  prf (ctx, "\\begin{%s}%%\n", [F.STR kind]);
		  f body;
		  prf (ctx, "\\end{%s}%%\n", [F.STR kind])
		end
	  fun doItem (M.ELEM{elem=E.ITEM, body, ...}, r) = (app (doPP ctx) body; r)
	  fun doLItem (item, r) = (pr(ctx, "\\item\n"); doItem(item, r))
	  fun doDItem (M.ELEM{elem=E.DTAG, body, ...}, item::r) = (
		pr(ctx, "\\item[");
		app (doInline (C.headingContext ctx)) body;
		pr(ctx, "]\n");
		doItem(item, r))
	  in
	    case elem
	     of (M.ELEM{elem=E.ITEMIZE, body, ...}) =>
		  prList ("itemize", doLItem) body
	      | (M.ELEM{elem=E.ENUM, body, ...}) =>
		  prList ("enumerate", doLItem) body
	      | (M.ELEM{elem=E.DESCRIP, body, ...}) =>
		  prList ("description", doDItem) body
	      | _ => Error.bogusMarkup("%LIST", elem)
	    (* end case *)
	  end

    and doBlock ctx elem = let
	  fun blockEnv (name, body) = (
		prf (ctx, "\\begin{%s}%%\n", [F.STR name]);
		app (doPP ctx) body;
		prf (ctx, "\\end{%s}%%\n", [F.STR name]))
	  in
	    case elem
	     of (M.ELEM{elem=E.DISPLAYMATH, body, ...}) => (
		  pr (ctx, "\\begin{displaymath}\n");
		  doMath (ctx, true, body);
		  pr (ctx, "\n\\end{displaymath}%\n"))
	      | (M.ELEM{elem=E.EQNARRAY, body, ...}) => let
		  fun doEqn (lhs, rel, rhs) = (
			pr (ctx, "  ");
			mathToLaTeX (ctx, false, lhs);
			pr (ctx, " & ");
			mathToLaTeX (ctx, false, rel);
			pr (ctx, " & ");
			mathToLaTeX (ctx, false, rhs);
			pr (ctx, " \\\\\n"))
		  in
		    pr (ctx, "\\begin{eqnarray*}\n");
		    List.app doEqn (Math.getEqnarrayContents body);
		    pr (ctx, "\\end{eqnarray*}%\n")
		  end
	      | (M.ELEM{elem=E.FLOAT _, ...}) => raise Fail "FLOAT"
	      | (M.ELEM{elem=E.TABLE _, ...}) =>
		  doTable ctx (Flt.getTableContents elem)
	      | (M.ELEM{elem=E.EXAMPLE, body, ...}) =>
		  blockEnv ("mldExample", body)
	      | (M.ELEM{elem=E.QUESTION, body, ...}) =>
		  if (!includeQuestions)
		    then blockEnv ("mldQuestion", body)
		    else ()
	      | (M.ELEM{elem=E.IMPLNOTE, body, ...}) =>
		  blockEnv ("mldImplNote", body)
	      | (M.ELEM{elem=E.SYSNOTE{opsys=[], arch=[]}, ...}) =>
		  Error.error "SYSNOTE element with empty attributes"
	      | (M.ELEM{elem=E.SYSNOTE{opsys, arch}, body, ...}) => let
		  fun join [x] = [x]
		    | join (x::r) = x :: ", " :: join r
		  in
		    prf (ctx, "\\begin{mldSysNote}{%s}\n",
		      [F.STR(concat(join(opsys @ arch)))]);
		    app (doPP ctx) body;
		    pr (ctx, "\\end{mldSysNote}%\n")
		  end
	      | (M.ELEM{elem=E.RATIONALE, body, ...}) =>
		  blockEnv ("mldRationale", body)
(** need to modify the translation for PRE context **)
	      | (M.ELEM{elem=E.CODE{lang}, body, ...}) => (
		  pr (ctx, "\\begin{mldCenterCode}\n");
		  doCode ctx (stylize(lang, body))
handle ex => (
print "TextToLaTeX.doBlock:\n";
PrintMLDoc.print {say = print, flush = fn _ => TextIO.flushOut TextIO.stdOut} [elem];
print "*** after stylization ***\n";
PrintMLDoc.print {say = print, flush = fn _ => TextIO.flushOut TextIO.stdOut} (stylize(lang, body));
print "*****\n";
raise ex);
		  pr (ctx, "\\end{mldCenterCode}%\n"))
	      | (M.ELEM{elem=E.GRAMMAR, body, ...}) => raise Fail "GRAMMAR"
	      | (M.ELEM{elem=E.REGEXP, body, ...}) => (
		  pr (ctx, "\\begin{displaymath}\n");
		  doRegExp ctx (true, body);
		  pr (ctx, "\n\\end{displaymath}%\n"))
	      | _ => Error.bogusMarkup("%BLOCK", elem)
	    (* end case *)
	  end

    and doTable ctx {cols, rows, long, small} = let
	  fun alignment Flt.LEFT = "l"
	    | alignment Flt.RIGHT = "r"
	    | alignment Flt.CENTER = "c"
	    | alignment Flt.DEFAULT = "l"
	    | alignment (Flt.PARBOX s) = concat["p{", s, "}"]
	  val colAligns = List.map alignment cols
	  fun doRow cells = let
		fun multi ((NONE|SOME 1), Flt.DEFAULT, _) = NONE
		  | multi (SOME colSpan, Flt.DEFAULT, colAlign) =
		      SOME(colSpan, colAlign)
		  | multi ((NONE|SOME 1), align, colAlign) = let
		      val align = alignment align
		      in
			if (align <> colAlign)
			  then SOME(1, align)
			  else NONE
		      end
		  | multi (SOME colSpan, align, _) = SOME(colSpan, alignment align)
		fun trans (last, {colspan, align, contents}, colAlign) = let
		      val nCols = (case multi (colspan, align, colAlign)
			     of NONE => (app (doInline ctx) contents; 1)
			      | SOME(colSpan, align) => (
				  prf (ctx, "\\multicolumn{%d}{%s}{",
				    [F.INT colSpan, F.STR align]);
				  app (doInline ctx) contents;
				  pr (ctx, "}");
				  colSpan)
			    (* end case *))
		      in
			if last then pr(ctx, " \\\\\n") else pr(ctx, " &\n");
			nCols
		      end
		fun transCell (last, Flt.TH arg, colAlign) =
		      trans(last, arg, colAlign)
		  | transCell (last, Flt.TD arg, colAlign) =
		      trans(last, arg, colAlign)
		fun doCells ([cell], [colAlign]) =
		      ignore (transCell(true, cell, colAlign))
		  | doCells (cell::r1, colAlign::r2) = let
		      val nCols = transCell(false, cell, colAlign)
		      in
			doCells (r1, List.drop(r2, nCols-1))
		      end
		in
		  doCells (cells, colAligns)
		end
	(* split the list of rows into the table header and body *)
	  val (hdr, rows) = let
		fun isHeader row = List.exists (fn (Flt.TH _) => true | _ => false) row
		fun getHdr (row::r, hdr) =
		      if isHeader row
			then getHdr(r, row::hdr)
			else (List.rev hdr, row::r)
		  | getHdr ([], hdr) = raise Fail "empty table"
		in
		  getHdr(rows, [])
		end
	  fun prTable kind = (
		prf (ctx, "\\begin{%s}{%s}\n", [F.STR kind, F.STR(concat colAligns)]);
		List.app doRow hdr;
		if List.null hdr then () else pr(ctx, "\\mldEndHead\n");
		List.app doRow rows;
		prf (ctx, "\\end{%s}%%\n", [F.STR kind]))
	  in
	    if long
	      then prTable "mldLongTable"
	      else (
		pr (ctx, "\\begin{center}\n");
		if small then pr(ctx, "\\small\n") else ();
		prTable "mldTabular";
		pr (ctx, "\\end{center}%\n"))
	  end

    and doFloat ctx float = let
	  fun doCaption [] = ()
	    | doCaption l = (
		pr (ctx, "\\caption{");
		app (doInline ctx) l;
		pr (ctx, "}\n"))
	  fun doLabel lab = prf (ctx, "\\label{%s}\n", [F.STR lab])
	  in
	    case float
	     of Flt.FIGURE _ => raise Fail "doFloat: figure"
	      | Flt.TABLE{long, small, label, caption, capAlign, cols, rows} => let
(* FIXME: this test is a hack to deal with tables in the Basis book; we probably
 * need an attribute on the float to specify placement.
 *)
		  val tblEnv = if (List.length rows > 35)
			then [F.STR "mldTablePage"]
			else [F.STR "mldTable"]
		  in
		    if long
		      then raise Fail "floating longtable not supported"
		      else ();
		    prf (ctx, "\\begin{%s}\n", tblEnv);
		    doCaption caption;
		    doLabel label;
		    doTable ctx {long=long, small=small, cols=cols, rows=rows};
		    prf (ctx, "\\end{%s}%%\n", tblEnv)
		  end
	      | Flt.CODE{label, caption, body, ...} => let
		  val M.ELEM{elem=E.CODE{lang}, body, ...} = body
		  val body = stylize(lang, body)
		(* count the number of lines in the code; we are relying on the
		 * fact that the stylized code has newlines as independent elements.
		 *)
		  val nlines = let
			fun countLines (n, []) = n
			  | countLines (n, [_]) = n
			  | countLines (n, M.DATA "\n" :: r) = countLines(n+1, r)
			  | countLines (n, _ :: r) = countLines (n, r)
			in
			  countLines (1, body)
			end
		  val listingEnv = if (nlines > 35)
			then [F.STR "mldListingPage"]
			else [F.STR "mldListing"]
		  in
		    prf (ctx, "\\begin{%s}\n", listingEnv);
		    pr (ctx, "\\begin{mldCenterCode}\n");
		    doCode ctx (stylize(lang, body));
		    pr (ctx, "\\end{mldCenterCode}%\n");
		    doCaption caption;
		    doLabel label;
		    prf (ctx, "\\end{%s}%%\n", listingEnv)
		  end
	    (* end case *)
	  end

  (* map a list of ML-Doc paragraphs to LaTeX.  An ML-Doc paragraph
   * may contain lists and blocks, as well as inline text.  We map this to a
   * list of blocks; if the first consists of inline text, then it is treated
   * as a paragraph.  Subsequent blocks of text are not tagged.
   *)
    and doPP ctx (elem as M.ELEM{elem=E.PP, body, ...}) = let
	  fun doElems (true, []) = ()
	    | doElems (false, []) = pr (ctx, "\n")
	    | doElems (prevNL, elem::r) = (case (K.kind elem)
		 of K.LIST => (
		      if prevNL then () else pr (ctx, "\n");
		      doList ctx elem;
		      doElems (true, r))
		  | K.BLOCK => (
		      if prevNL then () else pr (ctx, "\n");
		      doBlock ctx elem;
		      doElems (true, r))
		  | K.FLOAT => raise Fail "FLOAT"
		  | K.OTHER => (
		      doInline ctx elem;
		      doElems (false, r))
		(* end case *))
	  in
	    pr (ctx, "\n");
	    doElems (true, TrimWS.trim body)
	  end
      | doPP ctx elem = Error.bogusMarkup("PP", elem)

  (* format a single item in a SEEALSO list *)
    fun doSeeAlso ctx (M.ELEM{elem, body=[M.DATA id], ...}) = let
(* What about symbolic ids? *)
	  val id' = LaTeXTranslate.transLabelData id
	  fun mkLabel (SOME _, _) = NONE
	    | mkLabel (NONE, kind) =
		SOME(F.format "sec:%s:%s" [F.STR id', F.STR kind])
	  val lab = (case elem
		 of E.SIGREF{noindex, document, ...} =>
		      mkLabel(document, "sig")
		  | E.FCTREF{noindex, document, ...} =>
		      mkLabel(document, "fct")
		  | E.STRREF{noindex, document, home, ...} =>
		      mkLabel(document, "str")
		  | E.EXNREF{noindex, document, home, ...} =>
		      mkLabel(document, "exn")
		  | E.TYREF{noindex, document, home, ...}  =>
		      mkLabel(document, "ty")
		  | E.CONREF{noindex, document, home, ...} =>
		      mkLabel(document, "con")
		  | E.VALREF{noindex, document, home, ...} =>
		      mkLabel(document, "val")
		  | E.IDREF{kind, noindex} => NONE	(* ??? *)
		  | _ => Error.bogusElem("%IDREF", elem)
		(* end case *))
	  in
	    case lab
	     of NONE => prf(ctx, "%s", [F.STR(transProtoData id)])
	      | SOME lab => prf (ctx, "\\mldSee{%s}{%s}",
		  [F.STR(transProtoData id), F.STR lab])
	    (* end case *)
	  end
      | doSeeAlso ctx elem = Error.bogusMarkup("SEEALSO", elem)

  end
