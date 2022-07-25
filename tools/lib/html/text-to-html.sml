(* text-to-html.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TextToHTML : sig

    val transData : string -> string
    val transPreData : string -> string

    val inlineToHTML : HTMLContext.context -> MLDocMarkup.markup -> HTML.text

    val ppToHTML : HTMLContext.context -> MLDocMarkup.markup list -> HTML.block list
	(* convert a list of ML-DOC PP elements to the corresponding list
	 * of HTML blocks.
	 *)

    val stripLeadingPara : HTML.block list -> HTML.block list
	(* if the first element of a list of blocks is a <P>, then convert
	 * it to a TextBlock.  This is sometimes necessary to avoid excessive
	 * newlines.
	 *)

    val listToHTML : HTMLContext.context -> MLDocMarkup.markup -> HTML.block

    val blockToHTML : HTMLContext.context -> MLDocMarkup.markup -> HTML.block

    val codeToHTML : HTMLContext.context -> MLDocMarkup.markup list -> HTML.text
	(* convert "%code" markup to HTML text *)

    val floatToHTML : HTMLContext.context -> MLDocFloat.float -> HTML.block

    val idToHTML : string -> HTML.text
	(* convert identifiers, which are PCDATA elements, to HTML text *)

    val dataToHTML : string -> HTML.text
	(* convert PCDATA elements to HTML text *)

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup
    structure C = HTMLContext
    structure I = MLDocIndex
    structure H = HRefs
    structure Mk = MakeHTML
    structure SS = Substring
    structure Math = MLDocMath
    structure Gram = MLDocGrammar
    structure Flt = MLDocFloat
    structure K = MLDocKind

    fun special #"<" = SOME "&lt;"
      | special #">" = SOME "&gt;"
      | special #"&" = SOME "&amp;"
      | special #"\"" = SOME "&quot;"
      | special _ = NONE

    val transData = let
	  fun escape #"\\" = "\\"
	    | escape c = if (Char.isPrint c) then String.str c else " "
	  in
	    M.transData {
		escape = escape,
		sdata = EntitiesToHTML.transEntity,
		special = special
	      }
	  end

    val transId = let
	  fun escape #"\\" = "\\"
	    | escape c = if (Char.isPrint c) then String.str c else " "
	(* extend the special characters with ID family index brackets *)
	  fun special' #"{" = SOME "<I>&lt;"
	    | special' #"}" = SOME "&gt;</I>"
	    | special' c = special c
	  in
	    M.stripData {
		escape = escape,
		sdata = EntitiesToHTML.transEntity,
		special = special'
	      }
	  end

    val transPreData = let
	  fun escape #"\\" = "\\"
	    | escape #"\n" = "\n"
	    | escape c = if (Char.isPrint c) then String.str c else " "
	  in
	    M.transData {
		escape = escape,
		sdata = EntitiesToHTML.transEntity,
		special = special
	      }
	  end

    fun idToHTML data = HTML.PCDATA(transId data)

    fun dataToHTML data = HTML.PCDATA(transData data)

    fun dataToHTML' ctx data = if (HTMLContext.inCode ctx)
	  then HTML.PCDATA(transPreData data)
	  else HTML.PCDATA(transData data)

  (* map a section reference label to its filename and section heading *)
    fun findSect (ctx, label) = (case C.findLabel(ctx, label)
	   of NONE => NONE
	    | SOME entry => let
		val fileEntry = I.Label.file entry
		val key = I.Label.name entry
		val fileName = Atom.toString(I.File.name fileEntry)
		fun find [] = NONE
		  | find (I.File.INCLFILE _ :: r) = find r
		  | find (I.File.SECTION{title, label=NONE, content} :: r) = (
		      case find content
		       of NONE => find r
			| something => something
		      (* end case *))
		  | find (I.File.SECTION{title, label=SOME lab, content} :: r) =
		      if Atom.sameAtom(lab, key)
			then SOME{file=fileName, head=title}
			else (case find content
			   of NONE => find r
			    | something => something
			  (* end case *))
		in
		  find (I.File.content fileEntry)
		end
	  (* end case *))

  (* convert an %inline element to a single HTML element *)
    fun inlineToHTML ctx elem = let
	  fun doData [M.DATA data] = dataToHTML' ctx data
	    | doData [] = raise Fail "empty PCDATA"
	    | doData _ = raise Fail "bad PCDATA"
	  fun doRef (elem, [M.DATA id]) =
		H.xrefToHTML ctx {xref=elem, id=id} (idToHTML id)
	    | doRef _ = raise Fail "bad XREF contents"
	  fun doCodeRef (elem, body) =
		if (HTMLContext.inCode ctx)
		  then doRef (elem, body)
		  else HTML.CODE(doRef (elem, body))
	  fun emph style body = style(Mk.textList(map (inlineToHTML ctx) body))
	  in
	    case elem
	     of (M.DATA data) => dataToHTML' ctx data
	      | (M.ELEM{elem=E.MATH, body, ...}) => doMath (ctx, false, body)
	      | (M.ELEM{elem=E.RE, body, ...}) => regexpToHTML ctx (false, body)
	      | (M.ELEM{elem=E.EM, body, ...}) => emph HTML.EM body
	      | (M.ELEM{elem=E.IT, body, ...}) => emph HTML.I body
	      | (M.ELEM{elem=E.BF, body, ...}) => emph HTML.B body
	      | (M.ELEM{elem=E.TT, body, ...}) => emph HTML.TT body
(* FIXME! need to stylize body here *)
	      | (M.ELEM{elem=E.CD _, body, ...}) => emph HTML.CODE body
	      | (M.ELEM{elem=E.KW, body, ...}) =>
		  if (HTMLContext.inCode ctx)
		    then emph HTML.B body
		    else HTML.CODE(inlineToHTML (HTMLContext.codeCtx ctx) elem)
	      | (M.ELEM{elem=E.ARG, body, ...}) => HTML.VAR(doData body)
	      | (M.ELEM{elem as E.STRREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.FCTREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.SIGREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.TYREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.EXNREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.CONREF _, body, ...}) => doCodeRef(elem, body)
	      | (M.ELEM{elem as E.VALREF _, body, ...}) => doCodeRef(elem, body)
(* FIXME! need to generate a link here *)
	      | (M.ELEM{elem as E.IDREF _, body, ...}) => emph HTML.CODE body
	      | (M.ELEM{elem as E.ID, body, ...}) => doData body
	      | (M.ELEM{elem=E.ADEF{tag}, body, ...}) =>
		  Mk.textList(H.mkAnchor tag :: (map (inlineToHTML ctx) body))
	      | (M.ELEM{elem as E.AREF{tag, ...}, body, ...}) =>
		  H.xrefToHTML ctx {xref=elem, id=tag}
		    (Mk.textList(map (inlineToHTML ctx) body))
	      | (M.ELEM{elem as E.URL{href}, body, ...}) =>
		  H.mkHRef href (Mk.textList(map (inlineToHTML ctx) body))
	      | (M.ELEM{elem as E.DOCREF{document}, body, ...}) => (
		  Error.warning ("DOCREF not yet supported", []);
		  Mk.textList(map (inlineToHTML ctx) body))
	      | (M.ELEM{elem as E.SECREF{label}, ...}) => (
		  case findSect (ctx, label)
		   of NONE => (
			Error.warning ("SECREF \"%s\" not resolved",
			  [Format.STR label]);
			HTML.B(HTML.PCDATA(
			  concat["[SECREF ", label, "]"])))
		    | SOME{file, head} =>
			H.mkSectRef (file, SOME label, 0,
			  HTML.B(HTML.PCDATA(
			    concat["[", transData head, "]"])))
		  (* end case *))
	      | (M.ELEM{elem as E.FLOATREF{label}, ...}) => (
		  Error.warning ("FLOATREF not yet supported", []);
		  HTML.B(HTML.PCDATA "[FLOAT]"))
	      | (M.ELEM{elem as E.CITE{key}, ...}) => (
		  Error.warning ("CITE not yet supported", []);
		  HTML.B(HTML.PCDATA "[CITE]"))
	      | (M.ELEM{elem=E.INDEX _, ...}) => HTML.PCDATA ""
	      | _ => Error.bogusMarkup ("Inline", elem)
	    (* end case *)
	  end

    and mathToHTML (ctx, isDisplay, contents : Math.math_markup list) = let
	  fun toHTML (Math.Text txt) = List.map (inlineToHTML ctx) txt
	    | toHTML (Math.Data data) = [dataToHTML' ctx data]
	    | toHTML (Math.Group l) =
		(HTML.PCDATA "(") :: (listToHTML l) @ [HTML.PCDATA ")"]
	    | toHTML (Math.Sum{ll, ul, opd}) =
		raise Fail "Math.Sum"
	    | toHTML (Math.Prod{ll, ul, opd}) =
		raise Fail "Math.Prod"
	    | toHTML (Math.Union{ll, ul, opd}) =
		raise Fail "Math.Union"
	    | toHTML (Math.Intersect{ll, ul, opd}) =
		raise Fail "Math.Intersect"
	    | toHTML (Math.Frac{num, denom}) =
		(toHTML num) @ (HTML.PCDATA " / " :: toHTML denom)
	    | toHTML (Math.Subscript(m, sub)) =
		(toHTML m) @ [HTML.SUB(Mk.textList(toHTML sub))]
	    | toHTML (Math.Superscript(m, sup)) =
		(toHTML m) @ [HTML.SUP(Mk.textList(toHTML sup))]
	    | toHTML (Math.Mod(m1, m2)) =
		(toHTML m1) @ (HTML.PCDATA "(" :: HTML.B(HTML.PCDATA "mod")
		  :: HTML.PCDATA "&nbsp;" :: (toHTML m2 @ [HTML.PCDATA ")"]))
	    | toHTML (Math.Norm m) =
		(HTML.PCDATA "|") :: (toHTML m) @ [HTML.PCDATA "|"]
	    | toHTML (Math.Ceiling m) =
		HTML.B(HTML.PCDATA "ceil") :: (HTML.PCDATA "(")
		  :: (toHTML m) @ [HTML.PCDATA ")"]
	    | toHTML (Math.Floor m) =
		HTML.B(HTML.PCDATA "floor") :: (HTML.PCDATA "(")
		  :: (toHTML m) @ [HTML.PCDATA ")"]
	    | toHTML (Math.Set l) =
		(HTML.PCDATA "{") :: (listToHTML l) @ [HTML.PCDATA "}"]
	  and listToHTML ml = List.foldr (fn (m, l) => (toHTML m) @ l) [] ml
	  in
	    Mk.textList (listToHTML contents)
	  end

    and doMath (ctx, isDisplay, contents) = mathToHTML (ctx, isDisplay, Math.getMathContents contents)

    and regexpToHTML ctx (isDisplay, contents) = let
	  fun emph style s = style(dataToHTML' ctx s)
	  fun symbol {id=NONE, term, noindex} =
		[dataToHTML' ctx term]
	    | symbol {id=SOME i, term, ...} =
		[dataToHTML' ctx term, HTML.SUB(HTML.PCDATA(Int.toString i))]
	(* return true if the object of a closure operator requires parens *)
	  fun needsParens (Gram.ALT _) = true
	    | needsParens (Gram.LIT l) = (size l > 1)
	    | needsParens _ = false
	  fun toHTML (Gram.NONTERM sym) = [HTML.I(Mk.textList(symbol sym))]
	    | toHTML (Gram.TERM sym) = symbol sym
	    | toHTML (Gram.KW sym) = [HTML.CODE(emph HTML.B sym)]
	    | toHTML (Gram.LIT l) = [emph HTML.TT l]
	    | toHTML (Gram.CSET cs) = let
		fun f (Gram.CHARS cs, l) = emph HTML.TT cs :: l
		  | f (Gram.RANGE(c1, c2), l) =
		      (emph HTML.TT c1) :: HTML.PCDATA "-":: (emph HTML.TT c2) :: l
		in
		  HTML.PCDATA "[" :: (List.foldr f [HTML.PCDATA "]"] cs)
		end
	    | toHTML (Gram.GRP elements) = listToHTML elements
	    | toHTML (Gram.OPT elem) = closure ("?", {elem=elem, sep=[]})
	    | toHTML (Gram.STAR clos) = closure ("*", clos)
	    | toHTML (Gram.PLUS clos) = closure ("+", clos)
	    | toHTML (Gram.ALT elems) = let
		fun f [] = []
		  | f [elem] = toHTML elem
		  | f (elem :: r) = toHTML elem @ (HTML.PCDATA "&nbsp;|&nbsp;" :: f r)
		in
		  f elems
		end
	  and closure (rator, {elem, sep=[]}) = let
		val elem = if (needsParens elem)
		      then paren elem
		      else toHTML elem
		in
		  elem @ [HTML.SUP(HTML.PCDATA rator)]
		end
	  and listToHTML [elem] = toHTML elem
	    | listToHTML ml = let
		fun toHTML' (elem as Gram.ALT _, l) = paren elem @ l
		  | toHTML' (elem, l) = toHTML elem @ l
		in
		  List.foldr toHTML' [] ml
		end
	  and paren elem = HTML.PCDATA "(" :: (toHTML elem @ [HTML.PCDATA ")"])
	  in
	    Mk.textList (listToHTML (Gram.getREContents contents))
	  end

    fun codeToHTML ctx l =
	  Mk.textList (List.map (inlineToHTML (C.codeCtx ctx)) l)

    fun stripLeadingPara (HTML.P{content, align=NONE} :: r) =
	  (HTML.TextBlock content)::r
      | stripLeadingPara bl = bl

    fun listToHTML ctx (elem : MLDocMarkup.markup) = let
	  fun getList (getItem, cons) body = let
		fun get ([], l) = cons(rev l)
		  | get (item::r, l) = let
		      val (item, r) = getItem(item, r)
		      in
			get (r, item::l)
		      end
		in
		  get (body, [])
		end
	  fun getItem (M.ELEM{elem=E.ITEM, body, ...}) =
		Mk.blockList(stripLeadingPara(ppToHTML ctx body))
	  fun getLItem (item, r) = (Mk.mkLI(getItem item), r)
	  fun getDItem (M.ELEM{elem=E.DTAG, body, ...}, item::r) = let
		val item = getItem item
		in
		  ({dt = [Mk.textList(map (inlineToHTML ctx) body)], dd = item}, r)
		end
	  in
	    case elem
	     of (M.ELEM{elem=E.ITEMIZE, body, ...}) =>
		  getList (getLItem, Mk.mkUL) body
	      | (M.ELEM{elem=E.ENUM, body, ...}) =>
		  getList (getLItem, Mk.mkOL) body
	      | (M.ELEM{elem=E.DESCRIP, body, ...}) =>
		  getList (getDItem, Mk.mkDL) body
	      | _ => Error.bogusMarkup("%LIST", elem)
	    (* end case *)
	  end

    and blockToHTML ctx elem = let
	  fun blockQ (label, body) = HTML.BLOCKQUOTE(
		HTML.BlockList(
		  HTML.TextBlock(HTML.TextList[HTML.B(HTML.PCDATA label), Mk.BR])
		    :: ppToHTML ctx body))
	  in
	    case elem
	     of (M.ELEM{elem=E.DISPLAYMATH, body, ...}) =>
		  HTML.BLOCKQUOTE(HTML.TextBlock(doMath (ctx, true, body)))
	      | (M.ELEM{elem=E.EQNARRAY, body, ...}) => let
		  fun cell (align, content) = HTML.TD{
			  nowrap = false,
			  rowspan = NONE, colspan = NONE,
			  align = SOME align,
			  valign = NONE,
			  height = NONE, width = NONE,
			  content = HTML.TextBlock(mathToHTML(ctx, false, content))
			}
		  fun transEqn (lhs, rel, rhs) = HTML.TR{
			  align = NONE, valign = NONE,
			  content = [
			      cell (HTML.HAlign.right, lhs),
			      cell (HTML.HAlign.center, rel),
			      cell (HTML.HAlign.left, rhs)
			    ]
			}
		  in
		    HTML.BLOCKQUOTE(
		      HTML.TABLE{
			  align = SOME HTML.HAlign.left,
			  width = NONE,
			  border = NONE,
			  cellspacing = NONE, cellpadding = NONE,
			  caption = NONE,
			  content = List.map transEqn (Math.getEqnarrayContents body)
	        	})
		  end
	      | (M.ELEM{elem=E.FLOAT _, ...}) => raise Fail "FLOAT"
	      | (M.ELEM{elem=E.TABLE _, ...}) =>
		  tableToHTML ctx ([], Flt.getTableContents elem)
	      | (M.ELEM{elem=E.EXAMPLE, body, ...}) => blockQ ("Example:", body)
	      | (M.ELEM{elem=E.QUESTION, body, ...}) => blockQ ("Question:", body)
	      | (M.ELEM{elem=E.IMPLNOTE, body, ...}) =>
		  blockQ ("Implementation note:", body)
	      | (M.ELEM{elem=E.SYSNOTE{opsys=[], arch=[]}, ...}) =>
		  Error.error "SYSNOTE element with empty attributes"
	      | (M.ELEM{elem=E.SYSNOTE{opsys, arch}, body, ...}) => let
		  fun join [x] = [x, "]:"]
		    | join (x::r) = x :: "," :: join r
		  val label = concat("System note [" :: join(opsys @ arch))
		  in
		    blockQ (label,  body)
		  end
	      | (M.ELEM{elem=E.RATIONALE, body, ...}) => blockQ ("Rationale:", body)
	      | (M.ELEM{elem=E.CODE{lang}, body, ...}) =>
		  codeBlockToHTML (ctx, body, lang)
	      | (M.ELEM{elem=E.GRAMMAR, body, ...}) => raise Fail "GRAMMAR"
	      | (M.ELEM{elem=E.REGEXP, body, ...}) =>
		  HTML.BLOCKQUOTE(HTML.TextBlock(regexpToHTML ctx (true, body)))
	      | _ => Error.bogusMarkup("%BLOCK", elem)
	    (* end case *)
	  end

(** FIXME: need to modify the translation for PRE context **)
    and codeBlockToHTML (ctx, body, lang) = HTML.PRE{
	    width=NONE, content=codeToHTML ctx body
	  }

    and tableToHTML ctx (caption, {cols, rows, long, small}) = let
	  fun textListToHTML l = Mk.textList(map (inlineToHTML ctx) l)
	  val caption = (case caption
		 of [] => NONE
		  | l => SOME(HTML.CAPTION{
			  align = SOME HTML.CaptionAlign.top,
			  content = HTML.B(textListToHTML l)
			})
		(* end case *))
	  fun alignment Flt.LEFT = SOME HTML.HAlign.left
	    | alignment Flt.RIGHT = SOME HTML.HAlign.right
	    | alignment Flt.CENTER = SOME HTML.HAlign.center
(* FIXME: we should probably try to fix the width in this case *)
	    | alignment (Flt.PARBOX _) = SOME HTML.HAlign.left
	    | alignment Flt.DEFAULT = NONE
	  val colAligns = List.map alignment cols
	  fun transRow cells = let
		fun nCols (Flt.TH{colspan = SOME n, ...}) = n
		  | nCols (Flt.TD{colspan = SOME n, ...}) = n
		  | nCols _ = 1
		fun trans ({colspan, align, contents}, colAlign) = {
			nowrap = false,
			rowspan = NONE,
			colspan = colspan,
			align = (case (alignment align)
			   of NONE => colAlign
			    | someAlign => someAlign
			  (* end case *)),
			valign = SOME HTML.CellVAlign.top,
			height = NONE, width = NONE,
			content = HTML.TextBlock(textListToHTML contents)
		      }
		fun transCell (Flt.TH arg, colAlign) = HTML.TH(trans(arg, colAlign))
		  | transCell (Flt.TD arg, colAlign) = HTML.TD(trans(arg, colAlign))
		fun doCells (cell::r1, colAlign::r2) = let
		      val cell' = transCell (cell, colAlign)
		      val cells = doCells(r1, List.drop(r2, nCols cell - 1))
		      in
			cell' :: cells
		      end
		in
		  HTML.TR{
		      align = NONE, valign = NONE,
		      content = ListPair.map transCell (cells, colAligns)
		    }
		end
	  in
	    HTML.BlockList[
		MakeHTML.HR,
(* FIXME: if small=true, the we should shrink the font here *)
		HTML.CENTER(HTML.TABLE{
		    align = SOME HTML.HAlign.center,
		    width = NONE,
		    border = NONE,
		    cellspacing = NONE, cellpadding = NONE,
		    caption = caption,
		    content = List.map transRow rows
	          }),
		MakeHTML.HR
	      ]
	  end

    and floatToHTML ctx flt = (case flt
	   of Flt.FIGURE _ => raise Fail "floatToHTML:figure"
	    | Flt.TABLE{long, small, caption, label, capAlign, cols, rows} =>
		tableToHTML ctx
		  (caption, {cols=cols, rows=rows, long=long, small=small})
(* FIXME: what about the caption? *)
	    | Flt.CODE{
		  label, caption, capAlign,
		  body=M.ELEM{elem=E.CODE{lang}, body, ...}
		} => codeBlockToHTML (ctx, body, lang)
	  (* end case *))

  (* map a list of ML-Doc paragraphs to HTML.  An ML-Doc paragraph
   * may contain lists and blocks, as well as inline text.  We map this to a
   * list of blocks; if the first consists of inline text, then it is tagged
   * as a <P> element.  Subsequent blocks of text are not tagged.
   *)
    and ppToHTML ctx l = let
	  val inlineToHTML = inlineToHTML ctx
	  val listToHTML = listToHTML ctx
	  val blockToHTML = blockToHTML ctx
	  fun pp (tl, []) = [Mk.mkP(Mk.textList(rev tl))]
	    | pp (tl, blocks) = HTML.TextBlock(Mk.textList(rev tl)) :: blocks
	  fun doText (elem::r, text, blocks) = (case (K.kind elem, text)
		 of (K.OTHER, _) => doText(r, inlineToHTML elem :: text, blocks)
		  | (_, []) => doBlock(elem::r, blocks)
		  | (_, text) => doBlock(elem::r, pp (text, blocks))
	      (* end case *))
	    | doText ([], [], blocks) = rev blocks
	    | doText ([], text, blocks) = rev(pp(text, blocks))
	  and doBlock (elem::r, blocks) = (case (K.kind elem)
		 of K.LIST => doBlock (r, listToHTML elem :: blocks)
		  | K.BLOCK => doBlock (r, blockToHTML elem :: blocks)
		  | K.FLOAT => raise Fail "FLOAT"
		  | K.OTHER => doText (elem::r, [], blocks)
		(* end case *))
	    | doBlock ([], blocks) = rev blocks
	  fun doPP (M.ELEM{elem=E.PP, body, ...}) = doText(body, [], [])
	  in
	    List.foldr (fn (para, bl) => (doPP para @ bl)) [] l
	  end

  end;

