(* html-template.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Entities:
 *	&body;			placeholder for document body
 *	&filename;		document filename (w/o extension)
 *	&title;			document title
 *	&version;		document version
 *	&doc.date;		document date in "month day, year" format
 *	&doc.year;		document year (4 digit format)
 *	&doc.day;		document day
 *	&doc.month;		document month (as a string)
 *	&doc.monthnum;		document month (as a number from 1-12)
 *	&today.date;		today's date in "month day, year" format
 *	&today.year;		today's year (4 digit format)
 *	&today.day;		today's day
 *	&today.month;		today's month (as a string)
 *	&today.monthnum;	today's month (as a number from 1-12)
 *	&base.url;		URL of the root directory of the document
 *	&parent.url;		URL of parent document
 *	&root.url;		URL of document root
 *	&index.url;		URL of document index
 *	&toc.url;		URL of table of contents
 *)

structure HTMLTemplate : sig

    val merge : {
	    context : HTMLContext.context,
	    template : HTML.html,
	    doc : HTML.html
	  } -> HTML.html

    val getOptTemplate : {
	    context : HTMLContext.context,
	    templateFile : string option,
	    doc : HTML.html
	  } -> HTML.html

  end = struct

    structure H = HTML
    structure I = MLDocIndex
    structure C = HTMLContext
    structure SS = Substring
    structure F = Format

    val entity_body		= Atom.atom "body"
    val entity_filename		= Atom.atom "filename"
    val entity_title		= Atom.atom "title"
    val entity_version		= Atom.atom "version"
    val entity_doc_date		= Atom.atom "doc.date"
    val entity_doc_year		= Atom.atom "doc.year"
    val entity_doc_day		= Atom.atom "doc.day"
    val entity_doc_month	= Atom.atom "doc.month"
    val entity_doc_monthnum	= Atom.atom "doc.monthnum"
    val entity_today_date	= Atom.atom "today.date"
    val entity_today_year	= Atom.atom "today.year"
    val entity_today_day	= Atom.atom "today.day"
    val entity_today_month	= Atom.atom "today.month"
    val entity_today_monthnum	= Atom.atom "today.monthnum"
    val entity_base_url		= Atom.atom "base.url"
    val entity_parent		= Atom.atom "parent.url"
    val entity_root		= Atom.atom "root.url"
    val entity_next		= Atom.atom "next.url"
    val entity_prev		= Atom.atom "prev.url"
    val entity_index		= Atom.atom "index.url"
    val entity_toc		= Atom.atom "toc.url"

    fun translateURL s = let
	  fun trans #" "  = "%20"
	    | trans #"\"" = "%22"
	    | trans #"%"  = "%25"
	    | trans #"&"  = "%26"
	    | trans #"<"  = "%3C"
	    | trans #">"  = "%3E"
	    | trans #"["  = "%5B"
	    | trans #"\\" = "%5C"
	    | trans #"]"  = "%5D"
	    | trans #"^"  = "%5E"
	    | trans #"`"  = "%60"
	    | trans #"{"  = "%7B"
	    | trans #"|"  = "%7C"
	    | trans #"}"  = "%7D"
	    | trans #"~"  = "%7E"
	    | trans c = if (Char.isPrint c)
		then str c
		else F.format "%%%02x" [F.INT(Char.ord c)]
	  in
	    String.translate trans s
	  end

    datatype 'a split = NOSPLIT of 'a | SPLIT of ('a * 'a)
    type text_split = H.text split

  (* translate a PCDATA token (either data or entity).  Return NONE, if
   * the token is "body", otherwise return SOME of the translation.
   *)
    fun transToken ctx = let
	  fun transStr s = (TextToHTML.transData s)
	  val version = HTMLContext.version ctx
	  val today = Date.fromTimeLocal(Time.now())
(* NOTE: empty titles arise in the case where we are generating an HTML
 * file that has no ML-Doc equivalent.  We should probably fix this in
 * the HTMLContext structure.
 *)
	  val title = (case (HTMLContext.title ctx)
		 of NONE => "NO TITLE"
		  | SOME s => s
		(* end case *))
	  val months = #[
		  "January", "February", "March", "April", 
		  "May", "June", "July", "August",
		  "September", "October", "November", "December"
		]
	  fun fmtVerOpt fmt sel = (case version
		 of (SOME v) => (case (sel v)
		       of (SOME x) => (fmt x)
			| NONE => "??"
		      (* end case *))
		  | NONE => "??"
		(* end case *))
	  fun fmtFileOpt NONE = ""
	    | fmtFileOpt (SOME f) = Atom.toString(I.File.name f) ^ ".html"
	  fun fmtYMD {y, m, d} = SOME(F.format "%s %d, %d" [
		  F.STR(Vector.sub(months, m - 1)), F.INT d, F.INT y
		])
	  fun trans (ScanPCData.PCDATA s) = SOME s
	    | trans (ScanPCData.ENTITY e) =
		if Atom.sameAtom(entity_body, e)
		  then NONE
		else if Atom.sameAtom(entity_filename, e)
		  then SOME(HTMLContext.srcFile ctx)
		else if Atom.sameAtom(entity_title, e)
		  then SOME(translateURL(transStr title))
		else if Atom.sameAtom(entity_version, e)
		  then SOME(fmtVerOpt transStr #verid)
		else if Atom.sameAtom(entity_doc_date, e)
		  then (case version
		     of NONE => SOME "??"
		      | (SOME{year, day=SOME d, month=SOME m, ...}) =>
			  fmtYMD{y=year, d=d, m=m}
		      | (SOME{year, day=NONE, month=SOME m, ...}) =>
			  SOME(F.format "%s %d" [
			      F.STR(Vector.sub(months, m - 1)), F.INT year
			    ])
		      | (SOME{year, ...}) => SOME(Int.toString year)
		  (* end case *))
		else if Atom.sameAtom(entity_doc_year, e)
		  then (case version
		     of NONE => SOME "??"
		      | (SOME v) => SOME(Int.toString(#year v))
		    (* end case *))
		else if Atom.sameAtom(entity_doc_day, e)
		  then (case version
		     of (SOME{day = SOME d, ...}) => SOME(Int.toString d)
		      | _ => SOME "??"
		    (* end case *))
		else if Atom.sameAtom(entity_doc_month, e)
		  then SOME(fmtVerOpt (fn m => Vector.sub(months, m - 1)) #month)
		else if Atom.sameAtom(entity_doc_monthnum, e)
		  then SOME(fmtVerOpt Int.toString #month)
		else if Atom.sameAtom(entity_today_date, e)
		  then SOME(Date.fmt "%B %d, %Y" today)
		else if Atom.sameAtom(entity_today_year, e)
		  then SOME(Int.toString(Date.year today))
		else if Atom.sameAtom(entity_today_day, e)
		  then SOME(Int.toString(Date.day today))
		else if Atom.sameAtom(entity_today_month, e)
		  then SOME(Date.fmt "%B" today)
		else if Atom.sameAtom(entity_today_monthnum, e)
		  then SOME(Date.fmt "%m" today)
		else if Atom.sameAtom(entity_base_url, e)
		  then MLDocConfig.getStr(C.config ctx, ["BaseURL"])
		else if Atom.sameAtom(entity_parent, e)
		  then SOME(fmtFileOpt(C.parentFile ctx))
		else if Atom.sameAtom(entity_root, e)
		  then SOME(fmtFileOpt(C.rootFile ctx))
		else if Atom.sameAtom(entity_index, e)
		  then SOME "index.html"
		else if Atom.sameAtom(entity_toc, e)
		  then SOME "toc.html"
		  else SOME(concat["&", Atom.toString e, ";"])
	  in
	    trans
	  end

    fun merge {context, template, doc} = let
	  val seenBody = ref false
	  val H.HTML{version, head, body=H.BODY{content=docBody, ...}} = doc
	  val H.HTML{body = H.BODY{
		  background, bgcolor, text, link, vlink, alink,
		  content=templateBody
		}, ...} = template
	  val transToken = transToken context
	  fun transStringNoSplit s = let
		fun transTokenNoSplit t = (case transToken t
		       of (SOME t) => t
			| _ => raise Fail "bad placement of &body; entity"
		      (* end case *))
		in
		  concat(map transTokenNoSplit (ScanPCData.tokenize s))
		end
	  fun mergeBlock (H.BlockList bl) =
		H.BlockList(map mergeBlock bl)
	    | mergeBlock (H.TextBlock t) = (case (mergeText t)
		 of (NOSPLIT t') => H.TextBlock t'
		  | (SPLIT(H.TextList[], H.TextList[])) => docBody
		  | (SPLIT(H.TextList[], t2)) => H.BlockList[docBody, H.TextBlock t2]
		  | (SPLIT(t1, H.TextList[])) => H.BlockList[H.TextBlock t1, docBody]
		  | (SPLIT(t1, t2)) => H.BlockList[
			H.TextBlock t1,
			docBody,
			H.TextBlock t2
		      ]
		(* end case *))
	    | mergeBlock (H.Hn{n, align, content}) = H.Hn{
		  n = n, align = align, content = noSplitText content
		}
	    | mergeBlock (H.ADDRESS b) = H.ADDRESS(mergeBlock b)
	    | mergeBlock (H.P{align, content}) = (case (mergeText content)
		 of (NOSPLIT t) => H.P{align = align, content = t}
		  | (SPLIT(H.TextList[], H.TextList[])) => docBody
		  | (SPLIT(H.TextList[], t2)) => H.BlockList[
			docBody,
			H.P{align = align, content = t2}
		      ]
		  | (SPLIT(t1, H.TextList[])) => H.BlockList[
			H.P{align = align, content = t1}, 
			docBody
		      ]
		  | (SPLIT(t1, t2)) => H.BlockList[
			H.P{align = align, content = t1}, 
			docBody,
			H.P{align = align, content = t2}
		      ]
		(* end case *))
	    | mergeBlock (H.UL{ty, compact, content}) = H.UL{
		  ty = ty, compact = compact, content = map mergeListItem content
		}
	    | mergeBlock (H.OL{ty, start, compact, content}) = H.OL{
		  ty = ty, start = start, compact = compact,
		  content = map mergeListItem content
		}
	    | mergeBlock (H.DIR{compact, content}) =
		H.DIR{compact = compact, content = map mergeListItem content}
	    | mergeBlock (H.MENU{compact, content}) =
		H.MENU{compact = compact, content = map mergeListItem content}
	    | mergeBlock (H.DL{compact, content}) = let
		fun mergeItem {dt, dd} =
		      {dt = map noSplitText dt, dd = mergeBlock dd}
		in
		  H.DL{compact = compact, content = map mergeItem content}
		end
	    | mergeBlock (H.PRE{width, content}) =
		H.PRE{width = width, content = noSplitText content}
	    | mergeBlock (H.DIV{align, content}) =
		H.DIV{align = align, content = mergeBlock content}
	    | mergeBlock (H.CENTER b) = H.CENTER(mergeBlock b)
	    | mergeBlock (H.BLOCKQUOTE b) = H.BLOCKQUOTE(mergeBlock b)
	    | mergeBlock (H.FORM{action, method, enctype, content}) =
		raise Fail "forms not supported"
	    | mergeBlock (b as H.ISINDEX{prompt}) = b
	    | mergeBlock (b as H.HR{...}) = b
	    | mergeBlock (H.TABLE{
		align, width, border, cellspacing, cellpadding, caption, content
	      }) = H.TABLE{
		  align = align, width = width, border = border,
		  cellspacing = cellspacing, cellpadding = cellpadding,
		  caption = Option.map mergeCaption caption,
		  content = map mergeTR content
		}
	  and mergeListItem (H.LI{ty, value, content}) =
		H.LI{ty = ty, value = value, content = mergeBlock content}
	  and mergeCaption (H.CAPTION{align, content}) =
		H.CAPTION{align = align, content = noSplitText content}
	  and mergeTR (H.TR{align, valign, content}) = H.TR{
		  align = align, valign = valign,
		  content = map mergeTableCell content
		}
	  and mergeTableCell cell = let
		fun mergeCell {
		      nowrap, rowspan, colspan, align, valign, width, height,
		      content
		    } = {
			nowrap  = nowrap, rowspan = rowspan, colspan = colspan,
			align = align, valign = valign,
			width = width, height = height,
			content = mergeBlock content
		      }
		in
		  case cell
		   of (H.TH x) => H.TH(mergeCell x)
		    | (H.TD x) => H.TD(mergeCell x)
		  (* end case *)
		end
	  and mergeText (H.TextList tl) = let
		fun cons (H.TextList[], l) = l
		  | cons (t, l) = t::l
		fun merge ([], tl') = NOSPLIT(H.TextList(rev tl'))
		  | merge (t::r, tl') = (case (mergeText t)
		       of NOSPLIT t' => merge(r, t'::tl')
			| SPLIT(t1, t2) =>
			    SPLIT(
			      H.TextList(rev(cons(t1,tl'))),
			      H.TextList(cons(t2, map noSplitText r)))
		      (* end case *))
		in
		  merge (tl, [])
		end
	    | mergeText (H.PCDATA d) = let
		fun concatList [] = H.TextList[]
		  | concatList l = H.PCDATA(String.concat l)
		fun merge ([], tl') = NOSPLIT(H.PCDATA(concat(rev tl')))
		  | merge (tok :: r, tl') = (case (transToken tok)
		       of NONE => (
			    if (! seenBody)
			      then raise Fail "multiple body attributes"
			      else seenBody := true;
			    SPLIT(
			      concatList(rev tl'),
			      concatList(map (valOf o transToken) r)))
			| (SOME s) => merge (r, s::tl')
		      (* end case *))
		in
		  merge (ScanPCData.tokenize d, [])
		end
	    | mergeText (H.TT t) = doTag H.TT t
	    | mergeText (H.I t) = doTag H.I t
	    | mergeText (H.B t) = doTag H.B t
	    | mergeText (H.U t) = doTag H.U t
	    | mergeText (H.STRIKE t) = doTag H.STRIKE t
	    | mergeText (H.BIG t) = doTag H.BIG t
	    | mergeText (H.SMALL t) = doTag H.SMALL t
	    | mergeText (H.SUB t) = doTag H.SUB t
	    | mergeText (H.SUP t) = doTag H.SUP t
	    | mergeText (H.EM t) = doTag H.EM t
	    | mergeText (H.STRONG t) = doTag H.STRONG t
	    | mergeText (H.DFN t) = doTag H.DFN t
	    | mergeText (H.CODE t) = doTag H.CODE t
	    | mergeText (H.SAMP t) = doTag H.SAMP t
	    | mergeText (H.KBD t) = doTag H.KBD t
	    | mergeText (H.VAR t) = doTag H.VAR t
	    | mergeText (H.CITE t) = doTag H.CITE t
	    | mergeText (H.A{name, href, rel, rev, title, content}) = NOSPLIT(H.A{
		  name = name,
		  href = Option.map transStringNoSplit href,
		  rel = rel, rev = rev, title = title,
		  content = noSplitText content
		})
	    | mergeText (t as H.IMG{...}) = NOSPLIT t
	    | mergeText (H.APPLET{...}) = raise Fail "APPLET not supported"
	    | mergeText (H.PARAM{...}) = raise Fail "PARAM not supported"
	    | mergeText (H.FONT{size, color, content}) =
		doTag (fn t => H.FONT{size=size, color=color, content=t}) content
	    | mergeText (H.BASEFONT{size, content}) =
		doTag (fn t => H.BASEFONT{size=size, content=t}) content
	    | mergeText (t as H.BR{...}) = NOSPLIT t
	    | mergeText (t as H.MAP{...}) = NOSPLIT t
	    | mergeText (H.INPUT{...}) = raise Fail "INPUT not supported"
	    | mergeText (H.SELECT{...}) = raise Fail "SELECT not supported"
	    | mergeText (H.TEXTAREA{...}) = raise Fail "TEXTAREA not supported"
	    | mergeText (H.SCRIPT data) = raise Fail "SCRIPT not supported"
	  and doTag cons t = (case mergeText t
		 of (NOSPLIT t') => NOSPLIT(cons t')
		  | (SPLIT(t1, t2)) => let
		      fun cons' (H.TextList[]) = H.TextList[]
			| cons' t = cons t
		      in
			SPLIT(cons' t1, cons' t2)
		      end
		(* end case *))
	  and noSplitText t = (case mergeText t
		 of (NOSPLIT t') => t'
		  | _ => raise Fail "bad placement of &body; entity"
		(* end case *))
	  val newDoc = H.HTML{
		  version = version,
		  head = head,
		  body = H.BODY{
		      background = background, bgcolor = bgcolor, text = text,
		      link = link, vlink = vlink, alink = alink,
		      content = mergeBlock templateBody
		    }
	        }
	  in
	    if (! seenBody)
	      then ()
	      else Error.error "template is missing &body; attribute";
	    newDoc
	  end

    local
      structure Err =
	struct
	  type context = {file : string option, line : int}

	  structure F = Format

	  fun prf ({file, line}, fmt, args) = (
	        case file
	         of NONE => TextIO.output (
		      TextIO.stdErr,
		      F.format "line %3d: " [F.INT line])
		  | (SOME fname) => TextIO.output (
		      TextIO.stdErr,
		      F.format "%s[%d]: " [F.STR fname, F.INT line])
	        (* end case *);
	        TextIO.output(TextIO.stdErr, F.format fmt args);
	        TextIO.output1(TextIO.stdErr, #"\n"))

	  fun badStartTag ctx tagName =
	        prf (ctx, "unrecognized start tag \"%s\"",[F.STR tagName])

	  fun badEndTag ctx tagName =
	        prf (ctx, "unrecognized end tag \"%s\"",[F.STR tagName])

	  fun badAttrVal ctx (attrName, attrVal) =
	        prf (ctx, "bad value \"%s\" for attribute \"%s\"",
		  [F.STR attrVal, F.STR attrName])

	  fun lexError ctx msg = prf (ctx, "%s", [F.STR msg])

	  fun syntaxError ctx msg = prf (ctx, "%s", [F.STR msg])

	  fun missingAttrVal ctx attrName =
	        prf (ctx, "missing value for \"%s\" attribute", [F.STR attrName])

	  fun missingAttr ctx attrName =
	        prf (ctx, "missing \"%s\" attribute", [F.STR attrName])

	  fun unknownAttr ctx attrName =
	        prf (ctx, "unknown attribute \"%s\"", [F.STR attrName])

	  fun unquotedAttrVal ctx attrName =
	        prf (ctx, "attribute value for \"%s\" should be quoted",
		  [F.STR attrName])

	end
    in
    structure HTMLParser = HTMLParserFn(Err)
    end

    fun getOptTemplate {context, templateFile, doc} = (case templateFile
	   of NONE => doc
	    | (SOME f) => let
		val template = HTMLParser.parseFile f
		in
		  merge{context=context, template=template, doc=doc}
		end
	  (* end case *))

  end

