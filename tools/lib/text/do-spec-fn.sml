(* do-spec-fn.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * These functions take the signature specs from declaration elements, and
 * return strings for inclusion in a document.
 *
 * For example, the assuming that the structure is "List", then the data:
 *
 *  "val app : ('a -> unit) -> 'a list -> unit\n"
 *
 * gets mapped to:
 *
 *  "val <A HREF="#List.app">app</A> : ('a -> unit) -> 'a list -> unit<BR>\n"
 *)

functor DoSpec (T : TEXT) : sig

    structure T : Text

    val doSpec : T.context -> MLDocMarkup.markup -> {
	    sigText : T.text list,
	    descText : {
		dt : T.text list,
		dd : T.block
	      } list
	  }

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup
    structure Sp = MLSpec
    structure F = Format

    val pcdata_BAR = HTML.PCDATA " <B>|</B> "
    val pcdata_BAR2 = HTML.PCDATA "&nbsp;&nbsp;<B>|</B> "
    val pcdata_COLON = HTML.PCDATA " <B>:</B> "
    val pcdata_EQ = HTML.PCDATA " <B>=</B> "
    val pcdata_EQ2 = HTML.PCDATA "&nbsp;&nbsp;<B>=</B> "
    val pcdata_SP = HTML.PCDATA " "
    val pcdata_datatype = HTML.PCDATA "<B>datatype</B> "
    val pcdata_end = HTML.PCDATA "<B>end</B>"
    val pcdata_eqtype = HTML.PCDATA "<B>eqtype</B> "
    val pcdata_exception = HTML.PCDATA "<B>exception</B> "
    val pcdata_include = HTML.PCDATA "<B>include</B> "
    val pcdata_of = HTML.PCDATA " <B>of</B> "
    val pcdata_sharing = HTML.PCDATA "<B>sharing</B> "
    val pcdata_sharing_type = HTML.PCDATA "<B>sharing type</B> "
    val pcdata_sig = HTML.PCDATA "<B>sig</B>"
    val pcdata_structure = HTML.PCDATA "<B>structure</B> "
    val pcdata_type = HTML.PCDATA "<B>type</B> "
    val pcdata_val = HTML.PCDATA "<B>val</B> "

    fun indentSp n =
	  HTML.PCDATA(String.concat (List.tabulate (n, fn _ => "&nbsp;")))
    fun indentList 0 l = l
      | indentList n l = indentSp n :: l

  (* map an "a -> b list" function over a list and concatenate the
   * results.
   *)
    fun mapConcat f l = List.foldr (fn (x, l) => f x @ l) [] l

    fun doList {init, sep, term, doItem} [] = init @ term
      | doList {init, sep, term, doItem} l = let
	  fun f [] = term
	    | f [x] = doItem x @ term
	    | f (x :: r) = doItem x @ sep @ (f r)
	  in
	    init @ f l
	  end

  (* clean-up a DD element; we convert the leading <P> to HTEXT to avoid
   * excessive space, and convert empty elements to <BR>.  We also add a double
   * <BR> to the end to get inter-item spacing.
   *)
    fun mkDD' ctx comment = (case (T.ppToHTML ctx comment)
	   of [] => [HTML.TextBlock(Mk.BR)]
	    | bl => T.stripLeadingPara bl
		@ [HTML.TextBlock(HTML.TextList[Mk.BR, Mk.BR])]
	  (* end case *))
    fun mkDD ctx comment = Mk.blockList(mkDD' ctx comment)

    datatype tag = TAG of {
	xref : string -> HTML.text -> HTML.text,
	anchor : HTML.text -> HTML.text
      }

    fun mkTag (ctx, kind, objId) =
	  TAG(HRefs.mkLocalRef ctx {kind=kind, localId=objId})

    fun mkHRef (TAG{xref, ...}) name = xref name (T.dataToHTML name)

    fun anchor (TAG{anchor, ...}, htext::r) = anchor htext :: r
      | anchor (_, []) = []

  (* extract the PROTOTY element (if any) from the contents of a COMMENT
   * element.  Under the 1.5 version of ML-DOC DTD, the PROTOTY must be the
   * first element.
   *)
    fun getPrototypes ctx (M.ELEM{elem=E.PROTOTY, body, ...} :: r) = let
	  fun getPROTO (M.ELEM{elem=E.PROTO, body, ...}) = let
		fun getEval ([], _) = (body, NONE)
		  | getEval ([M.ELEM{elem=E.EVALTO, body, ...}], l) =
		      (rev l, SOME(T.codeToHTML ctx body))
		  | getEval (x::r, l) = getEval(r, x::l)
		in
		  case getEval(body, [])
		   of (body, NONE) => HTML.CODE(T.codeToHTML ctx body)
		    | (body, SOME result) => HTML.TextList[
			  HTML.CODE(T.codeToHTML ctx body),
			  HTML.EM(HTML.PCDATA "evaluates to"),
			  HTML.CODE result
			]
		  (* end case *)
		end
	    | getPROTO _ = raise Fail "bogus PROTOTY contents"
	  in
	    SOME(map getPROTO body, r)
	  end
      | getPrototypes _ _ = NONE

    fun doSharing s = let
	  fun doItem x = [T.dataToHTML x]
	  in
	    case s
	     of (Sp.STRshare sl) =>
		  doList {
		      init=[pcdata_sharing], sep=[pcdata_EQ], term=[Mk.BR],
		      doItem = doItem
		    } sl
	      | (Sp.TYshare sl) =>
		  doList {
		      init=[pcdata_sharing_type], sep=[pcdata_EQ], term=[Mk.BR],
		      doItem = doItem
		    } sl
	    (* end case *)
	  end

    fun doIncludes (ctx, indent, specs as ((tagId, sl) :: _), comment) = let
	  val tag = mkTag (ctx, HRefs.SigRef, tagId)
	  val mkHRef = mkHRef tag
	  fun htmlSig (sigId, sl) = let
		val spec = indentList indent [pcdata_include, mkHRef sigId, Mk.BR]
		in
		  spec @ (mapConcat doSharing sl)
		end
(** NOTE: we should probably wrap the signature ID with a link to the
 ** defining site.
 **)
	  fun mkDT (sigId, _) =
		HTML.CODE(HTML.TextList[pcdata_include, T.dataToHTML sigId])
	  in
	    { sigText = mapConcat htmlSig specs,
	      descText = [{
		  dt = anchor (tag, map mkDT specs),
		  dd = mkDD ctx comment
		}]
	    }
	  end

    fun doExns (ctx, indent, specs as ((tagId, _)::_), comment) = let
	  val tag = mkTag (ctx, HRefs.ExnRef, tagId)
	  val mkHRef = mkHRef tag
	  fun htmlSig (exnId, NONE) =
		indentList indent [pcdata_exception, mkHRef exnId, Mk.BR]
	    | htmlSig (exnId, SOME ty) = indentList indent [
		  pcdata_exception, mkHRef exnId, pcdata_of,
		  T.dataToHTML ty, Mk.BR
		]
	  fun mkDT (exnId, _) =
		HTML.CODE(HTML.TextList[pcdata_exception, T.dataToHTML exnId])
	  in
	    { sigText = mapConcat htmlSig specs,
	      descText = [{
		  dt = anchor (tag, map mkDT specs),
		  dd = mkDD ctx comment
		}]
	    }
	  end

    fun tySpec (indent, kw, NONE, id) =
	  indentList indent [kw, id]
      | tySpec (indent, kw, SOME p, id) =
	  indentList indent [kw, T.dataToHTML p, pcdata_SP, id]

    fun tySpecText (kw, params, id) = HTML.TextList(tySpec (0, kw, params, id))

(** Need to do something with type abbreviations **)
    fun doTypes (ctx, indent, specs as ({eq, params, id, share, def}::_), comment) =
	  let
	  val tag = mkTag (ctx, HRefs.TyRef, id)
	  val mkHRef = mkHRef tag
	  fun kw true = pcdata_eqtype
	    | kw false = pcdata_type
	  fun htmlSig {eq, params, id, share, def} = let
		val share = (case share
		       of (SOME s) => doSharing s
			| NONE => []
		      (* end case *))
		in
		  tySpec (indent, kw eq, params, mkHRef id) @ (Mk.BR :: share)
		end
	  fun mkDT {eq, params, id, share, def} =
		HTML.CODE(tySpecText(kw eq, params, HTML.PCDATA id))
	  in
	    { sigText = mapConcat htmlSig specs,
	      descText = [{
		  dt = anchor (tag, map mkDT specs),
		  dd = mkDD ctx comment
		}]
	    }
	  end

    type dtspec = {
	  compact : bool, params : string option, id : string,
	  cons : (string * MLSpec.ml_type option * M.markup list) list
	}

    fun mkDatatypeDT ({params, id, ...} : dtspec) =
	  HTML.CODE(tySpecText(pcdata_datatype, params, HTML.PCDATA id))

(** NOTE: what if we have a mix of complex and simple datatype specs in
 ** the same list??
 **)
    fun doComplexDatatypes (ctx, indent, [spec : dtspec], comment) = let
	  val {params, id, cons, ...} = spec
	  val tag = mkTag (ctx, HRefs.TyRef, id)
	  val prefix = tySpec (indent, pcdata_datatype, params, mkHRef tag id)
	  fun consToHTML (isFirst, (id, optTy, comment) :: r, (specs, descs)) = let
		val prefix = if isFirst then pcdata_EQ2 else pcdata_BAR2
		val spec = (case optTy
		       of NONE => [T.dataToHTML id]
			| (SOME ty) =>
			    [T.dataToHTML id, pcdata_of, T.dataToHTML ty]
		      (* end case *))
		in
		  consToHTML (false, r,
		    ( (prefix::spec) :: specs,
		      {dt=[HTML.CODE(Mk.textList spec)], dd=mkDD ctx comment}
			:: descs
		    ))
		end
	    | consToHTML (_, [], (specs, descs)) =
		(rev([Mk.BR] :: specs), rev descs)
	  val (specs, descs) = consToHTML (true, cons, ([], []))
	  val descList = HTML.DL{compact = false, content=descs}
	  val dd = (case (T.ppToHTML ctx comment)
		 of [] => descList
		  | bl => Mk.blockList(
		      T.stripLeadingPara bl @ [HTML.TextBlock Mk.BR, descList])
		(* end case *))
	  in
	    { sigText = List.concat(prefix :: specs),
	      descText = [{dt = anchor (tag, [mkDatatypeDT spec]), dd = dd}]
	    }
	  end
      | doComplexDatatypes _ =
	  raise Fail "multiple complex datatypes unimplemented"

    fun doSimpleDatatypes (
	  ctx, indent, specs as (({id=tagId, ...} : dtspec) :: _), comment
	) = let
	  val tag = mkTag (ctx, HRefs.TyRef, tagId)
	  val mkHRef = mkHRef tag
	  fun htmlSig {compact, params, id, cons} = let
		val prefix = tySpec (indent, pcdata_datatype, params, mkHRef id)
		fun consToHTML (id, NONE, _) = [T.dataToHTML id]
		  | consToHTML (id, SOME ty, _) =
		      [T.dataToHTML id, pcdata_of, T.dataToHTML ty]
		in
		  if compact
		    then doList {
			init = prefix @ [pcdata_EQ],
			sep = [pcdata_BAR], term = [Mk.BR],
			doItem = consToHTML
		      } cons
		    else doList {
			init = prefix @ [Mk.BR, pcdata_EQ2],
			sep = [Mk.BR, pcdata_BAR2], term = [Mk.BR],
			doItem = consToHTML
		      } cons
		end
	  in
	    { sigText = mapConcat htmlSig specs,
	      descText = [{
		  dt = anchor (tag, map mkDatatypeDT specs),
		  dd = mkDD ctx comment
		}]
	    }
	  end

    fun doDatatypes (ctx, indent, specs, comment) = let
	  fun isSimple ({cons, ...} : dtspec) =
		List.all (fn (_, _, []) => true | _ => false) cons
	  in
	    if (List.all isSimple specs)
	      then doSimpleDatatypes (ctx, indent, specs, comment)
	      else doComplexDatatypes (ctx, indent, specs, comment)
	  end

    fun doVals (ctx, indent, specs as ((tagId, _)::_), comment) = let
	  val tag = mkTag (ctx, HRefs.ValRef, tagId)
	  val mkHRef = mkHRef tag
	  fun htmlSig (valId, ty) = indentList indent [
		  pcdata_val, mkHRef valId, pcdata_COLON,
		  T.dataToHTML ty, Mk.BR
		]
	  fun mkDT (valId, ty) =
		HTML.CODE(HTML.TextList[pcdata_val, T.dataToHTML valId])
	  val sigText = mapConcat htmlSig specs
	  in
	    case (getPrototypes ctx comment)
	     of NONE => {
		    sigText = sigText,
		    descText = [{
			dt = anchor (tag, map mkDT specs),
			dd = mkDD ctx comment
		      }]
		  }
	      | SOME(dt, r) => {
		    sigText = sigText,
		    descText = [{dt = anchor (tag, dt), dd = mkDD ctx r}]
		  }
	    (* end case *)
	  end

    fun doSpec' ctx indent (spec, comment) = (case spec
	   of (Sp.INCLspec specs) => doIncludes (ctx, indent, specs, comment)
	    | (Sp.STRspec(strId, sigId, sl)) => let
		val tag = mkTag (ctx, HRefs.StrRef, strId)
		val mkHRef = mkHRef tag
		val sigText = [
			pcdata_structure, mkHRef strId, pcdata_COLON,
			HTML.PCDATA sigId, Mk.BR
		      ]
		val dt = [HTML.CODE(HTML.TextList[
			pcdata_structure, T.dataToHTML(strId)
		      ])]
		in
		  { sigText = sigText,
		    descText = [{
			dt = anchor (tag, dt),
			dd = mkDD ctx comment
		      }]
		  }
		end
	    | (Sp.STRSIGspec(strId, body, sl)) => let
		val (sigs, descs) = doSpecs ctx (indent+4) body
		val tag = mkTag (ctx, HRefs.StrRef, strId)
		val mkHRef = mkHRef tag
		val firstLine = indentList indent [
			pcdata_structure, mkHRef strId,
			pcdata_COLON, pcdata_sig, Mk.BR
		      ]
		val lastLine = [
			indentSp (indent+2), pcdata_end, Mk.BR
		      ]
		val dt = [HTML.CODE(HTML.TextList[
			pcdata_structure, T.dataToHTML(strId)
		      ])]
		val descList = HTML.DL{
			compact = false,
			content = descs
		      }
		in
		  { sigText = firstLine @ sigs @ lastLine,
		    descText = [{
			dt = anchor (tag, dt),
			dd = HTML.BlockList((mkDD' ctx comment) @ [descList])
		      }]
		  }
		end
	    | (Sp.EXNspec specs) => doExns (ctx, indent, specs, comment)
	    | (Sp.TYspec specs) => doTypes (ctx, indent, specs, comment)
	    | (Sp.DTspec specs) => doDatatypes (ctx, indent, specs, comment)
	    | (Sp.VALspec specs) => doVals (ctx, indent, specs, comment)
	  (* end case *))

    and doSpecs ctx indent specs = let
	  fun f (specAndComment, (sigs, descs)) = let
		  val {sigText, descText} = doSpec' ctx indent specAndComment
		  in
		    (sigText @ sigs, descText @ descs)
		  end
	  in
	    List.foldr f ([], []) specs
	  end

    fun doSpec ctx spec = doSpec' ctx 0 (Sp.getSpecContents spec)

  end (* DoSpec *)

