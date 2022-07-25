(* do-spec.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These functions take the signature specs from declaration elements, and
 * returns strings for inclusion in the HTML document.
 *
 * For example, the assuming that the structure is "List", then the data:
 *
 *  "val app : ('a -> unit) -> 'a list -> unit\n"
 *
 * gets mapped to:
 *
 *  "val <A HREF="#List.app">app</A> : ('a -> unit) -> 'a list -> unit<BR>\n"
 *)

structure DoSpec : sig

    val preWid : int ref

    val doSigBody : HTMLContext.context -> MLDocMarkup.markup list -> {
	    sigText : HTML.block,
	    descText : HTML.block
	  }

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup
    structure Sp = MLSpec
    structure T = TextToHTML
    structure Mk = MakeHTML
    structure F = Format
    structure C = HTMLContext
    structure PP = HTMLPPSpec

  (* this is set by command-line args *)
    val preWid = ref 80

    fun withPP pp = let
	  val ppDev = HTMLDev.openDev {wid = !preWid, textWid = NONE}
	  val ppStrm = HTMLPPStrm.openStream ppDev
	  in
	    pp ppStrm;
	    HTMLPPStrm.closeStream ppStrm;
	    HTML.CODE(HTMLDev.done ppDev)
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

    fun mkAnchor (ctx, kind, objId) =
	  HRefs.mkAnchor(HRefs.descURL ctx {isRef=false, kind=kind, id=objId})

    fun mkItem ({inSpec, ctx}, kind, id, ppSpec, comment) = [{
	    dt = [HTML.TextList[mkAnchor (ctx, kind, id), withPP ppSpec]],
	    dd = mkDD ctx comment
	  }]

  (* extract the PROTOTY elements (if any) from the contents of a COMMENT
   * element.  Under the 2.3 version of ML-DOC DTD, a PROTOTY must be the
   * first element if there are any.  This function returns a list of
   * prototype, comment pairs.  If there are no prototypes, then the empty
   * list is returned.
   *)
    fun getPrototypes ctx (content as (M.ELEM{elem=E.PROTOTY, ...} :: _)) = let
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
	  fun getPPs ((elem as M.ELEM{elem=E.PP, ...}) :: r, grp) =
		getPPs (r, elem::grp)
	    | getPPs (r, grp) = (r, List.rev grp)
	  fun get [] = []
	    | get (M.ELEM{elem=E.PROTOTY, body, ...} :: r) = let
		val (r, pps) = getPPs (r, [])
		in
		  (List.map getPROTO body, pps) :: get r
		end
	    | get _ = raise Fail "bogus COMMENT contents"
	  in
	    get content
	  end
      | getPrototypes _ _ = []

    fun doIncludes (ctx, specs as ((id, _)::_), comment) = let
	  fun ppSpec ppStrm = PP.ppInclSpec (ctx, ppStrm, false) specs
	  in
	    case comment
	     of [] => []
	      | _ => (case (Sp.getIdAndDocument id)
		   of (id, NONE) => mkItem (ctx, HRefs.SigRef, id, ppSpec, comment)
(*** TODO: external document xref ***)
		  (* end case *))
	    (* end case *)
	  end

    fun doSharings (ctx, specs, comment) = let
	  val id = (case specs
		 of (Sp.STRshare(id::_) :: _) => id
		  | (Sp.TYshare(id::_) :: _) => id
		(* end case *))
	  fun ppSpec ppStrm = PP.ppSharingSpec (ctx, ppStrm) specs
	  in
	    mkItem (ctx, HRefs.ShareRef, id, ppSpec, comment)
	  end

    fun doExns (ctx, specs as ((id, _)::_), comment) = let
	  fun ppSpec ppStrm = PP.ppExnSpec (ctx, ppStrm) specs
	  in
	    mkItem (ctx, HRefs.ExnRef, id, ppSpec, comment)
	  end

    fun doTypes (ctx, specs as ({id, ...}::_), comment) = let
	  fun ppSpec ppStrm = PP.ppTySpec (ctx, ppStrm) specs
	  in
	    mkItem (ctx, HRefs.TyRef, id, ppSpec, comment)
	  end

    type dtspec = {
	  compact : bool, params : string option, id : string,
	  cons : (string * MLSpec.ml_type option * M.markup list) list
	}

(** NOTE: what if we have a mix of complex and simple datatype specs in
 ** the same list??
 **)
    fun doComplexDatatypes (ctx, [spec : dtspec], comment) = let
	  val {params, id, cons, ...} = spec
	  fun consToHTML (isFirst, (id, optTy, comment) :: r) = let
		fun ppSpec ppStrm = PP.ppConsSpec (ctx, ppStrm) {
			isFirst = isFirst, id = id, ty = optTy
		      }
		in
		  {dt = [withPP ppSpec], dd = mkDD (#ctx ctx) comment}
		    :: consToHTML (false, r)
		end
	    | consToHTML (_, []) = []
	  val descList = HTML.DL{compact = false, content = consToHTML (true, cons)}
	  fun ppSpec ppStrm = (
		HTMLPPStrm.openHBox ppStrm;
		PP.ppKW (ppStrm, "datatype");
		HTMLPPStrm.space ppStrm 1;
		PP.ppTyParams (ppStrm, params);
		HTMLPPStrm.string ppStrm id)
	  val dd = (case (T.ppToHTML (#ctx ctx) comment)
		 of [] => descList
		  | bl => Mk.blockList(
		      T.stripLeadingPara bl @ [HTML.TextBlock Mk.BR, descList])
		(* end case *))
	  in
	    [{dd = dd, dt = [mkAnchor ((#ctx ctx), HRefs.TyRef, id), withPP ppSpec]}]
	  end
      | doComplexDatatypes _ =
	  raise Fail "multiple complex datatypes unimplemented"

    fun doSimpleDatatypes (_, _, []) = []
      | doSimpleDatatypes (ctx, specs as (({id, ...} : dtspec) :: _), comment) = let
	  fun ppSpec ppStrm = PP.ppDTSpec (ctx, ppStrm) specs
	  in
	    mkItem (ctx, HRefs.TyRef, id, ppSpec, comment)
	  end

    fun doDatatypes (ctx, specs, comment) = let
	  fun isSimple ({cons, ...} : dtspec) =
		List.all (fn (_, _, []) => true | _ => false) cons
	  in
	    if (List.all isSimple specs)
	      then doSimpleDatatypes (ctx, specs, comment)
	      else doComplexDatatypes (ctx, specs, comment)
	  end

    fun doDatatypeDefs (ctx, specs as ({id, def} :: _), comment) = let
	  fun ppSpec ppStrm = PP.ppDTDefSpec (ctx, ppStrm) specs
	  in
	    mkItem (ctx, HRefs.TyRef, id, ppSpec, comment)
	  end

(* NOTE: someday we should do something with the RAISES clause *)
    fun doVals (ctx, specs as ((id, _, _)::_), comment) = let
	  fun ppSpec ppStrm = PP.ppValSpec (ctx, ppStrm, false) specs
	  in
	    case (getPrototypes (#ctx ctx) comment)
	     of [] => mkItem (ctx, HRefs.ValRef, id, ppSpec, comment)
	      | items => List.map (fn (dt, r) => {
		    dt = mkAnchor((#ctx ctx), HRefs.ValRef, id)::dt,
		    dd = mkDD (#ctx ctx) r
		  }) items
	    (* end case *)
	  end

    fun structureId id = HTML.CODE(HTML.TextList[
	    HTML.B(HTML.PCDATA "structure"),
	    HTML.PCDATA "&nbsp;",
	    HTML.PCDATA id
	  ])

    fun doSpec ctx (Sp.BRspec _, _) = []
(* FIXME: wheretypes *)
      | doSpec ctx (Sp.STRSIGspec(strId, body), comment) = let
	  val ctx' = C.withSubstr (#ctx ctx, strId)
	  val descList = HTML.DL{
		  compact = false,
		  content = doSpecs {inSpec = #inSpec ctx, ctx = ctx'} body
		}
	  in
	    [{
		dt = [mkAnchor((#ctx ctx), HRefs.StrRef, strId), structureId strId],
		dd = HTML.BlockList((mkDD' (#ctx ctx) comment) @ [descList])
	      }]
	  end
      | doSpec ctx (Sp.DTspec specs, comment) = doDatatypes (ctx, specs, comment)
      | doSpec ctx (_, []) = [] (* we omit specifications with no comments *)
      | doSpec ctx (Sp.INCLspec specs, comment) = doIncludes (ctx, specs, comment)
      | doSpec ctx (Sp.STRspec(spec as (id, _, _)), comment) = let
	  fun ppSpec ppStrm = PP.ppStrSpec (ctx, ppStrm, false) spec
	  in
	    mkItem (ctx, HRefs.StrRef, id, ppSpec, comment)
	  end
      | doSpec ctx (Sp.SHARINGspec specs, comment) =
	  doSharings (ctx, specs, comment)
      | doSpec ctx (Sp.EXNspec specs, comment) = doExns (ctx, specs, comment)
      | doSpec ctx (Sp.TYspec specs, comment) = doTypes (ctx, specs, comment)
      | doSpec ctx (Sp.DTDEFspec specs, comment) =
	  doDatatypeDefs (ctx, specs, comment)
      | doSpec ctx (Sp.VALspec specs, comment) = doVals (ctx, specs, comment)

    and doSpecs ctx specs =
	  List.foldr (fn (spec, descs) => (doSpec ctx spec) @ descs) [] specs

    fun doSigBody ctx sigBody = let
	  val specs = Sp.getSigbodyContents sigBody
	  val specCtx = {inSpec = true, ctx = ctx}
	  val specPart =
		withPP (fn ppStrm => PP.ppSpecs (specCtx, ppStrm, true) specs)
	  val doSpec = doSpec {inSpec=false, ctx=ctx}
	  fun f (spec, descs) = doSpec spec @ descs
	  val descPart = (case List.foldr f [] specs
		 of [] => HTML.BlockList[]
		  | stuff => HTML.DL{compact = false, content = stuff}
		(* end case *))
	  in
	    { sigText = HTML.BLOCKQUOTE(HTML.TextBlock specPart),
	      descText = descPart
	    }
	  end

  end (* DoSpec *)

