(* do-file.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A tool for extracting latex output from ML-Doc documents
 *)

structure DoFile : sig

    val doFile : {
	    config : MLDocConfig.configuration,
	    master: MLDocIndex.index,
	    nodeMap : MLDocIndex.File.entry -> FileTree.file_nd,
	    outStrm : TextIO.outstream,
	    doc : MLDocMarkup.markup list,
            level : int
	  } -> unit

  end = struct

    structure F = Format
    structure E = MLDocElem
    structure M = MLDocMarkup
    structure MM = MLDocModule
    structure Doc = MLDocContent
    structure Sp = MLSpec
    structure C = LaTeXContext
    structure Txt = TextToLaTeX
    structure Idx = LaTeXIndex
    structure PP = LaTeXPPStrm
    structure PPSp = LaTeXPPSpec
    structure Err = Error

    val pr = C.pr
    val prf = C.prf

    fun withPP (ppCtx : LaTeXStyle.context) ppFn = let
	  val dev = LaTeXDev.openDev{wid = 60, dst = C.streamOf(#ctx ppCtx)}
	  val ppStrm = PP.openStream dev
	  in
	    ppFn (ppCtx, ppStrm);
	    PP.closeStream ppStrm
	  end

  (* make PP contexts from regular contexts *)
    fun inSynop ctx = {loc=LaTeXStyle.Synop, ctx=ctx}
    fun inSpec ctx = {loc=LaTeXStyle.Spec, ctx=ctx}
    fun inDesc ctx = {loc=LaTeXStyle.Desc, ctx=ctx}


  (* doHeading config (ctx, level, heading)
   *)
    local
      val secHeadings = (0, #[
	      F.STR "part",		F.STR "chapter",
	      F.STR "section",		F.STR "subsection",
	      F.STR "subsubsection",	F.STR "paragraph",
	      F.STR "subparagraph"
	    ])
      val moduleHeadings = (1, #[
	      F.STR "mldModuleChap",	F.STR "mldModuleSec",
	      F.STR "mldModuleSubsec",	F.STR "mldMODULE",
	      F.STR "mldMODULE",	F.STR "mldMODULE"
	    ])
      fun doHeading' (bias, headings) config = let
	    val topLevelSection = (
		  case MLDocConfig.getStr(config, ["TopLevelSection"])
		   of SOME "Part" => 0 - bias
		    | SOME "Chapter" => 1 - bias
		    | SOME "Section" => 2 - bias
		  (* end case *))
	    fun prHead (ctx, level, style, heading) = let
		  val sec = Vector.sub(headings, topLevelSection + level)
		  fun prCmd optStar = (
			prf(ctx, "\\%s%s{", [sec, F.STR optStar]);
			app (Txt.doInline (C.headingContext ctx)) heading;
			pr (ctx, "}\n"))
		  in
		    case style
		     of Doc.NUMBER_SEC => prCmd ""
		      | Doc.NONUMBER_SEC => (
			  prCmd "*";
			  prf(ctx, "\\addcontentsline{toc}{%s}{", [sec]);
			  app (Txt.doInline (C.headingContext ctx)) heading;
			  pr (ctx, "}\n"))
		      | Doc.NOTOC_SEC => prCmd "*"
		    (* end case *)
		  end
	    in
	      prHead
	    end
    in
    val doHeading = doHeading' secHeadings
    val doModuleHeading = doHeading' moduleHeadings
    end (* local *)

    fun doLabel (ctx, NONE) = ()
      | doLabel (ctx, SOME lab) = prf (ctx, "\\label{%s}\n", [F.STR lab])

    datatype phase = SYNOPSIS | INTERFACE | DESCRIPTION

  (* COMMENT ::= ((PROTOTY, PP+)+ | PP+) *)
    fun doComment (ctx, path) = let
	  fun doElem (pp as M.ELEM{elem=E.PP, body, ...}) = Txt.doPP ctx pp
	    | doElem (proto as M.ELEM{elem=E.PROTOTY, body, ...}) = let
		val doCode = app (Txt.doInline (C.protoContext ctx))
		fun doProtoBody body = let
		      fun getOptEvalto ([], _) = (body, [])
			| getOptEvalto ([M.ELEM{elem=E.EVALTO, body, ...}], l) =
			    (rev l, body)
			| getOptEvalto (elem::r, l) = getOptEvalto(r, elem::l)
		      in
			case getOptEvalto (body, [])
			 of (body, []) => doCode (TrimWS.trim body)
			  | (body, evalTo) => (
			      doCode (TrimWS.trim body);
			      pr(ctx, " \\mldEVALTO{} ");
			      doCode (TrimWS.trim evalTo))
			(* end case *)
		      end
		fun doBodyElem cmd (M.ELEM{elem=E.PROTO, body, ...}) = (
		      prf(ctx, "\\%s{", [F.STR cmd]);
		      doProtoBody(TrimWS.leading body);
		      pr(ctx, "}"))
		in
		  pr(ctx, "\n");
		  case body
		   of [_] => List.app (doBodyElem "mldPROTO") body
		    | _ => List.app (doBodyElem "mldPROTONL") body
		  (* end case *)
		end
	  fun doComment' (Sp.STRSIGspec(id, specs), comment) = let
		fun doSpec () = (
		      pr (ctx, "\\begin{mldSPEC}\n");
		      withPP (inDesc ctx) (fn (ppCtx, ppStrm) =>
			PPSp.ppStructure (ppCtx, ppStrm) (id, false, NONE, []));
		      pr (ctx, "\\end{mldSPEC}%\n"))
		in
		  case (comment, Sp.emptyDescription specs)
		   of ([], true) => ()
		    | ([], false) => (
			doSpec ();
			pr (ctx, "\\begin{mldCOMMENT}%\n");
			pr (ctx, "\\noindent\\begin{mldSUBDESC}%\n");
			List.app doComment' specs;
			pr (ctx, "\\end{mldSUBDESC}%\n");
			pr (ctx, "\\end{mldCOMMENT}%\n"))
		    | (_, true) => (
			doSpec ();
			pr (ctx, "\\begin{mldCOMMENT}%");
			  List.app doElem comment;
			pr (ctx, "\\end{mldCOMMENT}%\n"))
		    | (_, false) => (
			doSpec ();
			pr (ctx, "\\begin{mldCOMMENT}%");
			List.app doElem comment;
			pr (ctx, "\\begin{mldSUBDESC}%\n");
			List.app doComment' specs;
			pr (ctx, "\\end{mldSUBDESC}%\n");
			pr (ctx, "\\end{mldCOMMENT}%\n"))
		  (* end case *)
		end
	    | doComment' (spec as Sp.DTspec specs, comment) = let
		fun isSimple {compact, params, id, cons} =
		      List.all (fn (_, _, []) => true | _ => false) cons
		val simple = List.all isSimple specs
		fun doComplexSpec [{compact, params, id, cons}] = (
(*		      prf (ctx, "\\begin{mldSPEC}%s\n", [F.STR(indexTyDesc(path, id))]);*)
		      pr (ctx, "\\begin{mldSPEC}%s\n");
		      withPP (inDesc ctx) (fn (ppDesc, ppStrm) =>
			PPSp.ppDTLHS (ppDesc, ppStrm) (params, id));
		      pr (ctx, "\\end{mldSPEC}%\n");
		      cons)
		  | doComplexSpec _ = raise Fail "multiple complex datatype specs"
		fun doCons ((id, optTy, []), _) = false
		  | doCons ((id, optTy, comment), isFirst) = (
(*		      prf (ctx, "\\begin{mldSPEC}%s\n", [F.STR(indexConDesc(path, id))]);*)
		      pr (ctx, "\\begin{mldSPEC}\n");
			withPP (inDesc ctx)
			  (fn (ppCtx, ppStrm) => PPSp.ppConsSpec (ppCtx, ppStrm) {
			      isFirst = isFirst, id = id, ty = optTy
			    });
		      pr (ctx, "\\end{mldSPEC}%\n");
		      pr (ctx, "\\begin{mldCOMMENT}%");
		      List.app doElem comment;
		      pr (ctx, "\\end{mldCOMMENT}%\n");
		      false)
		in
		  case (comment, List.all isSimple specs)
		   of ([], true) => ()
		    | (comment, true) => specAndComment (spec, comment)
		    | ([], false) => let
			val cons = doComplexSpec specs
			in
			  pr (ctx, "\\begin{mldCOMMENT}%\n");
			  pr (ctx, "\\noindent\\begin{mldSUBDESC}%\n");
			  List.foldl doCons true cons;
			  pr (ctx, "\\end{mldSUBDESC}%\n");
			  pr (ctx, "\\end{mldCOMMENT}%\n")
			end
		    | (comment, false) => let
			val cons = doComplexSpec specs
			in
			  pr (ctx, "\\begin{mldCOMMENT}%");
			  List.app doElem comment;
			  pr (ctx, "\\begin{mldSUBDESC}%\n");
			  List.foldl doCons true cons;
			  pr (ctx, "\\end{mldSUBDESC}%\n");
			  pr (ctx, "\\end{mldCOMMENT}%\n")
			end
		  (* end case *)
		end
	    | doComment' (spec, []) = ()
	    | doComment'(spec, comment) = specAndComment (spec, comment)
	  and specAndComment (spec, comment) = let
		fun prRaisesIx (id, _, raises) = let
		      fun prRaises (elem as M.ELEM{elem=E.EXNREF _, body=[M.DATA exn], ...}) =
			    pr (ctx, Idx.raises{ctx=ctx, exn=exn, f=id})
		      in
			List.app prRaises raises
		      end
		fun prRaisesDescIx (id, _) = pr(ctx, Idx.exnRaisesDesc (ctx, id))
		in
		  pr (ctx, "\\begin{mldSPEC}\n");
		  withPP (inDesc ctx)
		    (fn (ppCtx, ppStrm) => PPSp.ppSpec (ppCtx, ppStrm, false) spec);
		  pr (ctx, "\\end{mldSPEC}%\n");
		(* generate Raises index entries as necessary *)
		  case spec
		   of Sp.EXNspec spcs => List.app prRaisesDescIx spcs
		    | Sp.VALspec spcs => List.app prRaisesIx spcs
		    | _ => ()
		  (* end case *);
		  pr (ctx, "\\begin{mldCOMMENT}%");
		  List.app doElem comment;
		  pr (ctx, "\\end{mldCOMMENT}%\n")
		end
	  in
	    doComment'
	  end

    fun doSynopsis (ctx, doit) = (
	  pr(ctx, "\\begin{mldSYNOPSIS}%\n\\begin{mldTightCode}");
	  withPP (inSynop ctx) doit;
	  pr(ctx, "\\end{mldTightCode}%\n\\end{mldSYNOPSIS}%\n"))

    fun doInterface (ctx, doit) = (
	  pr(ctx, "\\begin{mldINTERFACE}%\n\\begin{mldTightCode}\n");
	  withPP (inSpec ctx) doit;
	  pr(ctx, "\\end{mldTightCode}%\n\\end{mldINTERFACE}%\n"))

    fun doFctInterface (ctx, kind, doit) = (
	  prf(ctx, "\\begin{mldFCTINTERFACE}{%s}%%\n\\begin{mldTightCode}\n", [
	      F.STR kind
	    ]);
	  withPP (inDesc ctx) doit;
	  pr(ctx, "\\end{mldTightCode}%\n\\end{mldFCTINTERFACE}%\n"))

    fun doDescription (ctx, path, specs) =
	  if (Sp.emptyDescription specs)
	    then ()
	    else (
	      pr(ctx, "\\begin{mldDESCRIPTION}%\n");
	      app (doComment (ctx, path)) specs;
	      pr(ctx, "\\end{mldDESCRIPTION}%\n"))

    fun doSignature (ctx, sigId, status, specs, instances) = let
	  val ctx = C.withSig (ctx, sigId)
	  val specs = Sp.getSigbodyContents specs
	  fun ppInstances ppStrm = let
		fun pp [] = ()
		  | pp (instance::r) = let
		      val {strId, status, opaque, whereTypes, comment} = instance
		      in
			PPSp.ppPageBreak(ppStrm, false);
			PP.cut ppStrm;
			PPSp.ppStructure (inSpec ctx, ppStrm)
			  (strId, opaque, SOME sigId, whereTypes);
			PP.string ppStrm
			  (LaTeXIndex.sigInstance {sigId=sigId, strId=strId});
			pp r
		      end
		in
		  pp
		end
	  fun doSigSynopsis (_, ppStrm) = (
		PP.openVBox ppStrm (PP.Abs 0);
		  PPSp.ppSignature(ppStrm, sigId);
		  ppInstances ppStrm instances;
		  PP.cut ppStrm;
		PP.closeBox ppStrm)
	  fun doSigInterface (_, ppStrm) = (
		PP.openVBox ppStrm (PP.Abs 0);
		  PPSp.ppSpecs (inSpec ctx, ppStrm, true) specs;
		PP.closeBox ppStrm)
	  in
	    doSynopsis (ctx, doSigSynopsis);
	    doInterface (ctx, doSigInterface);
	    doDescription (ctx, [sigId], specs)
	  end

   fun doStructure (ctx0, path, strId, status, opaque, body) = (case body
	   of MM.SIG_ID{sigId, doc, whereTypes} => let
		val strName = String.concatWith "." (path@[strId])
		fun doStructSynopsis (ppCtx, ppStrm) = (
		      PP.openVBox ppStrm (PP.Abs 0);
			PPSp.ppSignature(ppStrm, sigId); PP.newline ppStrm;
			PPSp.ppStructure (ppCtx, ppStrm)
			  (strName, opaque, SOME sigId, whereTypes);
		      PP.closeBox ppStrm)
		in
		  doSynopsis (ctx0, doStructSynopsis)
		end
	    | MM.SIG_SPEC{sigId, specs} => let
		val qid = path@[strId]
		val strName = String.concatWith "." qid
		val specs = Sp.getSigbodyContents specs
		val ctx = (case path
		       of [] => C.withTopStr(ctx0, strId)
			| _ => C.withStrPath(ctx0, qid)
		      (* end case *))
		fun doStructSynopsis (ppCtx, ppStrm) = (
		      PP.openVBox ppStrm (PP.Abs 0);
			case sigId
			 of (SOME id) => (
			      PPSp.ppSignature(ppStrm, id);
			      PP.newline ppStrm)
			  | NONE => ()
			(* end case *);
			PPSp.ppStructure (ppCtx, ppStrm) (strName, opaque, sigId, []);
		      PP.closeBox ppStrm)
		fun doStructInterface (ppCtx, ppStrm) = (
		      PP.openVBox ppStrm (PP.Abs 0);
			PPSp.ppSpecs (ppCtx, ppStrm, true) specs;
		      PP.closeBox ppStrm)
		in
		  doSynopsis (ctx0, doStructSynopsis);
		  doInterface (ctx, doStructInterface);
		  doDescription (ctx, qid, specs)
		end
	  (* end case *))

    fun doFunctor (ctx, fctId, status, argSpecs, opaque, resSpecs) = let
	  val (argSigId, argSpecs) = (case argSpecs
		 of (MM.FCTARG_ID{strId, sigId, ...}) =>
		      (SOME strId, [])
		  | (MM.FCTARG_SPEC{sigId, specs}) =>
		      (sigId, Sp.getSigbodyContents specs)
		(* end case *))
	  val (resSigId, resWhereTys, resSpecs) = (case resSpecs
		 of MM.SIG_ID{sigId, whereTypes, ...} =>
		      (SOME sigId, whereTypes, [])
		  | MM.SIG_SPEC{sigId, specs, ...} =>
		      (sigId, [], Sp.getSigbodyContents specs)
		(* end case *))
	  fun optSigSynopsis (ppStrm, NONE) = ()
	    | optSigSynopsis (ppStrm, SOME sigId) = (
		PPSp.ppSignature(ppStrm, sigId); PP.newline ppStrm)
	  fun doFctSynopsis (ppCtx, ppStrm) = (
		optSigSynopsis (ppStrm, argSigId);
		optSigSynopsis (ppStrm, resSigId);
		PPSp.ppFunctor (ppCtx, ppStrm)
		  (fctId, argSigId, opaque, resSigId, resWhereTys))
	  fun doFctArgInterface (ppCtx, ppStrm) = (
		PP.openVBox ppStrm (PP.Abs 0);
		PPSp.ppSpecs (ppCtx, ppStrm, true) argSpecs;
		PP.closeBox ppStrm)
	  fun doFctResInterface (ppCtx, ppStrm) = (
		PP.openVBox ppStrm (PP.Abs 0);
		PPSp.ppSpecs (ppCtx, ppStrm, true) resSpecs;
		PP.closeBox ppStrm)
	  in
	    doSynopsis (ctx, doFctSynopsis);
	    if not(null argSpecs)
	      then let
		val ctx = C.withFctArg(ctx, fctId)
		in
		  doFctInterface (ctx, "Argument", doFctArgInterface);
		  doDescription (ctx, [fctId], argSpecs)
		end
	      else ();
	    if not(null resSpecs)
	      then let
		val ctx = C.withFct(ctx, fctId)
		in
		  doFctInterface (ctx, "Result", doFctResInterface);
		  doDescription (ctx, [fctId], resSpecs)
		end
	      else ()
	  end

  (* emit labels for the components being defined here *)
    fun doModuleLabels (ctx, module) = let
	  fun prLabel (id, kind) = prf(ctx, "\\label{sec:%s:%s}\n", [
		  F.STR(LaTeXTranslate.transLabelData id), F.STR kind
		])
	  fun labelSigbody (MM.SIG_SPEC{sigId=SOME id, ...}) =
		prLabel (id, "sig")
	    | labelSigbody _ = ()
	  in
	    case module
	     of (MM.SIG{sigId, structs, ...}) => (
		  prLabel (sigId, "sig");
		  List.app (fn {strId, ...} => prLabel (strId, "str")) structs)
	      | (MM.TOPSTR{strId, sigBody, ...}) => (
		  prLabel (strId, "str"); labelSigbody sigBody)
	      | (MM.SUBSTR{context, strId, sigBody, ...}) => let
		  val id = concat(
			List.foldr (fn (s, l) => s :: "." :: l) [strId] context)
		  in
		    prLabel (id, "str"); labelSigbody sigBody
		  end
	      | (MM.FCT{fctId, argSpecs, resSpecs, ...}) => (
		  prLabel (fctId, "fct");
(* FIXME: do argSpecs!! *)
		  labelSigbody resSpecs)
	    (* end case *)
	  end

    fun doModule (ctx, module) = (case module
	   of (MM.SIG{sigId, status, specs, structs}) =>
		doSignature (ctx, sigId, status, specs, structs)
	    | (MM.TOPSTR{strId, status, opaque, sigBody}) =>
		doStructure (ctx, [], strId, status, opaque, sigBody)
	    | (MM.SUBSTR{context, strId, opaque, sigBody}) =>
		doStructure (ctx, context, strId, MM.OTHER, opaque, sigBody)
	    | (MM.FCT{fctId, status, argSpecs, opaque, resSpecs}) =>
		doFunctor (ctx, fctId, status, argSpecs, opaque, resSpecs)
	  (* end case *))

  (* SEEALSO ::=  ((%IDREF)+) *)
    fun doSeeAlso (ctx, []) = ()
      | doSeeAlso (ctx, first::rest) = let
	  val doItem = Txt.doSeeAlso ctx
	  fun doAll [] = pr (ctx, "\n")
	    | doAll (item::r) = (pr(ctx, ", "); doItem item; doAll r)
	  in
	    pr(ctx, "\\begin{mldSEEALSO}%\n");
	    doItem first; doAll rest;
	    pr(ctx, "\\end{mldSEEALSO}%\n")
	  end

    fun doFile {config, master, nodeMap, outStrm, doc, level} = let
	  val standalone = MLDocConfig.getFlag(config, ["Standalone"])
	  val {body, ...} = Doc.getContents doc
	  val ctx = LaTeXContext.context {
		  outS = outStrm, index = master,
		  config = config, nodeMap = nodeMap
		}
	  fun doDocSect level docSect = (case docSect
		 of (Doc.SECTION{title, label, style, id, content}) => (
		      doHeading config (ctx, level, style, title);
		      doLabel (ctx, label);
		      app (doElement (level+1)) content)
		  | (Doc.INCLFILE fname) =>
		      prf (ctx, "\\input{%s}\n", [F.STR fname])
		  | (Doc.INTERFACE{title, label, id, seealso, pre, module, post}) =>
		      let
		      val m = MM.parseModule module
		      in
			doModuleHeading config (ctx, level, Doc.NUMBER_SEC, title);
(* FIXME: if label is NONE, we should synthesize something! *)
			doLabel (ctx, label);
		      (* emit labels for the components being defined here *)
			doModuleLabels (ctx, m);
			if (not(null pre))
			  then List.app (doElement (level+1)) pre
			  else ();
			doModule (ctx, m);
			if (not(null post))
			  then (
			    pr(ctx, "\\begin{mldDISCUSSION}%");
			    List.app (doElement (level+1)) post;
			    pr(ctx, "\\end{mldDISCUSSION}%\n"))
			  else ();
			doSeeAlso (ctx, seealso)
		      end
		(* end case *))
	  and doElement level elem = (case elem
		 of (Doc.DOCSECT docSect) => (pr(ctx, "\n"); doDocSect level docSect)
		  | (Doc.FLOAT flt) => (pr(ctx, "\n"); Txt.doFloat ctx flt)
		  | (Doc.PARA pp) => Txt.doPP ctx pp
		(* end case *))
	  in
	    if standalone
	      then (
		pr(ctx, "\\documentclass{mldoc-book}\n");
		pr(ctx, "\\begin{document}\n");
		  app (doDocSect level) body;
		pr(ctx, "\\end{document}\n"))
	      else app (doDocSect level) body
	  end

  end
