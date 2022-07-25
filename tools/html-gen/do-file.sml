(* do-file.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A tool for extracting HTML documents from the ML-Doc.
 *)

structure DoFile : sig

    val debugFlg : bool ref

    val doit : {
	    config : MLDocConfig.configuration,
	    inFile : string,
	    baseName : string,
	    outFile : string,
	    templateFile : string option
	  } -> unit

  end = struct

    structure F = Format
    structure Run = RunMLDocSGMLS

    structure E = MLDocElem
    structure P = MLDocParser
    structure M = P.Markup
    structure MM = MLDocModule
    structure I = MLDocIndex
    structure C = HTMLContext
    structure Mk = MakeHTML
    structure PP = HTMLPPSpec.PPStrm

    structure SS = Substring

    val debugFlg = ref false
    fun debug msg = if !debugFlg then TextIO.print msg else ()
    fun debugf (fmt, items) =
	  if !debugFlg then TextIO.print(F.format fmt items) else ()

    fun error msg = (
	  TextIO.output(TextIO.stdErr, concat["Error: ", msg, "\n"]);
	  TextIO.flushOut TextIO.stdErr;
	  raise Fail "error")

  (* Heading levels for various elements; eventually, these should come
   * from the configuration file.
   *)
    val hdrBaseLvl = 2
    val synopsisHLvl = 4
    val interfaceHLvl = 4
    val descriptionHLvl = 4
    val discussHLvl = 4
    val seeAlsoHLvl = 4
    val subsectHLvl = 4

  (* Usefule HTML tokens *)
    val nbsp = HTML.PCDATA "&nbsp;"
    val nbsp2 = HTML.PCDATA "&nbsp;&nbsp;"

    fun sigBodyToHTML ctx (optHdr, specs) = let
	  val hdr = (case optHdr
		 of NONE => []
		  | (SOME h) => [Mk.mkH(interfaceHLvl, h)])
	  val ppToHTML = TextToHTML.ppToHTML ctx
	  val {sigText, descText} = DoSpec.doSigBody ctx specs
	  val desc = (case descText
		 of (HTML.DL _) => [Mk.mkH(descriptionHLvl, "Description"), descText]
		  | _ => []
		(* end case *))
	  in
	    debug "  sigBodyToHTML\n";
	    hdr @ (sigText :: desc)
	  end

    fun glueHTML ctx {synopsis, interfaces, pre, post, seealso} = let
	  val inlineToHTML = TextToHTML.inlineToHTML ctx
	  in
	    Mk.HR :: synopsis @ pre @
	      (case interfaces
	         of [resSig] => Mk.HR :: resSig
		  | [argSig, resSig] => Mk.HR :: argSig @ (Mk.HR :: resSig)
		  | [] => []
	      (* end case *)) @
	      (case seealso
	         of [] => []
		  | (first::rest) => let
		      fun f (x, l) = (HTML.PCDATA ", ") :: (inlineToHTML x) :: l
		      in
		        [ Mk.mkH(seeAlsoHLvl, "See Also"),
		          HTML.BLOCKQUOTE(
			      HTML.TextBlock(Mk.textList(
			        inlineToHTML first :: (List.foldr f [] rest)))
			    )
		        ]
		      end
		  (* end case *)) @
	      (case post
	         of [] => []
		  | _ => Mk.mkH(discussHLvl, "Discussion") :: post
	      (* end case *))
	  end

  (* convert a unit's status to HTML markup; this function is used for adding
   * annotations to the synopsis.
   *)
    fun statusToHTML MM.OPTIONAL = [
	    nbsp2,
	    HTML.FONT{
		size = NONE, color = SOME "#FF0000",
		content = HTML.CODE(HTML.I(HTML.PCDATA "(* OPTIONAL *)"))
	      }
	  ]
      | statusToHTML _ = []

    fun docBodyToHTML (dir, ctx) docBody = (
	  debug "docBodyToHTML\n";
	  Mk.blockList (List.map (docSectToHTML (dir, ctx)) docBody))

    and docSectToHTML (dir, ctx) (
	  MLDocContent.SECTION{title, label, style, id, content}
	) = (
	  debug "docSectToHTML: SECTION\n";
	  Mk.blockList (
	    sectToHTML (
	      dir, C.sectionCtx ctx, C.sectionRelLevel ctx, title, label, id, content)))
      | docSectToHTML (dir, ctx) (MLDocContent.INCLFILE file) = (
	  debug "docSectToHTML: INCLFILE\n";
(***** NEED TO HANDLE EMPTY FILES (AND NESTED EMPTY FILES) *****)
	  case (C.findFile (ctx, OS.Path.concat(dir, file)))
	   of (SOME(FileTree.FILE{file=info, level, ...})) => HTML.Hn{
		  n = hdrBaseLvl + C.sectionRelLevel ctx + 1,
		  align = NONE,
		  content = Mk.mkA_HREF{
		      href = file ^ ".html",
		      content =
			HTML.PCDATA(TextToHTML.transData(C.I.File.title info))
		    }
		}
	    | NONE => (
		Error.warning("cannot find file \"%s\" in index", [F.STR file]);
		Mk.blockList[])
	  (* end case *))
      | docSectToHTML (dir, ctx) (MLDocContent.INTERFACE stuff) = let
	  val inlineToHTML = TextToHTML.inlineToHTML ctx
	  val lvl = C.sectionRelLevel ctx
	  val {title, seealso, pre, module, post, ...} = stuff
	  val hd = HTML.Hn{
		  n = hdrBaseLvl + lvl, align = NONE,
		  content = Mk.textList (
		    HRefs.mkSectAnchor (#label stuff, #id stuff)
		      :: (map inlineToHTML title))
		}
	  fun elemToHTML (MLDocContent.FLOAT flt) = (
		debug "docSectToHTML.elemToHTML: FLOAT\n";
		[TextToHTML.floatToHTML ctx flt])
	    | elemToHTML (MLDocContent.PARA m) =(
		debug "docSectToHTML.elemToHTML: PARA\n";
		TextToHTML.ppToHTML ctx [m])
	  in
	    debug "docSectToHTML: INTERFACE\n";
	    HTML.BlockList(hd :: moduleToHTML ctx {
		pre = List.foldr (fn (e, l) => (elemToHTML e) @ l) [] pre,
		module = MM.parseModule module,
		post = List.foldr (fn (e, l) => (elemToHTML e) @ l) [] post,
		seealso = seealso
	      })
	  end

    and sectToHTML (dir, ctx, lvl, title, label, id, contents) = let
	  val inlineToHTML = TextToHTML.inlineToHTML ctx
	  val hd = HTML.Hn{
		  n = hdrBaseLvl + lvl, align = NONE,
		  content = Mk.textList (
		    HRefs.mkSectAnchor (label, id) :: (map inlineToHTML title))
		}
	  fun elemToHTML (MLDocContent.DOCSECT s) = [docSectToHTML (dir, ctx) s]
	    | elemToHTML (MLDocContent.FLOAT flt) = (
		debug "sectToHTML.elemToHTML: FLOAT\n";
		[TextToHTML.floatToHTML ctx flt])
	    | elemToHTML (MLDocContent.PARA m) =(
		debug "sectToHTML.elemToHTML: PARA\n";
		TextToHTML.ppToHTML ctx [m])
	  in
	    debug "sectToHTML\n";
	    hd :: (List.foldr (fn (e, l) => (elemToHTML e) @ l) [] contents)
	  end

    and moduleToHTML ctx {pre, module, post, seealso} = let
(* do we want to include the result signature in the title, and if so, what
 * about the functor argument signature?  We should at least provide
 * SPEC anchors for all named modules and signatures.
 *)
	  fun mkAnchor (kind, id) = HRefs.mkAnchor (HRefs.specURL ctx {
		  isRef = false, kind = kind, id = id
		})
	  fun sigSynopsis (sigId, sts) = [
		  HTML.B(HTML.PCDATA "signature"),
		  nbsp,
		  mkAnchor (HRefs.SigRef, sigId),
		  HTML.CODE(TextToHTML.idToHTML sigId)
		] @ statusToHTML sts @ [Mk.BR]
	  val anonSig = [
		  HTML.B(HTML.PCDATA " : sig"), HTML.PCDATA " ... ",
		  HTML.B(HTML.PCDATA "end")
		]
	  val anonOpaqueSig = [
		  HTML.B(HTML.PCDATA " :> sig"), HTML.PCDATA " ... ",
		  HTML.B(HTML.PCDATA "end")
		]
	  fun whereTySynopsis [] = [Mk.BR]
	    | whereTySynopsis wtls = let
		val ppDev = HTMLDev.openDev {wid = !DoSpec.preWid, textWid = NONE}
		val ppStrm = HTMLPPStrm.openStream ppDev
		val ppCtx = {inSpec=false, ctx=ctx}
		in
		  PP.openVBox ppStrm (PP.Abs 0);
		  PP.openVBox ppStrm (PP.Abs 2);
		    HTMLPPSpec.ppWhereTys (ppCtx, ppStrm) wtls;
		  PP.closeBox ppStrm; PP.cut ppStrm;
		  PP.closeBox ppStrm;
		  HTMLPPStrm.closeStream ppStrm;
		  [HTMLDev.done ppDev]
		end
	  fun strSynopsis (strId, opaque, SOME sigId, sts, whereTys) = [
		  HTML.B(HTML.PCDATA "structure"),
		  nbsp,
		  mkAnchor (HRefs.StrRef, strId),
		  HTML.CODE(TextToHTML.idToHTML strId),
		  if opaque
		    then HTML.B(HTML.PCDATA " :> ")
		    else HTML.B(HTML.PCDATA " : "),
(** this should be a reference in some places **)
		  HTML.PCDATA sigId
		] @ statusToHTML sts @ whereTySynopsis whereTys
	    | strSynopsis (strId, opaque, NONE, sts, whereTys) = [
		  HTML.B(HTML.PCDATA "structure"),
		  nbsp,
		  mkAnchor (HRefs.StrRef, strId),
		  HTML.CODE(TextToHTML.idToHTML strId)
		] @ (if opaque then anonOpaqueSig else anonSig)
		  @ statusToHTML sts @ whereTySynopsis whereTys
	  fun fctSynopsis (fctId, optArgId, opaque, optResId, sts) = let
		val prefix = [
			HTML.B(HTML.PCDATA "functor"),
			nbsp,
			mkAnchor (HRefs.FctRef, fctId),
			HTML.CODE(TextToHTML.idToHTML fctId)
		      ]
		val arg = (case optArgId
		       of NONE => [
			      HTML.B(HTML.PCDATA " ("), HTML.PCDATA " ... ",
			      HTML.B(HTML.PCDATA ")")
			    ]
			| (SOME id) => [
			      HTML.B(HTML.PCDATA " ("), HTML.PCDATA id,
			      HTML.B(HTML.PCDATA ")")
			    ]
		      (* end case *))
		val res = (case (opaque, optResId)
		       of (true, NONE) => anonOpaqueSig
			| (false, NONE) => anonSig
			| (true, SOME id) => [
			      HTML.B(HTML.PCDATA " :> "), HTML.PCDATA id
			    ]
			| (false, SOME id) => [
			      HTML.B(HTML.PCDATA " : "), HTML.PCDATA id
			    ]
		      (* end case *))
		in
		  prefix @ arg @ res @ statusToHTML sts @ [Mk.BR]
		end
	  fun strAndSigSynopsis (strId, opaque, NONE, sts) =
		[strSynopsis (strId, opaque, NONE, sts, [])]
	    | strAndSigSynopsis (strId, opaque, SOME sigId, sts) = [
		  sigSynopsis(sigId, sts),
		  strSynopsis (strId, opaque, SOME sigId, sts, [])
		]
	  fun mkSynopsis l = [
		  Mk.mkH(synopsisHLvl, "Synopsis"),
		  HTML.BLOCKQUOTE(
		    HTML.TextBlock(HTML.CODE(HTML.TextList(List.concat l))))
		]
	  fun glue {synopsis, interfaces} = glueHTML ctx {
		  synopsis=synopsis, interfaces=interfaces,
		  pre = pre, post =post, seealso=seealso
		}
	  fun mkQId (context, strId) = String.concat (
		strId :: List.foldr (fn (id, l) => "."::id::l) [] context)
	  in
	    debug ("moduleToHTML\n");
	    case module
	     of MM.SIG{sigId, status, specs, structs} =>
		  glue {
(*
		      title = mkTitle (HRefs.SigRef, sigId, NONE),
*)
(**** NOTE: we need to do something with the sharing constraints here!!! ****)
		      synopsis = mkSynopsis (
			sigSynopsis (sigId, status)
			:: map (fn {strId, opaque, status, whereTypes, ...} =>
			    strSynopsis (strId, opaque, SOME sigId, status, whereTypes))
			  structs),
		      interfaces = [
			  sigBodyToHTML (C.withSig(ctx, sigId))
			    (SOME "Interface", specs)
			]
		    }
	      | MM.TOPSTR{strId, status, opaque, sigBody=MM.SIG_ID{sigId, whereTypes, ...}} =>
		  glue {
(*
		      title = mkTitle (HRefs.StrRef, strId, SOME sigId),
*)
		      synopsis =
			mkSynopsis [strSynopsis (strId, opaque, SOME sigId, status, whereTypes)],
		      interfaces = []
		    }
	      | MM.TOPSTR{strId, status, opaque, sigBody=MM.SIG_SPEC{sigId, specs}} =>
		  glue {
(*
		      title = mkTitle (HRefs.StrRef, strId, sigId),
*)
		      synopsis =
			mkSynopsis (strAndSigSynopsis (strId, opaque, sigId, status)),
		      interfaces = [
			  sigBodyToHTML (C.withTopStr(ctx, strId))
			    (SOME "Interface", specs)
			]
		    }
	      | MM.SUBSTR{context, strId, opaque, sigBody=MM.SIG_ID{sigId, whereTypes, ...}} =>
		  glue {
(*
		      title = mkTitle(HRefs.StrRef, mkQId(context, strId), SOME sigId),
*)
		      synopsis = mkSynopsis [
			  strSynopsis (strId, opaque, SOME sigId, MM.REQUIRED, whereTypes)
			],
		      interfaces = []
		    }
	      | MM.SUBSTR{context, strId, opaque, sigBody=MM.SIG_SPEC{sigId, specs}} =>
		  glue {
(*
		      title = mkTitle(HRefs.StrRef, mkQId(context, strId), sigId),
*)
		      synopsis =
			mkSynopsis (
			  strAndSigSynopsis (strId, opaque, sigId, MM.REQUIRED)),
		      interfaces = [
			  sigBodyToHTML (C.withStrPath(ctx, context@[strId]))
			    (SOME "Interface", specs)
			]
		    }
	      | MM.FCT{fctId, status, argSpecs, opaque, resSpecs} => let
		  fun doOptSig (_, _, MM.SIG_ID{sigId, ...}) =
			(SOME sigId, [], [])
		    | doOptSig (ctx, hdr, MM.SIG_SPEC{sigId=NONE, specs}) =
			( NONE,
			  [sigBodyToHTML ctx (SOME hdr, specs)],
			  []
			)
		    | doOptSig (ctx, hdr, MM.SIG_SPEC{sigId=SOME sigId, specs}) =
			( SOME sigId,
			  [sigBodyToHTML ctx (SOME hdr, specs)],
			  [sigSynopsis(sigId, status)]
			)
		  val (argId, argAPI, argSigSynopsis) = (case argSpecs
			 of MM.FCTARG_ID{strId, sigId, ...} =>
			      (SOME sigId, [], [])
			  | (MM.FCTARG_SPEC spec) =>
			      doOptSig (
				C.withFctArg (ctx, fctId),
				"Functor argument interface",
				MM.SIG_SPEC spec)
			(* end case *))
		  val (resId, resAPI, resSigSynopsis) = doOptSig (
			C.withFct (ctx, fctId),
			"Functor result interface",
			resSpecs)
		  in
		    glue {
(*
			title = mkTitle (HRefs.FctRef, fctId, resId),
*)
			synopsis = mkSynopsis (
			  argSigSynopsis @ resSigSynopsis @ [
			      fctSynopsis (fctId, argId, opaque, resId, status)
			    ]),
			interfaces = argAPI @ resAPI
		      }
		  end
	    (* end case *)
	  end

    fun genHTML (dir, context) (title, body) = let
	  val base = (case C.baseURL context
		 of (SOME url) => [HTML.Head_BASE{href = url}]
		  | NONE => []
		(* end case *))
	  in
	    HTML.HTML{
		version = SOME "3.2",
		head = HTML.Head_TITLE(TextToHTML.transData title)
		  :: HTML.Head_META {
		    httpEquiv = NONE,
		    name = SOME "generator", content = "ML-Doc::html-gen"
		  } :: base,
		body = HTML.BODY{
		    background = NONE, bgcolor = NONE, text = NONE,
		    link = NONE, vlink = NONE, alink = NONE,
		    content = docBodyToHTML (dir, context) body
		  }
	      }
	  end

    fun doit {config, inFile, baseName, outFile, templateFile} = let
	  val doc = Run.parseFile {config = config, includeEntities=[]} inFile
	  val context = C.mkSrcContext {
		  config = config,
		  src = {fileName = baseName, doc = doc}
		}
	  val {hdr={title, copyrights, ...}, body} = MLDocContent.getContents doc
	  val outStrm = TextIO.openOut outFile
	  fun pr s = TextIO.output (outStrm, s)
	  fun prf (fmt, arg) = TextIO.output (outStrm, F.format fmt arg)
	  val htmlDoc = genHTML (OS.Path.dir baseName, context) (title, body)
	  in
	    prf ("<!-- %s -->\n", [F.STR outFile]);
	    pr "\n";
	    List.app
	      (fn {owner, year} =>
		prf("<!-- COPYRIGHT (c) %d %s. -->\n", [
		    F.INT year, F.STR owner
		  ])) copyrights;
	    pr "\n";
	    PrHTML.prHTML {
		putc = fn c => TextIO.output1(outStrm, c),
		puts = fn s => TextIO.output(outStrm, s)
	      } (HTMLTemplate.getOptTemplate {
		doc = htmlDoc,
		context = context,
		templateFile = templateFile
	      });
	    TextIO.closeOut outStrm
	  end

  end
