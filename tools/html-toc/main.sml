(* main.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://cs.uchicago.edu/~jhr)
 *
 * Options:
 *      -dir path	specify the output directory
 *	-o path		specify output file (default is toc.html)
 *	-info path	specify input file (default "HTML.info")
 *	-depth n	specify nesting depth of sections listed (default 3)
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    val applName = "HTML-TOC"

    structure F = Format
    structure C = MLDocConfig
    structure T = FileTree
    structure Doc = MLDocContent

    fun mkPage (title, rest) = HTML.HTML{
	    version = SOME HTML.htmlVersion,
	    head = [HTML.Head_TITLE title],
	    body = HTML.BODY{
		background = NONE, bgcolor = NONE, text = NONE,
		link = NONE, vlink = NONE, alink = NONE,
		content = HTML.BlockList(HTML.Hn{
		    n=2, align=SOME HTML.HAlign.center, content=HTML.PCDATA title
		  } :: rest)
	      }
	  }

  (* make an entry of the TOC *)
    fun mkEntry (title, []) = MakeHTML.mkLI(HTML.TextBlock title)
      | mkEntry (title, items) =
	  MakeHTML.mkLI(HTML.BlockList[HTML.TextBlock title, MakeHTML.mkUL items])

(** Things to think about:
 **  -- don't exceed sectional nesting level
 **  -- add a fileContext function to HTMLContext?
 **)
    fun genHTML {ctx, depth} = let
	  val config = HTMLContext.config ctx
	  val mldocDir = valOf(C.getStr(config, ["MLDocDir"]))
	  val parse = RunMLDocSGMLS.parseFile {
		  config = config, includeEntities = []
		}
	  val {roots, nodeMap, parentMap} = T.mkForest(HTMLContext.index ctx)
	(* recursively walk a file; dir is the path to the file relative to the
	 * root directory of the ML-Doc sources.
	 *)
	  fun walkFile (dir, fileName, l) = let
		val fileName = OS.Path.concat(dir, fileName)
		val path = OS.Path.joinBaseExt {
			base = OS.Path.concat(mldocDir, fileName),
			ext = SOME "mldoc"
		      }
		val doc = parse path
		val {body, ...} = Doc.getContents doc
		val ctx = HTMLContext.fileCtx(ctx, {fileName=fileName, doc=doc})
		in
		  foldr (walkDocSect (ctx, dir)) l body
		end
	  and walkDocSect (ctx, dir) (Doc.SECTION{style=Doc.NOTOC_SEC, ...}, l) = []
	    | walkDocSect (ctx, dir) (Doc.SECTION{title, label, id, content, ...}, l) =
		let
		val text =
		      HRefs.mkSectRef (HTMLContext.srcFile ctx, label, id,
		        MakeHTML.textList(map (TextToHTML.inlineToHTML ctx) title))
		val ctx' = HTMLContext.sectionCtx ctx
		val body = if (HTMLContext.sectionAbsLevel ctx' < depth)
		      then List.foldr (walkDocElem (ctx', dir)) [] content
		      else []
		in
		  mkEntry(text, body) :: l
		end
	    | walkDocSect (ctx, dir) (Doc.INCLFILE fileName, l) = let
		val {dir=dir', file} = OS.Path.splitDirFile fileName
		in
		  walkFile(OS.Path.concat(dir, dir'), file, l)
		end
	    | walkDocSect (ctx, dir) (Doc.INTERFACE{title, label, id, ...}, l) =
		mkEntry (
		  HRefs.mkSectRef (HTMLContext.srcFile ctx, label, id,
		    MakeHTML.textList(map (TextToHTML.inlineToHTML ctx) title)),
		  []) :: l
	  and walkDocElem (ctx, dir) (Doc.DOCSECT s, items) =
		walkDocSect (ctx, dir) (s, items)
	    | walkDocElem (ctx, dir) (_, items) = items
	  fun walkRoot (T.FILE{file, ...}, l) =
		walkFile ("", Atom.toString(MLDocIndex.File.name file), l)
	  in
	    case (foldr walkRoot [] roots)
	     of [HTML.LI{content, ...}] => content
	      | items => MakeHTML.mkUL items
	    (* end case *)
	  end

    val options = StdOptions.stdOptions @ StdOptions.htmlOptions @ [
	    StdOptions.outFile "toc.html",
	    { flag		= C.ARG "-depth",
	      configName	= ".Depth",
	      default		= C.NUM 3
	    }
	  ]

    fun getOpts args = let
	  val {config, otherArgs} = C.configure {
		  appl = applName,
		  opts = options,
		  args = args
		}
	  val outFile = valOf(C.getStr(config, [applName, "OutFile"]))
	  val outDir = valOf(C.getStr(config, ["OutDir"]))
	  val outFile = (case (OS.Path.splitDirFile outFile)
		 of {dir="", ...} => OS.Path.joinDirFile{dir=outDir, file=outFile}
		  | {dir, file} => outFile
		(* end case *))
	  in
	    { config = config,
	      outFile = outFile,
	      depth = valOf(C.getInt(config, ["Depth"]))
	    }
	  end

    fun main (arg0, args) = let
	  val {config, outFile, depth} = getOpts args
	  val ctx = HTMLContext.mkContext {
		  config = config, title = "Table of Contents"
		}
	  val html =
		mkPage ("Table of Contents", [genHTML {ctx = ctx, depth = depth}])
	  val templateFile = C.getStr(config, [applName, "Template"])
	  val outStrm = TextIO.openOut outFile
	  in
	    PrHTML.prHTML {
		putc = fn c => TextIO.output1(outStrm, c),
		puts = fn s => TextIO.output(outStrm, s)
	      } (HTMLTemplate.getOptTemplate {
		doc = html,
		context = ctx,
		templateFile = templateFile
	      });
	    TextIO.closeOut outStrm;
	    OS.Process.success
	  end
	    handle exn => (Error.uncaughtExn exn; OS.Process.failure)

  end

