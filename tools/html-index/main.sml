(* main.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Options:
 *      -dir path	specify the output directory
 *	-o path		specify output file (default is by index type)
 *	-info path	specify input file (default "INDEX")
 *	-all		generate unified alphabetical index ("index-all.html")
 *	-sig		generate signature index ("index-sig.html")
 *	-struct		generate structure index ("index-struct.html")
 *	-exn		generate exception index ("index-exn.html")
 *	-type		generate type index ("index-type.html")
 *	-val		generate value index ("index-val.html")
 *	-cols n		specify the number of index columns (default 3)
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    val applName = "HTML-Index"

    structure F = Format
    structure C = MLDocConfig
    structure E = IndexEntry
    structure G = IndexGroup

    fun title E.AllIdx = "Unified identifier index"
      | title E.SigIdx = "Signature index"
      | title E.StrIdx = "Structure and functor index"
      | title E.ExnIdx = "Exception index"
      | title E.TyIdx = "Type index"
      | title E.ValIdx = "Value and constructor index"

    fun mkPage (title, rest) = HTML.HTML{
	    version = SOME HTML.htmlVersion,
	    head = [HTML.Head_TITLE title],
	    body = HTML.BODY{
		background = NONE, bgcolor = NONE, text = NONE,
		link = NONE, vlink = NONE, alink = NONE,
		content = HTML.BlockList(MakeHTML.mkH(1, title) :: rest)
	      }
	  }

    fun indexToHTML {config, kind, ncols} = let
	  val ctx = HTMLContext.mkContext {config = config, title = title kind}
	  val elems = E.buildList (HTMLContext.index ctx, kind)
	  val groups = G.groupEntries elems
	  fun mkTbl groups = ToHTML.mkIndexTable ctx (G.mkColumns (groups, ncols))
	  in
	    case kind
	     of E.AllIdx => let
		  fun mkTable ((tag, groups), l) =
			MakeHTML.mkH(2, tag) :: mkTbl groups :: MakeHTML.HR :: l
		  in
		    mkPage (
		      title kind,
		      MakeHTML.HR :: List.foldr mkTable [] (G.alphaGroups groups))
		  end
	      | _ => mkPage (title kind, [mkTbl groups])
	    (* end case *)
	  end

    val options = StdOptions.stdOptions @ StdOptions.htmlOptions @ [
	    { flag		= C.ARG "-o",
	      configName	= ".OutFile",
	      default		= C.NOVAL
	    },
	    { flag		= C.ARG "-cols",
	      configName	= ".NumColumns",
	      default		= C.NUM 3
	    }
	  ]

    fun getOpts args = let
	  val {config, otherArgs} = C.configure {
		  appl = applName,
		  opts = options,
		  args = args
		}
	  fun getIndexKind [] = E.AllIdx
	    | getIndexKind ["-all"] = E.AllIdx
	    | getIndexKind ["-sig"] = E.SigIdx
	    | getIndexKind ["-struct"] = E.StrIdx
	    | getIndexKind ["-exn"] = E.ExnIdx
	    | getIndexKind ["-type"] = E.TyIdx
	    | getIndexKind ["-val"] = E.ValIdx
	    | getIndexKind _ = Error.error "usage"
	  val kind = getIndexKind otherArgs
	  val outFile = (case (C.getStr(config, ["HTML", "OutFile"]), kind)
		 of (NONE, E.AllIdx) => "index-all.html"
		  | (NONE, E.SigIdx) => "index-sig.html"
		  | (NONE, E.StrIdx) => "index-struct.html"
		  | (NONE, E.ExnIdx) => "index-exn.html"
		  | (NONE, E.TyIdx) => "index-type.html"
		  | (NONE, E.ValIdx) => "index-val.html"
		  | (SOME f, _) => f
		(* end case *))
	  val outDir = valOf(C.getStr(config, ["OutDir"]))
	  val outFile = (case (OS.Path.splitDirFile outFile)
		 of {dir="", ...} => OS.Path.joinDirFile{dir=outDir, file=outFile}
		  | {dir, file} => outFile
		(* end case *))
	  in
	    { config = config,
	      kind = kind,
	      outFile = outFile,
	      ncols = valOf(C.getInt(config, ["NumColumns"]))
	    }
	  end

    fun main (arg0, args) = let
	  val {config, kind, outFile, ncols} = getOpts args
	  val ctx = HTMLContext.mkContext {
		  config = config, title = title kind
		}
	  val html = indexToHTML{config=config, kind=kind, ncols=ncols}
	  val templateFile = C.getStr(config, [applName, "Template"])
	  val outStrm = TextIO.openOut outFile
	  in
	    PrHTML.prHTML {
		putc = fn c => TextIO.output1(outStrm, c),
		puts = fn s => TextIO.output (outStrm, s)
	      } (HTMLTemplate.getOptTemplate {
		doc = html,
		context = ctx,
		templateFile = templateFile
	      });
	    TextIO.closeOut outStrm;
	    OS.Process.success
	  end
	    handle exn => (Error.uncaughtExn exn; OS.Process.failure)

  end;

