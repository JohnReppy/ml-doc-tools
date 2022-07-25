(* main.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A tool for extracting HTML documents from the ML-Doc.
 *)

structure Main : sig

    val test : string -> unit

    val main : (string * string list) -> OS.Process.status

  end = struct

    val applName = "HTML-Gen"

    structure F = Format
    structure C = MLDocConfig
    structure Run = RunMLDocSGMLS

    val options = StdOptions.stdOptions @ StdOptions.htmlOptions
	  @ [
	      { flag		= C.ARG "-wid",
		configName	= ".PreWid",
		default		= C.NUM 60
	      }
	    ]

    fun doInputFile config = let
	  val root = C.getStr(config, ["Root"])
	  val outDir = valOf(C.getStr(config, ["OutDir"]))
	  val templateFile = C.getStr(config, [applName, "Template"])
	  val basename = Run.basename config
	  fun doFile inFile = let
		val {path, base, dir} = basename inFile
		val htmlFile = OS.Path.joinDirFile{
			dir = OS.Path.concat(outDir, dir),
			file = OS.Path.joinBaseExt{base = base, ext = SOME "html"}
		      }
		in
		  DoFile.doit {
		      config = config,
		      inFile = inFile,
		      baseName = OS.Path.concat(dir, base),
		      outFile = htmlFile,
		      templateFile = templateFile
		    }
		end
		  handle OS.SysErr(msg, _) =>
		    Error.error (F.format "File \"%s\": %s\n" [
			F.STR inFile, F.STR msg
		      ])
	  in
	    doFile
	  end

    fun doOptions args = let
	  val {config, otherArgs} = C.configure {
		  appl = applName,
		  opts = options,
		  args = args
		}
	  in
	    DoFile.debugFlg := C.getFlag(config, ["Debug"]);
	    DoSpec.preWid := Option.valOf(C.getInt(config, ["PreWid"]));
	    {config = config, files = otherArgs}
	  end

    fun main (cmdName, cmdLine) = let
	  val {config, files} = doOptions cmdLine
	  in
	    List.app (doInputFile config) files;
	    OS.Process.success
	  end
	    handle e => (Error.uncaughtExn e; OS.Process.failure)

    fun test f = let
	  val {config, ...} = doOptions []
	  in
	    doInputFile config f
	  end

  end

