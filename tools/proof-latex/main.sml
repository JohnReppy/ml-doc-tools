(* main.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A tool for extracting compact 'proof' latex output from ML-Doc documents
 *
 * Parameters controlling output are:
 *   commentSep
 *   indexFlag
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure C = MLDocConfig
    structure I = MLDocIndex
    structure FT = FileTree
    structure Run = RunMLDocSGMLS
    structure F = Format
    structure P = OS.Path
    structure IO = TextIO

    fun doInputFile config = let
	  val standalone = C.getFlag(config, ["Standalone"])
	  val index = C.getFlag(config, ["Index"])
	  val outDir = Option.valOf(C.getStr(config, ["OutDir"]))
	  val masterFileName = valOf(C.getStr(config, ["MasterInfoFile"]))
	  val basename = Run.basename config
	  val parse = Run.parseFile {config=config, includeEntities=[]}
	  val masterFileIndex = I.parse masterFileName
	  val nodeMap = #nodeMap(FT.mkForest masterFileIndex)
	  fun fileLevel fileName = let
		val fileEntry =
		      valOf(I.findFile(masterFileIndex, Atom.atom fileName))
		val FT.FILE{level, ...} = nodeMap fileEntry
		in
		  level
		end
	  fun doFile inFile = let
		val {path, base, dir} = basename inFile
		val outputPath = OS.Path.joinDirFile{
			dir = OS.Path.concat(outDir, dir),
			file = OS.Path.joinBaseExt{base = base, ext = SOME "tex"}
		      }
		val level = fileLevel(OS.Path.concat(dir, base))
		val _ = IO.output(IO.stdOut, concat[
			inFile, " level = ", Int.toString level, " ...\n"
		      ])
		in
		  let val outStrm = IO.openOut outputPath
		  in
		    (DoFile.doFile 
		       {config=config,
			standalone = standalone,
			index = index,
			doc = parse inFile,
			outStrm = outStrm,
			level = level
		      }) handle ex => (IO.closeOut outStrm; raise ex);
		    IO.closeOut outStrm
		  end
		    handle (Fail msg) => DoFile.error (
			      F.format "File \"%s\": %s" [F.STR inFile, F.STR msg])
		         | e => DoFile.error (
			      F.format "File \"%s\": uncaught exn %s" [
				  F.STR inFile , F.STR (exnName e)
			        ])
	      end
	  in
	    doFile
	  end

    val options = StdOptions.stdOptions @ StdOptions.latexOptions @ [
	    StdOptions.outDir "Proof",
	    { flag		= C.FLG "-doc",
	      configName	= ".Standalone",
	      default		= C.NOVAL
	    },
	    { flag		= C.FLG "-index",
	      configName	= ".Index",
	      default		= C.NOVAL
	    },
            { flag		= C.NA,
	      configName	= "TopLevelSection",
	      default		= C.STR "Chapter"
	    }
	  ]

    fun doOptions args = let
	  val {config, otherArgs} = C.configure {
		  appl = "Proof-LaTeX",
		  opts = options,
		  args = args
		}
	  in
	    {config = config, files = otherArgs}
	  end

    fun main (cmdName, cmdLine) = let
	  val {config, files} = doOptions cmdLine
	  in
	    List.app (doInputFile config) files;
	    OS.Process.success
	  end
	    handle e => (Error.uncaughtExn e; OS.Process.failure)

  end
