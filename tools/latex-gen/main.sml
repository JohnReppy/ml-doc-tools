(* main.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * A tool for extracting latex output from ML-Doc documents
 *
 * TODO: add a flag to include <QUESTION> elements (see lib/latex/text-to-latex.sml).
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

    fun exnHist e = String.concat (map (fn s => s ^ "\n") (SMLofNJ.exnHistory e))

    fun doInputFile config = let
(*	  val index = C.getFlag(config, ["Index"])*)
	  val outDir = valOf(C.getStr(config, ["OutDir"]))
	  val masterFileName = valOf(C.getStr(config, ["MasterInfoFile"]))
	  val basename = Run.basename config
	  val parse = Run.parseFile {config=config, includeEntities=[]}
	  val masterFileIndex = I.parse masterFileName
	  val nodeMap = #nodeMap(FT.mkForest masterFileIndex)
	  fun fileLevel fileName = let
		val fileEntry = valOf(I.findFile(masterFileIndex, Atom.atom fileName))
		val FT.FILE{level, ...} = nodeMap fileEntry
		in
		  level
		end
	  fun doFile inFile = let
		val {path, base, dir} = basename inFile
	      (* compute the relative name of the file (i.e., with the leading
	       * source directory stripped off.
	       *)
		val fileName = P.concat(dir, base)
		val outputPath = P.joinDirFile{
			dir = P.concat(outDir, dir),
			file = P.joinBaseExt{
			    base = base,
			    ext = SOME "tex"
			  }
		      }
		val level = fileLevel fileName
		val _ = IO.output(IO.stdOut, concat[
			inFile, " level = ", Int.toString level, " ...\n"
		      ])
		in
		  let val outStrm = IO.openOut outputPath
		  in
		    (DoFile.doFile {
			config=config,
			master=masterFileIndex,
			nodeMap = nodeMap,
			doc = parse inFile,
			outStrm = outStrm,
			level = level
		      }) handle ex => (IO.closeOut outStrm; raise ex);
		    IO.closeOut outStrm
		  end
	      end
	  in
	    doFile
	  end

    val options = StdOptions.stdOptions @ StdOptions.latexOptions @ [
	    StdOptions.outDir "Hardcopy",
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
		  appl = "LaTeX-Gen",
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
