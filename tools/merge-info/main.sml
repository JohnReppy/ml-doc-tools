(* main.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    structure C = MLDocConfig

    val options = StdOptions.stdOptions @ [
	    StdOptions.outFile "Info/Master.info"
	  ]

    fun doOptions args = let
	  val {config, otherArgs} = C.configure {
		  appl = "Merge-Info",
		  opts = options,
		  args = args
		}
	  in
	    {config = config, files = otherArgs}
	  end

    fun main (cmdName, cmdLine) = let
	  val {config, files} = doOptions cmdLine
	  val masterPath = valOf(C.getStr(config, ["OutFile"]))
	  val info = BuildIndex.index()
	  fun merge fileName = BuildIndex.merge (MLDocIndex.parse fileName, info)
	  in
	    List.app merge files;
	    MLDocIndex.print (masterPath, info);
	    OS.Process.success
	  end
	    handle exn => (Error.uncaughtExn exn; OS.Process.failure)

  end;

