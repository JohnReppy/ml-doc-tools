(* mldoc-cmdline.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *)

structure MLDocCmdline : MLDOC_CMDLINE =
  struct

    type option_info = {
	flag : string option,
	envVar : string option,
	configName : string option,
	default : string option
      }

    val stdOptions = [
	    { flag =		SOME "-config",
	      envVar =		SOME "MLDOC_CONFIG",
	      configName =	NONE,
	      default =		NONE
	    },
	    { flag =		SOME "-sgmls",
	      envVar =		SOME "SGMLS_PATH",
	      configName =	SOME "SGMLS_PATH",
	      default =		SOME "??"
	    },
	    { flag =		SOME "-filemap",
	      envVar =		SOME "FILEMAP",
	      configName =	SOME "FILEMAP",
	      default =		SOME "filemap"
	    }
	  ]

    fun doCmdLine opts args = let
	  in
	  end

    fun doStdOptions otherOpts args = let
	  val opts = otherOpts @ stdOptions
	  val {cmdOpts, otherArgs} = doCmdLine opts args
	  val config = MLDocConfig.configure {
		  options = cmdOpts,
		  defaults = ??,
		  file = ??
		}
	  in
	    {config = config, otherArgs = otherArgs}
	  end

  end;
