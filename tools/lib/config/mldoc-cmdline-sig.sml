(* mldoc-cmdline-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *)

signature MLDOC_CMDLINE =
  sig

    datatype tag = TAG of string | NA

    datatype default_value
      = NOVAL
      | NAME of Atom.atom
      | STR of string
      | NUM of int
      | BOOL of bool

    type option_info = {
	flag : tag,
	envVar : tag,
	configName : string list,
	default : string option
      }

    val stdOptions : option_info list

    val doCmdLine : option_info list -> string list -> {
	    cmdOpts : (string * string) list,
	    otherArgs : string list
	  }

    val doStdOptions : option_info list -> string list -> {
	    config : MLDocConfig.configuration,
	    otherArgs : string list
	  }

  end;
