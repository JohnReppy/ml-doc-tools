(* mldoc-config-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * This library provides generic support for specifying configuration
 * options as name/value pairs in a configuration file augmented with
 * information from the command line, environment and a list of defaults.
 * The search order for determining the correct value is:
 *    1) command-line options
 *    2) environment (not yet supported)
 *    3) configuration file
 *    4) program defaults
 *
 *)

signature MLDOC_CONFIG =
  sig

    datatype flag
      = NA		(* no command-line flag *)
      | FLG of string	(* command-line flag w/o any argument *)
      | ARG of string	(* command-line flag with an argument *)

    datatype default_value
      = NOVAL
      | NAME of Atom.atom
      | STR of string
      | NUM of int
      | BOOL of bool

    type option_info = {
	flag : flag,
	configName : string,
	default : default_value
      }

    type configuration

  (* create a configuration: appl is the name of the application, opts is
   * the list of option information, and args is the command-line argument
   * list.
   *)
    val configure : {
	    appl : string,
	    opts : option_info list,
	    args : string list
	  } -> {
	    config : configuration,
	    otherArgs : string list
	  }

    val getStr   : (configuration * string list) -> string option
    val getStr'  : (configuration * Atom.atom list) -> string option
    val getName  : (configuration * string list) -> Atom.atom option
    val getName' : (configuration * Atom.atom list) -> Atom.atom option
    val getInt   : (configuration * string list) -> int option
    val getInt'  : (configuration * Atom.atom list) -> int option
    val getBool  : (configuration * string list) -> bool option
    val getBool' : (configuration * Atom.atom list) -> bool option
    val getFlag  : (configuration * string list) -> bool
    val getFlag' : (configuration * Atom.atom list) -> bool
    val getDB    : (configuration * string list) -> configuration option
    val getDB'   : (configuration * Atom.atom list) -> configuration option

  end;
