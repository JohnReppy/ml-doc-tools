(* mldoc-config.sml
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
 *)

structure MLDocConfig :> MLDOC_CONFIG =
  struct

    structure DB = ConfigDB

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

    datatype configuration = CFG of {
	applDB : DB.config_db option,
	db : DB.config_db
      }

    fun cvtValue (NAME a) = DB.ATOM a
      | cvtValue (STR s) = DB.STR s
      | cvtValue (NUM n) = DB.NUM n
      | cvtValue (BOOL b) = DB.BOOL b

    val key_ConfigFile = Atom.atom "ConfigFile"
    val dfltConfigFile = "Config.cfg"

  (* load the configuration file *)
    fun loadConfigDB (appl, argDB, dfltDB) = let
	  fun find (db, key) = (case DB.find(db, key)
		 of NONE => DB.find(db, appl::key)
		  | res => res
		(* end case *))
	  fun fileExists (SOME(DB.STR f)) =
		if OS.FileSys.access(f, [OS.FileSys.A_READ]) then SOME f else NONE
	    | fileExists _ = NONE
	  val parse = Option.compose(DB.parse, fileExists)
	  in
	    case parse(find (argDB, [key_ConfigFile]))
	     of (SOME db) => db
	      | NONE => (case parse(find (dfltDB, [key_ConfigFile]))
		   of (SOME db) => db
		    | NONE => (case parse(SOME(DB.STR dfltConfigFile))
			 of (SOME db) => db
			  | NONE => DB.mkConfigDB()
			(* end case *))
		  (* end case *))
	    (* end case *)
	  end

  (* create a configuration: appl is the name of the application, opts is
   * the list of option information, and args is the command-line argument
   * list.  The configuration is setup as follows:
   *    1) create the default DB from the opts list.
   *    2) create the argument DB from the argument list.
   *    3) create the configuration DB from the configuration file.  This file
   *	   is specified by the argument DB, orelse by the default DB, orelse
   *	   by the installation defaults.
   *    4) the final DB is created by layering the argument DB on top of the
   * 	   configuration DB, which is layered on top of the default DB.
   *)
    fun configure {appl, opts, args} = let
	  val appl' = Atom.atom appl
	  val remainingArgs = ref args
	(* create the argument DB *)
	  val dfltDB = DB.mkConfigDB()
	  fun insert (db, s, v, override) = let
		fun ins path = DB.add'(db, path, cvtValue v, override)
		in
		  case (String.fields (fn #"." => true | _ => false) s)
		   of [] => () (* should generate error message *)
		    | (""::r) => ins(appl::r)
		    | r => ins r
		  (* end case *)
		end
	  fun doOption ({default=NOVAL, ...}) = ()
	    | doOption ({flag, configName, default}) =
		insert (dfltDB, configName, default, false)
	  val _ = List.app doOption opts
	(* create the argument DB *)
	  val argDB = DB.mkConfigDB()
	  fun doArg {flag, configName, default} = let
		fun ins (v, rest) = (
		      remainingArgs := rest;
		      insert(argDB, configName, v, true))
		fun findFlg (s, dflt, arg::r, pre) = if (arg=s)
		      then ins (BOOL(not dflt), List.revAppend(pre, r))
		      else findFlg (s, dflt, r, arg::pre)
		  | findFlg (_, _, [], _) = ()
		fun findArg (s, arg::v::r, pre) = if (arg=s)
		      then ins(STR v, List.revAppend(pre, r))
		      else findArg (s, v::r, arg::pre)
		  | findArg _ = ()
		in
		  case (flag, default)
		   of (FLG s, BOOL b) => findFlg(s, b, !remainingArgs, [])
		    | (FLG s, NOVAL) => findFlg(s, false, !remainingArgs, [])
		    | (ARG s, _) => findArg(s, !remainingArgs, [])
		    | _ => ()
		  (* end case *)
		end
	  val _ = List.app doArg opts
	(* load the configuration DB *)
	  val configDB = loadConfigDB (appl', argDB, dfltDB)
	(* merge the DBs *)
	  val db = DB.mergeConfigDBs(DB.mergeConfigDBs(dfltDB, configDB), argDB)
	  in
	    { config = CFG{
		  applDB = (case DB.find(db, [appl'])
		     of (SOME(DB.TBL db')) => SOME db'
		      | _ => NONE
		    (* end case *)),
		  db = db
		},
	      otherArgs = !remainingArgs
	    }
	  end

    type path = string list

    fun find (CFG{applDB = SOME applDB, db}, path) = (case DB.find(applDB, path)
	   of NONE => DB.find(db, path)
	    | res => res
	  (* end case *))
      | find (CFG{db, ...}, path) = DB.find(db, path)

    fun getStr' (cfg, path) = (case find(cfg, path)
	   of (SOME(DB.STR s)) => SOME s
	    | (SOME(DB.ATOM a)) => SOME(Atom.toString a)
	    | _ => NONE
	  (* end case *))
    fun getName' (cfg, path) = (case find(cfg, path)
	   of (SOME(DB.ATOM a)) => SOME a
	    | (SOME(DB.STR s)) => SOME(Atom.atom s)
	    | _ => NONE
	  (* end case *))
    fun getInt' (cfg, path) = (case find(cfg, path)
	   of (SOME(DB.NUM n)) => SOME n
	    | (SOME(DB.STR s)) => Int.fromString s
	    | _ => NONE
	  (* end case *))
    fun getBool' (cfg, path) = (case find(cfg, path)
	   of (SOME(DB.BOOL b)) => SOME b
	    | (SOME(DB.STR s)) => Bool.fromString s
	    | _ => NONE
	  (* end case *))
    fun getFlag' (cfg, path) = Option.getOpt(getBool'(cfg, path), false)
    fun getDB' (cfg, path) = (case find(cfg, path)
	   of (SOME(DB.TBL db)) => SOME(CFG{applDB=NONE, db=db})
	    | _ => NONE
	  (* end case *))

    local
      fun wrap f (cfg, path) = f(cfg, List.map Atom.atom path)
    in
    val getStr = wrap getStr'
    val getName = wrap getName'
    val getInt = wrap getInt'
    val getBool = wrap getBool'
    val getFlag = wrap getFlag'
    val getDB = wrap getDB'
    end

  end;

