(* run-mldoc-sgmls.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Support for running SGMLS (using the ML-Doc DTD) as a sub-process of SML.
 *)

structure RunMLDocSGMLS : sig

    val basename : MLDocConfig.configuration
	-> string -> {path : string, base : string, dir : string}
	(* given a filename with an optional ".mldoc" extension, return
	 * the full path name of the ML-Doc file, the base file name, and
	 * the relative directory.  This raises Fail if the ML-Doc file does
	 * not exist, or if the filename has an extension other than ".mldoc".
	 *)

    val parseFile : {
	    config : MLDocConfig.configuration,
	    includeEntities : string list
	  } -> string -> MLDocMarkup.markup list

  end = struct

    structure F = Format
    structure P = OS.Path
    structure Cfg = MLDocConfig

    fun joinPath (base, arcs) =
	  foldl (fn (p, f) => P.joinDirFile{dir=p, file=f}) base arcs

    fun runSGMLS {config, includeEntities} files = let
	  val nsgmlsPath =
		Option.getOpt(Cfg.getStr(config, ["Tools", "SGMLS"]), "nsgmls")
	  val options = "-oline" :: (map (fn e => "-i"^e) includeEntities)
	  val options = (case Cfg.getStr(config, ["Catalog"])
		 of (SOME p) => ("-c" ^ p) :: options
		  | NONE => options
		(* end case *))
	  in
	    RunSGMLS.run {path = SOME nsgmlsPath, args = options @ files}
	  end

    structure P = OS.Path
    fun basename config filePath = let
	  val mldocDir = Option.valOf(Cfg.getStr(config, ["MLDocDir"]))
	  val {dir=fullDir, file=fileName} = OS.Path.splitDirFile filePath
	  val (path, base) = (case (P.splitBaseExt fileName)
		 of {base, ext=SOME "mldoc"} => (filePath, base)
		  | {base, ext=NONE} =>
		      (P.joinBaseExt{base=filePath, ext=SOME "mldoc"}, fileName)
		  | {base, ext=SOME _} => raise Fail "strange extension"
		(* end case *))
	  val dir = (case P.fromString fullDir
		 of {isAbs=true, vol, arcs} =>
		      raise Fail "absolute pathname for ML-Doc file"
		  | {arcs=first::rest, vol, ...} =>
		      if (first <> mldocDir)
			then raise Fail "ML-Doc file not in ML-Doc source directory"
			else P.toString{isAbs=false, vol=vol, arcs=rest}
		  | _ => raise Fail "bogus ML-Doc file name"
		(* end case *))
	  in
	    if OS.FileSys.access(path, [])
	      then {path=path, base=base, dir=dir}
	      else raise Fail "ML-Doc file not found"
	  end

  (* parse the output of the SGMLS tool and return the markup tree *)
    fun parseFile args fname = let
	  val (inStrm, close) = runSGMLS args [fname]
	  in
	    ((MLDocParser.parse inStrm) before close())
	      handle ex => (close(); raise ex)
	  end

  end
