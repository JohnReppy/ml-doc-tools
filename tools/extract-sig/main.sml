(* main.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * A tool for extracting signatures from ML-Doc documents.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure C = MLDocConfig
    structure E = MLDocElem
    structure P = MLDocParser
    structure M = P.Markup
    structure MC = MLDocContent
    structure Sp = MLSpec
    structure SS = Substring
    structure F = Format

    val sdata = EntitiesToSML.transEntity

    val transData = M.transData {
	    escape = String.str,
	    sdata = sdata,
	    special = fn _ => NONE
	  }

    fun dataToStr data = SS.string(
	  SS.dropl Char.isSpace (
	    SS.dropr Char.isSpace (SS.full (transData data))))

    structure Style = struct
	type token = TextIOPP.token
	type style = TextIOPP.style
	type context = unit

	datatype xref_kind
	  = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

	val kwStyle = ()
	val punctStyle = ()
	val tyvarStyle = ()
	val idStyle = ()
	val idIdxStyle = ()
	val itStyle = ()

	fun extendContext ((), _) = ()

	fun str2tok s = dataToStr s

      (* identifiers that are possible cross references *)
	fun idToToken _ (Sp.ID s) = str2tok s
	  | idToToken _ (Sp.TAG(M.ELEM{elem, body=[M.DATA id], ...})) = str2tok id

      (* identifiers that are forward links to descriptions *)
	fun descRef (_, _, s) = str2tok s

      (* make a token with the given style (to support character escapes) *)
	fun mkToken (_, s) = str2tok s

      end

    structure PP = TextIOPP
    structure PPSpec = PPSpecFn (
	structure PPStrm = PP
	structure Style = Style
	fun specBreak _ _ = ()
(* the following breaks the PP's notion of indentation!
	fun specBreak false _ = ()
	  | specBreak true dev = SimpleTextIODev.newline dev)
*)
      )

  (* scan the markup tree for SIGBODYs *)
    fun getSigs m = let
	  fun find (M.ELEM{
		  elem=E.SIGBODY{sigid=SOME id, file=SOME f}, body, ...
		}) =
		  [{sigId=id, file=transData f, sigBody=body}]
	    | find (M.ELEM{
		  elem=E.SIGNATURE{sigid=id, ...}, body = (M.ELEM{
		    elem=E.SIGBODY{file=SOME f,...}, body, ...}
                    )::_, ...
		}) =
		  [{sigId=id, file=transData f, sigBody=body}]
	    | find (M.ELEM{body, ...}) = find' body
	    | find _ = []
	  and find' [] = []
	    | find' (elem::r) = find elem @ find' r
	  in
	    find m
	  end

    fun genSigFile (codeDir, base, vers, cpys) {sigId, file, sigBody} = let
	  val file = OS.Path.joinDirFile{dir=codeDir, file=file}
	  val outS = TextIO.openOut file
	  fun pr s = TextIO.output(outS, s)
	  fun prf (fmt, items) = pr(F.format fmt items)
	  fun prCopyright {owner, year} = prf(
		" * COPYRIGHT (c) %d %s.\n",
		[F.INT year, F.STR(dataToStr owner)]);
	  val version = (case vers
		 of SOME{verid=SOME v, year, month=SOME m, day=SOME d} =>
		      F.format " (v. %s; %d-%02d-%02d)"
			[F.STR v, F.INT year, F.INT m, F.INT d]
		  | SOME{verid=SOME v, year, month=SOME m, day=NONE} =>
		      F.format " (v. %s; %d-%02d)" [F.STR v, F.INT year, F.INT m]
		  | SOME{verid=SOME v, year, ...} =>
		      F.format " (v. %s; %d)" [F.STR v, F.INT year]
		  | SOME{verid=NONE, year, month=SOME m, day=SOME d} =>
		      F.format " (%d-%02d-%02d)" [F.INT year, F.INT m, F.INT d]
		  | SOME{verid=NONE, year, month=SOME m, day=NONE} =>
		      F.format " (%d-%02d)" [F.INT year, F.INT m]
		  | SOME{verid=NONE, year, ...} =>
		      F.format " (%d)" [F.INT year]
		  | NONE => ""
		(* end case *))
	  val ppStrm = PP.openOut {dst = outS, wid = 80}
	  fun str s = PP.string ppStrm s
	  fun sp () = PP.space ppStrm 1
	  val ppSpecs = PPSpec.ppSpecs ((), ppStrm, true)
	  in
	    prf ("\
		\(* %s\n\
		\ *\n\
	      \", [F.STR(OS.Path.file file)]);
	    List.app prCopyright cpys;
	    prf ("\
		\ *\n\
		\ * extracted from %s.mldoc%s\n\
		\ *)\n\n\
	      \", [F.STR base, F.STR version]);
	    PP.openVBox ppStrm (PP.Abs 2);
	      PP.openHBox ppStrm;
	        str "signature"; sp(); str sigId; sp(); str "=";
	      PP.closeBox ppStrm;
	      PP.newline ppStrm;
	      PP.openVBox ppStrm (PP.Abs 2);
	        str "sig";
	        PP.newline ppStrm;
		ppSpecs (Sp.getSigbodyContents sigBody);
	      PP.closeBox ppStrm;
	      PP.cut ppStrm;
	      str "end";
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm;
	    pr "\n";
	    TextIO.closeOut outS
	  end

    fun doBody (codeDir, base, hdr : MC.header, body) = let
	  fun doSigs module = (case getSigs module
		 of [] => ()
		  | sigs =>
		      List.app
			(genSigFile (codeDir, base, #version hdr, #copyrights hdr))
			  sigs
		(* end case *))
	  fun doSect (MC.SECTION{content, ...}) = List.app doElem content
	    | doSect (MC.INCLFILE _) = ()
	    | doSect (MC.INTERFACE{module, ...}) = doSigs module
	  and doElem (MC.DOCSECT s) = doSect s
	    | doElem _  = ()
	  in
	    List.app doSect body
	  end

    fun doInputFile config = let
	  val outDir = Option.valOf(MLDocConfig.getStr(config, ["OutDir"]))
	  val parse = RunMLDocSGMLS.parseFile {config=config, includeEntities=[]}
	  val basename = RunMLDocSGMLS.basename config
	  fun doFile inFile = let
		val {dir, base, ...} = basename inFile
		val outDir = OS.Path.concat(outDir, dir)
		val doc = parse inFile
		val {hdr, body} = MC.getContents doc
		in
		  doBody (outDir, base, hdr, body)
		end
		  handle (Fail msg) =>
		    Error.error (F.format "Error in \"%s\": %s\n" [
			F.STR inFile, F.STR msg
		      ])
	  in
	    doFile
	  end

    val options = StdOptions.stdOptions @ [
	    StdOptions.outDir "Sigs"
	  ]

    fun doOptions args = let
	  val {config, otherArgs} = C.configure {
		  appl = "Extract-Sig",
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
