(* main.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A tool for extracting an index from an ML-Doc file.
 *)

structure Main : sig

    val test : string -> unit

    val main : (string * string list) -> OS.Process.status

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup
    structure MM = MLDocModule
    structure MC = MLDocContent
    structure Flt = MLDocFloat
    structure I = MLDocIndex
    structure B = BuildIndex
    structure Spec = MLSpec
    structure Run = RunMLDocSGMLS
    structure P = OS.Path

    structure C = MLDocConfig

    structure SS = Substring
    structure F = Format

    fun error msg = (
	  TextIO.output(TextIO.stdErr, msg);
	  TextIO.flushOut TextIO.stdErr;
	  OS.Process.exit OS.Process.failure)

  (* parse the output of the SGMLS tool and return the markup tree *)
    fun parseFile fname = let
	  val inStrm = TextIO.openIn fname
	  val markup = MLDocParser.parse inStrm
		handle ex => (TextIO.closeIn inStrm; raise ex)
	  in
	    TextIO.closeIn inStrm;
	    markup
	  end

  (* translate CDATA.  We leave #"\\" escaped and SDATA marked for later
   * translation, but translate the other escape sequences.  We also trim
   * leading and trailing whitespace.
   *)
    local
      val sdataMark = SS.full "\\|"
    in
    val translate = M.stripData {
	    escape = (fn #"\\" => "\\\\" | c => String.str c),
	    sdata = fn ss => SS.concat[sdataMark, ss, sdataMark],
	    special = fn _ => NONE
	  }
    end

    fun translateOpt NONE = NONE
      | translateOpt (SOME data) = SOME(translate data)

    fun mkName s = Atom.atom(translate s)

  (* given a list of inline text, convert it into a string *)
    fun inlineToString ml = let
	  fun toStr (M.DATA s, l) = translate s :: l
	    | toStr (M.ELEM{body, ...}, l) = List.foldr toStr l body
	  in
	    String.concat(List.foldr toStr [] ml)
	  end

    fun walkSpec (file, index) = let
	  val n = mkName
	  fun sigRef sigId = B.NamedSig(B.findSig(index, n sigId))
	  fun walkSigBody env = let
		fun walk (Spec.BRspec _) = ()
		  | walk (Spec.INCLspec incls) = let
		      fun f (sigId, _) = (case Spec.getIdAndDocument sigId
			     of (_, SOME _) => ()
			      | (id, _) => B.includeSig(env, n id)
			    (* end case *))
		      in
			List.app f incls
		      end
		  | walk (Spec.STRspec(strId, sigId, _)) = (
		      case (Spec.getIdAndDocument sigId)
		       of (id, NONE) =>
			    B.newSubstr(env, {strId = n strId, bodySig = sigRef id})
			| _ =>
			    B.newSubstr(env,
			      {strId = n strId, bodySig = B.ExternSig})
		      (* end case *))
		  | walk (Spec.STRSIGspec(strId, sigBody)) = let
		      val subEnv = B.copyEnv (index, env)
		      in
			B.newSubstr(env, {
			    strId = n strId, bodySig = B.AnonSig subEnv
			  });
			List.app ((walkSigBody subEnv) o #1) sigBody
		      end
		  | walk (Spec.SHARINGspec _) = ()
		  | walk (Spec.EXNspec specs) = let
		      fun ins (exnId, _) = (B.insExn (env, n exnId))
		      in
			List.app ins specs
		      end
		  | walk (Spec.TYspec specs) = let
		      fun ins {eq, params, id, def} = 
			    B.insType (env, {id = n id, eq = eq})
		      in
			List.app ins specs
		      end
		  | walk (Spec.DTspec specs) = let
		      fun doSpec {params, id, cons, compact} = let
			    fun doCon (conId, _, _) = n conId
			    in
			      B.insDatatype (env, {
				  id = n id,
				  cons = List.map doCon cons
				})
			    end
		      in
			List.app doSpec specs
		      end
		  | walk (Spec.DTDEFspec specs) = let
		      fun ins {id, def} = B.insDatatypeDef (env, {id = n id})
		      in
			List.app ins specs
		      end
		  | walk (Spec.VALspec specs) = let
(* FIXME: We should record the RAISES information!!! *)
		      fun ins (valId, ty, _) = B.insVal (env, n valId)
		      in
			List.app ins specs
		      end
		in
		  walk
		end
	  in
	     walkSigBody
	  end

    fun scanSigBody (file, index) module = let
	  fun sigRef sigId = B.NamedSig(B.findSig(index, mkName sigId))
	  val walkSpec = walkSpec (file, index)
	  fun walkSigBody env markup = let
		val specs = Spec.getSigbodyContents markup
		in
		  List.app (fn (spec, _) => walkSpec env spec) specs
		end
	  fun getSigBody (MM.SIG_ID{sigId, doc=NONE, ...}, _) = sigRef sigId
	    | getSigBody (MM.SIG_ID{doc = SOME _, ...}, _) = B.ExternSig
	    | getSigBody (MM.SIG_SPEC{sigId=SOME id, specs}, bctx) = let
		val sigEntry = B.newSig (index, {sigId = mkName id, file = file})
		in
		  walkSigBody (I.Sig.env sigEntry) specs;
		  B.NamedSig sigEntry
		end
	    | getSigBody (MM.SIG_SPEC{sigId=NONE, specs}, bctx) = let
		val env = B.newEnv (index, file, bctx)
		in
		  walkSigBody env specs; B.AnonSig env
		end
	  fun getFctArg (MM.FCTARG_ID{sigId, sigDoc=NONE, ...}) = sigRef sigId
	    | getFctArg (MM.FCTARG_ID{sigDoc=SOME _, ...}) = B.ExternSig
	    | getFctArg (MM.FCTARG_SPEC{sigId=SOME id, specs}) = let
		val sigEntry = B.newSig (index, {sigId = mkName id, file = file})
		in
		  walkSigBody (I.Sig.env sigEntry) specs;
		  B.NamedSig sigEntry
		end
	    | getFctArg (MM.FCTARG_SPEC{sigId=NONE, specs}) = let
		val env = B.newEnv (index, file, I.ARGbound[])
		in
		  walkSigBody env specs; B.AnonSig env
		end
	  in
	    case module
	     of (MM.SIG{sigId, status, specs, structs}) => let
		  val sigEntry = B.newSig (index, {
			  sigId = mkName sigId, file = file
			})
		  val sigId = B.NamedSig sigEntry
		  fun insSigInst {strId, opaque, status, whereTypes, comment} =
			B.newStr (index, {
			    strId = mkName strId, file = file,
			    bodySig = sigId
			  })
		  in
		    walkSigBody (I.Sig.env sigEntry) specs;
		    List.app insSigInst structs
		  end
	      | (MM.TOPSTR{strId, status, opaque, sigBody}) =>
		  B.newStr (index, {
		      strId = mkName strId, file = file,
		      bodySig = getSigBody (sigBody, I.STRbound[mkName strId])
		    })
(** NOTE: some day, we may want to introduce a "partial" structure object into
 ** the index database to act as a parent for substructures.  This would allow
 ** better completeness checking.
 **)
	      | (MM.SUBSTR{context, strId, sigBody, ...}) => (
		  case sigBody
		   of MM.SIG_ID{sigId, doc = NONE, ...} => ignore(sigRef sigId)
		    | MM.SIG_ID{doc = SOME _, ...} => ()
		    | MM.SIG_SPEC{sigId=SOME sigId, specs} => let
			val sigEntry = B.newSig (index, {
				sigId = mkName sigId, file = file
			      })
			in
			  walkSigBody (I.Sig.env sigEntry) specs
			end
		    | MM.SIG_SPEC{sigId=NONE, ...} => Error.warning(
			"substructure \"%s\" without named signature is ignored",
			[ F.STR(String.concat(List.foldr
			    (fn (id, l) => id::"."::l) [strId] context))
			  ])
		  (* end case *))
	      | (MM.FCT{fctId, status, argSpecs, opaque, resSpecs}) =>
		  B.newFct (index, {
		      fctId = mkName fctId, file = file,
		      argSig = getFctArg argSpecs,
		      bodySig = getSigBody (resSpecs, I.FCTbound[mkName fctId])
		    })
	    (* end case *)
	  end

    fun findAnchors (index, fileEntry) contents = let
	  fun find iter = (case (M.next iter)
		 of (SOME(M.ELEM{elem=E.ADEF{tag}, ...}, iter')) => (
		      B.insAnchor (index, {tag=mkName tag, file=fileEntry});
		      find iter')
		  | (SOME(_, iter')) => find iter'
		  | NONE => ()
		(* end case *))
	  in
	    find (M.iterateList contents)
	  end

    fun walkContent (index, fileEntry, dir, body) = let
	  val scanSigBody = scanSigBody (fileEntry, index)
	  val findAnchors = findAnchors (index, fileEntry)
	  fun walkContent (MC.DOCSECT s, l) = walkSect(s, l)
	    | walkContent (MC.FLOAT(Flt.FIGURE{caption, label, ...}), l) = (
		B.insFigLabel (index, {file=fileEntry, name=Atom.atom label});
		findAnchors caption; l)
	    | walkContent (MC.FLOAT(Flt.TABLE{caption, label, rows, ...}), l) = (
		B.insTblLabel (index, {file=fileEntry, name=Atom.atom label});
		findAnchors caption;
		app (app (fn (Flt.TH{contents, ...}) => findAnchors contents
		           | (Flt.TD{contents, ...}) => findAnchors contents)) rows;
		l)
	    | walkContent (MC.FLOAT(Flt.CODE{caption, label, ...}), l) = (
		B.insCodeLabel (index, {file=fileEntry, name=Atom.atom label});
		findAnchors caption; l)
	    | walkContent (MC.PARA m, l) = (findAnchors [m]; l)
	  and walkSect (MC.SECTION{title, label, content, ...}, l) = let
		val label = Option.map Atom.atom label
		in
		  findAnchors title;
		  case label
		   of NONE => ()
		    | SOME lab => B.insSecLabel (index, {file=fileEntry, name=lab})
		  (* end case *);
		  I.File.SECTION{
		      title = inlineToString title,
		      label = label,
		      content = List.foldr walkContent [] content
		    } :: l
		end
	    | walkSect (MC.INCLFILE file, l) = let
		val path = P.joinDirFile{dir = dir, file = translate file}
		in
		  I.File.INCLFILE(Atom.atom path) :: l
		end
	    | walkSect (MC.INTERFACE{title, label, pre, module, post, ...}, l) =
		let
		val l = foldl walkContent l pre
		val l = foldl walkContent l post
		in
		  case label
		   of NONE => ()
		    | SOME lab => B.insIfaceLabel (index, {
			  file=fileEntry, name=Atom.atom lab
			})
		  (* end case *);
		  findAnchors title;
		  scanSigBody(MM.parseModule module);
(** should add this "section" to the list **)
		  l
		end
	  in
	    B.setFileContent (fileEntry, List.foldr walkSect [] body)
	  end

(***
 *** This should be its own application.
 ***
    fun summary index = let
	  val info = I.size index
	  in
	    TextIO.output (TextIO.stdOut,
	      F.format "\
		\Summary:\n\
		\  %d files; %d signatures; %d functors and structures\n\
		\  %d exceptions (%d unique)\n\
		\  %d types (%d unique)\n\
		\  %d values and constructors (%d unique)\n\
		\  %d anchors\n\
		\" [
		  F.INT(#nFiles info), F.INT(#nSigs info), F.INT(#nStructs info),
		  F.INT(#total(#nExns info)), F.INT(#distinct(#nExns info)),
		  F.INT(#total(#nTys info)), F.INT(#distinct(#nTys info)),
		  F.INT(#total(#nVals info)), F.INT(#distinct(#nVals info)),
		  F.INT(#nAnchors info)
		])
	  end
***)

    fun doInputFile config = let
	  val parse = Run.parseFile {config=config, includeEntities=[]}
	  val outDir = valOf(MLDocConfig.getStr(config, ["OutDir"]))
	  val basename = Run.basename config
	  fun doFile fileName = let
		val {path, base, dir} = basename fileName
		val infoFile = P.joinDirFile{
			dir = P.concat(outDir, dir),
			file = P.joinBaseExt{base = base, ext = SOME "info"}
		      }
		val index = B.index()
		val fileContents = parse fileName
		val {hdr, body, ...} = MLDocContent.getContents fileContents
(**!! need to figure out a policy for non-trivial source-file paths; for now,
 **!! we just strip the directory.
 **)
		val file = B.insFile (index, {
			name = mkName(P.concat(dir, base)),
			isEmpty = MLDocContent.emptyBody body,
			title = #title hdr
		      })
		in
		  walkContent (index, file, dir, body);
		  I.print (infoFile, index)
		end
	  in
	    doFile
	  end

    val options = StdOptions.stdOptions @ [
	    StdOptions.outDir "Info"
	  ]

    fun doit args = let
	  val {config, otherArgs} = C.configure {
		  appl = "Extract-Info",
		  opts = options,
		  args = args
		}
	  in
	    List.app (doInputFile config) otherArgs
	  end

    fun test file = doit [file]

    fun main (_, args) = (doit args; OS.Process.success)
	  handle exn => (Error.uncaughtExn exn; OS.Process.failure)

 end
