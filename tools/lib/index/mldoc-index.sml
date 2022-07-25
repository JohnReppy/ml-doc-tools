(* mldoc-index.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 * This is the external interface of an index.
 *)

structure MLDocIndex : MLDOC_INDEX =
  struct
    structure IR = IndexRep

    type index = IR.index
    type env = IR.env
    type name = IR.name
    datatype binding_context = datatype IR.binding_context

    type qual_id = (binding_context * name)

  (* extend a binding context with a substructure ID.  This raises Fail
   * on TOPbound contexts.
   *)
    val extendContext = IR.extendContext

    val compareContext = IR.compareContext

    fun bindingToString bctx = let
	  fun f (kind, path) =
		kind ^ (String.concatWith "." (List.map Atom.toString path))
	  in
	    case bctx
	     of TOPbound => "TOP"
	      | SIGbound path => f ("SIG:", path)
	      | STRbound path => f ("STR:", path)
	      | FCTbound path => f ("FCT:", path)
	      | ARGbound path => f ("ARG:", path)
	    (* end case *)
	  end

    val findContext = IndexSearch.findContext
    val contextToEnv = IndexSearch.contextToEnv

    fun canonicalContext (_, TOPbound) = TOPbound
      | canonicalContext (index, ctx) = let
	  val IR.ENV{context, ...} = contextToEnv (index, ctx)
	  in
	    case context
	     of SIGbound[id] => let
		  val SOME(IR.SIGentry{instances, ...}) = IndexSearch.findSig (index, id)
		  in
		  (* a "generic" signature is one that is named and has either
		   * zero or more than one instance.  For non-generic signatures,
		   * the canonical representation is the structure/functor that
		   * is its single instance.
		   *)
		    case !instances
		     of [inst] => inst
		      | _ => context
		    (* end case *)
		  end
	      | _ => context
	    (* end case *)
	  end

    val findFile = IndexSearch.findFile
    val findAnchor = IndexSearch.findAnchor
    val findLabel = IndexSearch.findLabel

    val findSig = IndexSearch.findSig
    val findStr = IndexSearch.findStr
    val findFct = IndexSearch.findFct

  (* find qualified objects *)
    val findSubstr = IndexSearch.findSubstr
    val findType   = IndexSearch.findType
    val findExn    = IndexSearch.findExn
    val findCon    = IndexSearch.findCon
    val findVal    = IndexSearch.findVal

  (* Other index operations *)

  (* Abstract interfaces to the various objects in the index *)

    structure File =
      struct
	type entry = IR.file_entry
	datatype file_content = datatype IR.file_content
	fun name (IR.FILEentry{name, ...}) = name
	fun dir (IR.FILEentry{name, ...}) = OS.Path.dir(Atom.toString name)
	fun base (IR.FILEentry{name, ...}) = OS.Path.base(Atom.toString name)
	fun isEmpty (IR.FILEentry{isEmpty, ...}) = isEmpty
	fun title (IR.FILEentry{title, ...}) = title
	fun content (IR.FILEentry{content, ...}) = !content
      end

    structure Anchor =
      struct
	type entry = IR.anchor_entry
	fun tag (IR.ANCHORentry{tag, ...}) = tag
	fun file (IR.ANCHORentry{file, ...}) = file
      end

    structure Label =
      struct
	type entry = IR.label_entry
	datatype kind = datatype IR.label_kind
	fun name (IR.LABELentry{name, ...}) = name
	fun kind (IR.LABELentry{kind, ...}) = kind
	fun file (IR.LABELentry{file, ...}) = file
      end

    structure Sig =
      struct
	type entry = IR.sig_entry
	fun name (IR.SIGentry{id, ...}) = id
	fun env (IR.SIGentry{body=ref(SOME e), ...}) = e
	  | env ent = Error.error'["MLDocIndex.env(", Atom.toString(name ent), ")"]
	fun file (IR.SIGentry{body=ref(SOME(IR.ENV{file, ...})), ...}) = file
	  | file ent = Error.error'["MLDocIndex.file(", Atom.toString(name ent), ")"]
	fun instances (IR.SIGentry{instances, ...}) = !instances
      end

    structure Str =
      struct
	type entry = IR.str_entry
	fun name (IR.STRentry{id, ...}) = id
	fun file (IR.STRentry{file, ...}) = file
	fun context (IR.STRentry{binding=SOME(IR.ENV{context=ctx, ...}), ...}) = ctx
	  | context _ = IR.TOPbound
      end

    structure Fct =
      struct
	type entry = IR.fct_entry
	fun name (IR.FCTentry{id, ...}) = id
	fun file (IR.FCTentry{file, ...}) = file
      end

    structure Ty =
      struct
	datatype ty_kind = datatype IR.ty_kind
	type entry = IR.ty_entry
	fun name (IR.TYentry{id, ...}) = id
	fun kind (IR.TYentry{kind, ...}) = kind
	fun file (IR.TYentry{binding=IR.ENV{file, ...}, ...}) = file
	fun context (IR.TYentry{binding=IR.ENV{context=ctx, ...}, ...}) = ctx
      end

    structure Exn =
      struct
	type entry = IR.exn_entry
	fun name (IR.EXNentry{id, ...}) = id
	fun file (IR.EXNentry{binding=IR.ENV{file, ...}, ...}) = file
	fun context (IR.EXNentry{binding=IR.ENV{context=ctx, ...}, ...}) = ctx
      end

    structure Val =
      struct
	type entry = IR.val_entry
	fun name (IR.VALentry{id, ...}) = id
	fun file (IR.VALentry{binding=IR.ENV{file, ...}, ...}) = file
	fun context (IR.VALentry{binding=IR.ENV{context=ctx, ...}, ...}) = ctx
      end

    structure Env =
      struct
	fun file (IR.ENV{file, ...}) = file
	fun listStrs (IR.ENV{strTbl, ...}) = AtomTable.listItems strTbl
	fun listTys (IR.ENV{tyTbl, ...}) = AtomTable.listItems tyTbl
	fun listExns (IR.ENV{exnTbl, ...}) = AtomTable.listItems exnTbl
	fun listVals (IR.ENV{valTbl, ...}) = AtomTable.listItems valTbl
	fun listCons (IR.ENV{conTbl, ...}) = AtomTable.listItemsi conTbl
      end


  (* Other index operations *)
    val parse = IndexParser.parseIndexFile

    fun print (fname, index) = let
	  val outS = TextIO.getOutstream(TextIO.openOut fname)
	  fun close () = TextIO.StreamIO.closeOut outS
	  fun say msg = TextIO.StreamIO.output (outS, msg)
	  fun flush () = TextIO.StreamIO.flushOut outS
	  in
	    (PrintIndex.printIndex {say=say, flush=flush} index; close())
	      handle ex => (close(); raise ex)
	  end

    fun listFiles (IR.IDX{fileTbl, ...}) = AtomTable.listItems fileTbl
    fun listAnchors (IR.IDX{anchorTbl, ...}) = AtomTable.listItems anchorTbl
    fun listLabels (IR.IDX{labelTbl, ...}) = AtomTable.listItems labelTbl
    fun listSigs (IR.IDX{sigTbl, ...}) = AtomTable.listItems sigTbl
    fun listTopStrs (IR.IDX{strTbl, ...}) = AtomTable.listItems strTbl
    fun listFcts (IR.IDX{fctTbl, ...}) = AtomTable.listItems fctTbl

  (* given a function for listing the elements of an environment and an index,
   * list all of the entries in the index.
   *)
    fun listAll listFn (IR.IDX{topEnv, sigTbl, strTbl, fctTbl, ...}) = let
	  fun strEnv (_, IR.STRentry{bodySig=IR.AnonSig env, ...}) = SOME env
	    | strEnv _ = NONE
	  fun envsOfStrTbl tbl = List.mapPartial strEnv (AtomTable.listItemsi tbl)
	  fun fctEnv (_, IR.FCTentry{argSig, bodySig, ...}) = (
		case (argSig, bodySig)
		 of (IR.AnonSig env1, IR.AnonSig env2) => [env1, env2]
		  | (_, IR.AnonSig env2) => [env2]
		  | (IR.AnonSig env1, _) => [env1]
		  | _ => []
		(* end case *))
	(* first we recursively walk the index to get a list of all of
	 * the environments.  We start with the top-level environments
	 * and signatures.
	 *)
	  val envList = !topEnv @ 
		(List.mapPartial (fn (_, IR.SIGentry{body, ...}) => !body)
		  (AtomTable.listItemsi sigTbl))
	(* add the anonymous top-level structure and functor environments *)
	  val envList = List.concat [
		  List.concat (List.map fctEnv (AtomTable.listItemsi fctTbl)),
		  envsOfStrTbl strTbl, envList
		]
	(* now we recursively walk the sub-environments of the top-level
	 * environment.
	 *)
	  fun walkEnv (env as IR.ENV{strTbl, ...}, l) =
		List.foldl walkEnv (env::l) (envsOfStrTbl strTbl)
	  val envList = List.foldl walkEnv [] envList
	  in
	  (* now fold the list function across each environment *)
	    List.foldl (fn (env, l) => listFn env @ l) [] envList
	  end

  end;
