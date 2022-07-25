(* html-context.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Provides context information for converting an ML-Doc document into an
 * HTML document.
 *)

structure HTMLContext : sig

    structure I : MLDOC_INDEX

    type context

    type source = {fileName : string, doc : MLDocMarkup.markup list}

    val mkSrcContext : {
	    config : MLDocConfig.configuration,
	    src : source
	  } -> context
    val mkContext : {
	    config : MLDocConfig.configuration,
	    title : string
	  } -> context

  (* a used to mark that we are inside a <CODE> element *)
    val codeCtx : context -> context
    val inCode  : context -> bool

    val config		: context -> MLDocConfig.configuration
    val index		: context -> I.index
    val baseURL		: context -> string option
    val srcFile		: context -> string
    val version		: context -> {
	    verid : string option, year : int, month : int option, day : int option
	  } option
    val title		: context -> string option
    val bindingContext	: context -> I.binding_context
    val thisFile	: context -> I.File.entry
    val parentFile	: context -> I.File.entry option
    val rootFile	: context -> I.File.entry option
	(* finds the root that is above this file; returns NONE, if this file
	 * is a root.
	 *)

  (* resolve %XREF elements *)
    val resolveXRef : (context * MLDocElem.element * string) -> {
	    doc  : Document.doc_info option,
	    nolink : bool,
	    noindex : bool,
	    kind : ResolveRef.xref_kind,
	    path : MLDocIndex.binding_context,
	    name : string
	  } option

  (* copy a context substituting a new source file. *)
    val fileCtx : (context * source) -> context

  (* section nesting levels *)
    val sectionRelLevel : context -> int	(* relative to file *)
    val sectionAbsLevel : context -> int	(* relative to document root *)
    val sectionCtx      : context -> context

    val docIndex : (context * string) -> I.index option
	(* returns the index of an external document (if it can be found).
	 * Once the index has been loaded, it is cached.
	 *)
    val docBaseURL : (context * string) -> string option
    val docRootURL : (context * string) -> string option

    val findFile   : (context * string) -> FileTree.file_nd option
    val findAnchor : (context * string) -> I.Anchor.entry option
    val findLabel  : (context * string) -> I.Label.entry option
    val findSig    : (context * string) -> I.Sig.entry option
    val findTopStr : (context * string) -> I.Str.entry option
    val findFct    : (context * string) -> I.Fct.entry option

  (* these functions use the current binding_context to find a locally
   * bound thing.
   *)
    val findStr    : (context * string) -> I.Str.entry option
    val findTy     : (context * string) -> I.Ty.entry option
    val findCon    : (context * string) -> I.Ty.entry option
    val findExn    : (context * string) -> I.Exn.entry option
    val findVal    : (context * string) -> I.Val.entry option

    val withContext : (context * I.binding_context) -> context
    val withSig     : (context * string) -> context
    val withTopStr  : (context * string) -> context
    val withFct     : (context * string) -> context
    val withFctArg  : (context * string) -> context
    val withSubstr  : (context * string) -> context
    val withStrPath : (context * string list) -> context

    val joinPath : Atom.atom list -> string
	(* a utility function for translating a qualified ID to a string *)

  end = struct

    structure I = MLDocIndex
    structure P = OS.Path

    fun joinPath path = let
	  fun f [id] = [Atom.toString id, "."]
	    | f (id::r) = (Atom.toString id) :: "." :: f r
	  in
	    concat(f path)
	  end

    type source = {fileName : string, doc : MLDocMarkup.markup list}

    datatype context = C of {
	info : static_info,
	ctx : I.binding_context,
	inCode : bool,
	sectLvl : int
      }
    and static_info = Info of {
	config : MLDocConfig.configuration,
	srcFile : I.File.entry option,
	fileLvl : int,
	hdr : {
	    title : string option,
	    author : {
		name : string, email : string,
		year : int, month : int option, day : int option
	      } option,
	    version : {
		verid : string option,
		year : int, month : int option, day : int option
	      } option,
	    copyrights : {owner : string, year : int} list
	  },
	index : I.index,
	baseURL : string option,	(* this holds the BaseURL configuration *)
					(* variable, but is NONE when the *)
					(* RelativeLinks is true. *)
	fileMap : I.File.entry -> FileTree.file_nd,
	parentMap : I.File.entry -> FileTree.file_nd option,
	docCache : Document.doc_cache
      }

    fun getSrcInfo (index, nodeMap, {fileName, doc}) = let
	  val hdr = #hdr(MLDocContent.getHeader doc)
	  in
	    case I.findFile(index, Atom.atom fileName)
	     of NONE => Error.error'["unable to find file \"", fileName, "\""]
	      | (SOME entry) => let
		  val FileTree.FILE{level, ...} = nodeMap entry
		  in
		    ( { title = SOME(#title hdr), author = #author hdr,
			version = #version hdr, copyrights = #copyrights hdr
		      },
		      SOME entry, level
		    )
		  end
	    (* end case *)
	  end

    fun mkContext {config, title} = let
	  val index = (case MLDocConfig.getStr(config, ["MasterInfoFile"])
		 of NONE => Error.error "no MasterInfoFile"
		  | (SOME s) => I.parse s
		(* end case *))
	  val {nodeMap, parentMap, ...} = FileTree.mkForest index
	  in
	    C{
		info = Info{
		    config = config,
		    srcFile = NONE,
		    fileLvl = 0,
		    hdr = {
			title=SOME title, author=NONE, version=NONE, copyrights=[]
		      },
		    index = index,
		    baseURL = NONE,
		    fileMap = nodeMap,
		    parentMap = parentMap,
		    docCache = Document.mkCache config
		  },
		ctx = I.TOPbound,
		inCode = false,
		sectLvl = 0
	      }
	  end

    fun mkSrcContext {config, src} = let
	  val index = I.parse(valOf(MLDocConfig.getStr(config, ["MasterInfoFile"])))
	  val {nodeMap, parentMap, ...} = FileTree.mkForest index
	  val (hdr, entry, sectLvl) = getSrcInfo (index, nodeMap, src)
	  in
	    C{
		info = Info{
		    config = config,
		    srcFile = entry,
		    fileLvl = sectLvl,
		    hdr = hdr,
		    index = index,
		    baseURL =
		      if MLDocConfig.getFlag(config, ["HTML", "RelativeLinks"])
			then NONE
			else MLDocConfig.getStr(config, ["HTML", "BaseURL"]),
		    fileMap = nodeMap,
		    parentMap = parentMap,
		    docCache = Document.mkCache config
		  },
		ctx = I.TOPbound,
		inCode = false,
		sectLvl = sectLvl
	      }
	  end

  (* a used to mark that we are inside a <CODE> element *)
    fun codeCtx (C{info, ctx, sectLvl, ...}) =
	  C{info=info, ctx=ctx, inCode=true, sectLvl=sectLvl}
    fun inCode (C{inCode=b, ...}) = b

    fun config (C{info=Info{config, ...}, ...}) = config
    fun index (C{info=Info{index, ...}, ...}) = index
    fun baseURL (C{info=Info{baseURL, ...}, ...}) = baseURL
    fun srcFile (C{info=Info{srcFile=NONE, ...}, ...}) = ""
      | srcFile (C{info=Info{srcFile=SOME f, ...}, ...}) =
	  Atom.toString(I.File.name f)
    fun version (C{info=Info{hdr, ...}, ...}) = #version hdr
    fun title (C{info=Info{hdr, ...}, ...}) = #title hdr
    fun bindingContext (C{ctx, ...}) = ctx
    fun thisFile (C{info=Info{srcFile, ...}, ...}) = valOf srcFile
    fun parentFile (C{info=Info{srcFile, parentMap, ...}, ...}) = (
	  case (Option.mapPartial parentMap srcFile)
	   of NONE => NONE
	    | (SOME(FileTree.FILE{file, ...})) => SOME file
	  (* end case *))
    fun rootFile (C{info=Info{srcFile=NONE, ...}, ...}) = NONE
      | rootFile (ctx as C{info=Info{parentMap, ...}, ...}) = (case parentFile ctx
	   of NONE => NONE
(* we could return srcFile instead of NONE? *)
	    | (SOME entry) => let
		fun findRoot entry = (case parentMap entry
		       of NONE => SOME entry
			| (SOME(FileTree.FILE{file, ...})) => findRoot file
		      (* end case *))
		in
		  findRoot entry
		end
	  (* end case *))

  (* resolve %XREF elements *)
    fun resolveXRef (C{info=Info{index, docCache, ...}, ctx, ...}, xref, id) =
	  ResolveRef.resolve (index, docCache) (ctx, xref, id)

  (* copy a context substituting a new source file. *)
    fun fileCtx (C{info, ...}, src) = let
	  val Info{config, index, fileMap, parentMap, docCache, ...} = info
	  val (hdr, entry, sectLvl) = getSrcInfo (index, fileMap, src)
	  in
	    C{
		info = Info{
		    config = config,
		    srcFile = entry,
		    fileLvl = sectLvl,
		    hdr = hdr,
		    index = index,
		    baseURL = NONE,
		    fileMap = fileMap,
		    parentMap = parentMap,
		    docCache = docCache
		  },
		ctx = I.TOPbound,
		inCode = false,
		sectLvl = sectLvl
	      }
	  end

  (* section nesting levels *)
    fun sectionRelLevel (C{info=Info{fileLvl, ...}, sectLvl, ...}) =
	  sectLvl - fileLvl
    fun sectionAbsLevel (C{sectLvl, ...}) = sectLvl
    fun sectionCtx (C{info, ctx, inCode, sectLvl}) =
	  C{info=info, ctx=ctx, inCode=inCode, sectLvl=sectLvl+1}

    fun getDoc (C{info=Info{docCache, ...}, ...}, docName) =
	  Document.getDoc (docCache, docName)

  (* returns the index of an external document (if it can be found).
   * Once the index has been loaded, it is cached.
   *)
    fun docIndex (C{info=Info{docCache, ...}, ...}, docName) =
	  Document.docIndex (docCache, docName)

    fun docBaseURL (C{info=Info{docCache, ...}, ...}, docName) =
	  Document.docBaseURL (docCache, docName)

    fun docRootURL (C{info=Info{docCache, ...}, ...}, docName) =
	  Document.docRootURL (docCache, docName)

    fun findFile (C{info=Info{index, fileMap, ...}, ...}, id) =
	  Option.map fileMap (I.findFile(index, Atom.atom id))

    local
      fun findTop findFn (C{info=Info{index, ...}, ...}, id) =
	    findFn (index, Atom.atom id)
      fun find findFn (C{info=Info{index, ...}, ctx, ...}, id) =
	    findFn (index, (ctx, Atom.atom id))
    in
    val findAnchor	= findTop I.findAnchor
    val findLabel	= findTop I.findLabel
    val findSig		= findTop I.findSig
    val findTopStr	= findTop I.findStr
    val findFct		= findTop I.findFct
    val findStr		= find I.findSubstr
    val findTy		= find I.findType
    val findCon		= find I.findCon
    val findExn		= find I.findExn
    val findVal		= find I.findVal
    end (* local *)

    fun withContext (C{info as Info{index, ...}, inCode, sectLvl, ...}, ctx) = C{
	    info = info,
	    ctx = I.canonicalContext(index, ctx),
	    inCode = inCode,
	    sectLvl = sectLvl
	  }

    fun withSig (c as C{ctx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val sigId = Atom.atom id
	  in
	    case I.findSig (index, sigId)
	     of (SOME _) => withContext(c, I.SIGbound[sigId])
	      | _ => raise Fail "withSig: undefined signature"
	    (* end case *)
	  end
      | withSig _ = raise Fail "withSig: not at top-level"

    fun withTopStr (c as C{ctx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val strId = Atom.atom id
	  in
	    case I.findStr (index, strId)
	     of (SOME _) => withContext(c, I.STRbound[strId])
	      | _ => raise Fail "withTopStr: undefined structure"
	    (* end case *)
	  end
      | withTopStr _ = raise Fail "withTopStr: not at top-level"

    fun withFct (c as C{ctx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.FCTbound[fctId])
	      | _ => raise Fail "withFct: undefined functor"
	    (* end case *)
	  end
      | withFct _ = raise Fail "withFct: not at top-level"

    fun withFctArg (c as C{ctx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.ARGbound[fctId])
	      | _ => raise Fail "withFctArg: undefined functor"
	    (* end case *)
	  end
      | withFctArg _ = raise Fail "withFctArg: not at top-level"

    fun withSubstr (c as C{ctx, ...}, id) =
	  withContext (c, I.extendContext (ctx, Atom.atom id))

    fun withStrPath (c as C{info=Info{index, ...}, ...}, path) = let
	  val path = List.map Atom.atom path
	  in
	    case I.findContext (index, path)
	     of (SOME bindCtx) => withContext (c, bindCtx)
	      | _ =>
		  Error.error ("withSubstr: undefined structure " ^ joinPath path)
	    (* end case *)
	  end

  end;

