(* latex-context.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure LaTeXContext : sig

    type context

    val context : {
	    outS : TextIO.outstream,
	    index : MLDocIndex.index,
	    config : MLDocConfig.configuration,
	    nodeMap : MLDocIndex.File.entry -> FileTree.file_nd
	  } -> context

    val streamOf : context -> TextIO.outstream
    val bindingContext : context -> MLDocIndex.binding_context

    val pr  : (context * string) -> unit
    val prc : (context * char) -> unit
    val prl : (context * string list) -> unit
    val prf : (context * string * Format.fmt_item list) -> unit

  (* change rendering context *)
    val headingContext : context -> context
    val codeContext : context -> context
    val protoContext : context -> context
    val mathContext : context -> context
    val indexContext : context -> context

  (* test rendering context *)
    val inHead : context -> bool
    val inCode : context -> bool

    val withContext : (context * MLDocIndex.binding_context) -> context
    val withSig     : (context * string) -> context
    val withTopStr  : (context * string) -> context
    val withFct     : (context * string) -> context
    val withFctArg  : (context * string) -> context
    val withSubstr  : (context * string) -> context
    val withStrPath : (context * string list) -> context

  (* resolve %XREF elements *)
    val resolveXRef : (context * MLDocElem.element * string) -> {
	    doc  : Document.doc_info option,
	    nolink : bool,
	    noindex : bool,
	    kind : ResolveRef.xref_kind,
	    path : MLDocIndex.binding_context,
	    name : string
	  } option

    val labelKind : (context * string) -> MLDocIndex.Label.kind
    val labelLevel : (context * string) -> int
    val translate : (context * string) -> string

    val dataToLaTeX : context -> string -> string

    val codeString : string -> string
    val protoString : string -> string
    val mathString : string -> string

  end = struct

    structure I = MLDocIndex
    structure SS = Substring
    structure F = Format

    datatype context_kind
      = C_TEXT | C_HEAD | C_MATH | C_PROTO | C_CODE | C_INDEX

    datatype context = C of {
	info : static_info,
	ctx : context_kind,		(* current rendering context *)
	bindCtx : I.binding_context	(* current binding context for free IDs *)
      }

    and static_info = Info of {
	out : TextIO.outstream,
	index : I.index,
	config : MLDocConfig.configuration,
	nodeMap : MLDocIndex.File.entry -> FileTree.file_nd,
	docCache : Document.doc_cache
      }

    fun context {outS, index, config, nodeMap} = C{
	    info = Info{
		out = outS, index = index,
		config = config, nodeMap = nodeMap,
		docCache = Document.mkCache config
	      },
	    ctx = C_TEXT,
	    bindCtx = I.TOPbound
	  }

    fun streamOf (C{info=Info{out, ...}, ...}) = out

    fun bindingContext (C{bindCtx, ...}) = bindCtx

    fun lookupLabel (index, label) = (
	  case I.findLabel(index, Atom.atom label)
	   of NONE => Error.error'["unknown label \"", label, "\""]
	    | SOME entry => entry
	  (* end case *))

    fun labelKind (C{info=Info{index, ...}, ...}, label) =
	  I.Label.kind (lookupLabel (index, label))

    fun labelLevel (ctx as C{info=Info{index, nodeMap, ...}, ...}, label) = let
	  val label' = Atom.atom label
	  val entry = lookupLabel (index, label)
	  val file = I.Label.file entry
	  val FileTree.FILE{level, ...} = nodeMap file
	  fun depth (n, []) = NONE
	    | depth (n, I.File.SECTION{label=SOME l, content, ...}::r) =
		if Atom.sameAtom(l, label')
		  then SOME n
		  else (case depth(n+1, content)
		     of NONE => depth (n, r)
		      | someN => someN
		    (* end case *))
	    | depth (n, I.File.SECTION{label=NONE, content, ...}::r) = (
		case depth(n+1, content)
		 of NONE => depth (n, r)
		  | someN => someN
		(* end case *))
	    | depth (n, I.File.INCLFILE _ :: r) = depth(n, r)
	  in
	    case depth (level, I.File.content file)
	     of SOME d => d
	      | NONE => raise Fail(concat["section label \"", label, "\" not found"])
	    (* end case *)
	  end

    local
      fun updateCtx (C{info, bindCtx, ...}, ctx) =
	    C{info=info, ctx=ctx, bindCtx=bindCtx}
    in
    fun headingContext ctx = updateCtx (ctx, C_HEAD)
    fun codeContext ctx = updateCtx (ctx, C_CODE)
    fun protoContext ctx = updateCtx (ctx, C_PROTO)
    fun mathContext ctx = updateCtx (ctx, C_MATH)
    fun indexContext ctx = updateCtx (ctx, C_INDEX)
    end

    fun withContext (C{info as Info{index, ...}, ctx, ...}, bindCtx) = C{
	    info = info,
	    ctx = ctx,
	    bindCtx = I.canonicalContext(index, bindCtx)
	  }

    fun withSig (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val sigId = Atom.atom id
	  in
	    case I.findSig (index, sigId)
	     of (SOME _) => withContext(c, I.SIGbound[sigId])
	      | _ => raise Fail "withSig: undefined signature"
	    (* end case *)
	  end
      | withSig _ = raise Fail "withSig: not at top-level"

    fun withTopStr (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val strId = Atom.atom id
	  in
	    case I.findStr (index, strId)
	     of (SOME _) => withContext(c, I.STRbound[strId])
	      | _ => raise Fail "withTopStr: undefined structure"
	    (* end case *)
	  end
      | withTopStr _ = raise Fail "withTopStr: not at top-level"

    fun withFct (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.FCTbound[fctId])
	      | _ => raise Fail "withFct: undefined functor"
	    (* end case *)
	  end
      | withFct _ = raise Fail "withFct: not at top-level"

    fun withFctArg (c as C{bindCtx=I.TOPbound, info=Info{index, ...}, ...}, id) = let
	  val fctId = Atom.atom id
	  in
	    case I.findFct (index, fctId)
	     of (SOME _) => withContext(c, I.ARGbound[fctId])
	      | _ => raise Fail "withFctArg: undefined functor"
	    (* end case *)
	  end
      | withFctArg _ = raise Fail "withFctArg: not at top-level"

    fun withSubstr (c as C{bindCtx, ...}, id) =
	  withContext (c, I.extendContext (bindCtx, Atom.atom id))

    fun withStrPath (c as C{info=Info{index, ...}, ...}, qid) = let
	  val path = List.map Atom.atom qid
	  in
	    case I.findContext (index, path)
	     of (SOME bindCtx) => withContext (c, bindCtx)
	      | _ => let
		  fun f [id] = [id]
		    | f (id::r) = id :: "." :: f r
		  val msg = String.concat("withSubstr: undefined structure " :: f qid)
		  in
		    Error.error msg
		  end
	    (* end case *)
	  end

    fun inHead (C{ctx=C_HEAD, ...}) = true
      | inHead _ = false
    fun inCode (C{ctx=C_CODE, ...}) = true
      | inCode (C{ctx=C_PROTO, ...}) = true
      | inCode _ = false

    fun pr (cxt : context, s) = TextIO.output(streamOf cxt, s)
    fun prc (cxt : context, c) = TextIO.output1(streamOf cxt, c)
    fun prl (cxt : context, items) = pr(cxt, concat items)
    fun prf (cxt : context, fmt, items) = pr(cxt, F.format fmt items)

  (* resolve cross references *)
    fun resolveXRef (C{bindCtx, info=Info{index, docCache, ...}, ...}, xref, id) =
	  ResolveRef.resolve (index, docCache) (bindCtx, xref, id)

  (* context sensitive character translation *)
    fun translate (C{ctx=C_CODE, ...} : context, s) = LaTeXTranslate.transCodeData s
      | translate (C{ctx=C_PROTO, ...}, s) = LaTeXTranslate.transProtoData s
      | translate (C{ctx=C_INDEX, ...}, s) = LaTeXTranslate.transIndexView s
      | translate (_, s) = LaTeXTranslate.transData s

  (* latexString -- replace a string with latex string.  This function does
   * not do SGML data translation.
   *)
    local
      datatype char_class = LOWER | UPPER | OTHER
      fun classify c = if (Char.isUpper c) then UPPER
	    else if (Char.isLower c) then LOWER
	    else OTHER
      val hyphen = SS.full "\\-"
    in
    fun latexString (hyphenate, s) = let
	  fun prefix (start, pos) =
		SS.slice(start, 0, SOME(SS.size start - SS.size pos))
	  fun scan (start, prevCls, ss) = (case SS.getc ss
		 of NONE => [start]
		  | SOME(#"\\", ss) => (case (SS.getc ss)
		       of SOME(#"{", ss) => scan (start, OTHER, ss)
			| SOME(#"}", ss) => scan (start, OTHER, ss)
			| SOME(#"#", ss) => scan (start, OTHER, ss)
			| SOME(#"%", ss) => scan (start, OTHER, ss)
			| SOME(#"$", ss) => scan (start, OTHER, ss)
			| SOME(_, ss) => let
			  (* eatCmd skips the argument of a command.  Note
			   * that we only handle single argument commands,
			   * but the transData function only uses single
			   * argument commands, so we should be okay.
			   *)
			    fun eatCmd (lvl, ss) = (case SS.getc ss
				   of NONE => [start]
				    | SOME(#"{", ss') => eatCmd(lvl+1, ss')
				    | SOME(#"}", ss') =>
					if (lvl = 1)
					  then scan (start, OTHER, ss')
					  else eatCmd(lvl-1, ss')
				    | SOME(#"\\", ss') => (case SS.getc ss'
					 of NONE => raise Fail "bogus backslash"
					  | (SOME(_, ss')) => eatCmd(lvl, ss')
					(* end case *))
				    | SOME(_, ss') => eatCmd(lvl, ss')
				  (* end case *))
			    in
			      eatCmd (0, SS.dropl Char.isAlpha ss)
			    end
			| _ => raise Fail "bogus backslash"
		      (* end case *))
		  | SOME(#".", ss') => (case SS.getc ss'
		       of (SOME(c, ss'')) => if (Char.isAlpha c) andalso hyphenate
			    then prefix (start, ss')
			      :: hyphen :: scan(ss', classify c, ss'')
			    else scan(start, OTHER, ss')
			| NONE => [start]
		      (* end case *))
		  | SOME(c, ss') => if hyphenate
		      then (case (prevCls, classify c)
		         of (LOWER, UPPER) =>
			      prefix(start, ss) :: hyphen :: scan(ss, UPPER, ss')
			  | (_, cls) => scan (start, cls, ss')
		        (* end case *))
		      else scan (start, prevCls, ss')
		(* end case *))
	  val ss = SS.full s
	  in
	    SS.concat (scan (ss, OTHER, ss))
	  end (* latexString *)
    end (* local *)

    fun codeString s = latexString(false, LaTeXTranslate.transCodeData s)
    fun protoString s = latexString(true, LaTeXTranslate.transProtoData s)
    fun mathString s = latexString(false, LaTeXTranslate.transData s)

    fun dataToLaTeX (C{ctx, ...}) data = (case ctx
	   of C_CODE => codeString data
	    | C_PROTO => protoString data
	    | C_INDEX => latexString(false, LaTeXTranslate.transIndexView data)
	    | _ => latexString(true, LaTeXTranslate.transData data)
	  (* end case *))

  end
