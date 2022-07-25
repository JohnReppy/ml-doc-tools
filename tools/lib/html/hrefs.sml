(* hrefs.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Support for generating hyper-text names and links.  There are two
 * kinds of anchors generated in these documents:
 *
 *   1) A specification anchor
 *
 *   2) A description anchor
 *
 * and three forms of hypertext references:
 *
 *   1) A local reference from a specification to the corresponding description.
 *
 *   2) A local reference to a specification anchor.
 *
 *   3) A remote reference to a specification anchor.
 *
 * The format for the generated URLs is:
 *
 *	[[file] "#"] [ context-kind ":" ] qualified-id ":" kind [":SPEC"]
 *
 * where
 *
 *	context-kind  ::=  "SIG" | "STR" | "FCT" | "ARG"
 *	kind          ::=  "SIG" | "STR" | "FCT" | "SHARE" | "EXN" | "TY" | "VAL"
 *
 * The file name prefix is for non-local references only.  The context-kind
 * specifies the kind of the binding context (top-level, signature, etc.);
 * top-level identifiers have no context-kind prefix.  The qualified-id is
 * the full name of the identifier, and the kind is its class (signature,
 * value, etc.).  The optional "SPEC" as appended to the specification site
 * URL, while the other name is used for the definition site.  Constructor
 * IDs get mapped to their owning datatype SPEC URL for specification sites,
 * and to a VAL URL at their description site.
 *)

structure HRefs : sig

    datatype xref_kind
      = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

    val specURL : HTMLContext.context -> {
	    isRef : bool, kind : xref_kind, id : string
	  } -> string
	(* return the local specification URL *)

    val descURL : HTMLContext.context -> {
	    isRef : bool, kind : xref_kind, id : string
	  } -> string
	(* return the description URL *)

    val xrefURL : HTMLContext.context -> {
	    xref : MLDocElem.element, id : string
	  } -> string option
	(* return the URL from an %XREF element; this returns NONE for elements
	 * that have the NOLINK attribute.
	 *)

    val xrefToHTML : HTMLContext.context -> {
	    xref : MLDocElem.element, id : string
	  } -> (HTML.text -> HTML.text)
	(* convert an %XREF element to an HTML <A> element *)

    val remoteURL : MLDocIndex.index -> {
	    base : string option,
	    bindCtx : MLDocIndex.binding_context,
	    id : string, kind : xref_kind
	  } -> string

  (* Anchors and hyperlinks for sections *)
    val mkSectAnchor : (string option * int) -> HTML.text
    val mkSectRef : (string * string option * int * HTML.text) -> HTML.text

    val mkAnchor : string -> HTML.text
	  (* return an HTML anchor element with the given NAME *)
    val mkHRef : string -> (HTML.text -> HTML.text)
	  (* wrap an HTML hyperlink around some text *)

  end = struct

    structure C = HTMLContext
    structure M = MLDocMarkup
    structure E = MLDocElem
    structure I = MLDocIndex
    structure F = Format

    datatype xref_kind
      = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

  (* extract the file name from a file index entry *)
    fun fileName fileInfo = Atom.toString(I.File.name fileInfo)

  (* split a qualified ID into a list of strings *)
    val splitId = String.fields (fn #"." => true | _ => false)

  (* map xref_kind to string *)
    fun kindSuffix SigRef   = ":SIG"
      | kindSuffix StrRef   = ":STR"
      | kindSuffix FctRef   = ":FCT"
      | kindSuffix ShareRef = ":SHARE"
      | kindSuffix TyRef    = ":TY"
      | kindSuffix ExnRef   = ":EXN"
      | kindSuffix ConRef   = ":TY"
      | kindSuffix ValRef   = ":VAL"

    fun kindName SigRef   = "signature"
      | kindName StrRef   = "structure"
      | kindName FctRef   = "functor"
      | kindName TyRef    = "type"
      | kindName ShareRef = "sharing"
      | kindName ExnRef   = "exception"
      | kindName ConRef   = "constructor"
      | kindName ValRef   = "value"

    fun contextToString bindCtx = let
	  fun joinPath [id] = [Atom.toString id, "."]
	    | joinPath (id::r) = (Atom.toString id) :: "." :: joinPath r
	  val strs = (case bindCtx
		 of I.TOPbound => []
		  | (I.SIGbound p) => "SIG:" :: joinPath p
		  | (I.STRbound p) => "STR:" :: joinPath p
		  | (I.FCTbound p) => "FCT:" :: joinPath p
		  | (I.ARGbound p) => "ARG:" :: joinPath p
		(* end case *))
	  in
	    String.concat strs
	  end

    fun concatURL (ctx, true, l) = (case C.baseURL ctx
	   of NONE => String.concat("#" :: l)
	    | (SOME _) => String.concat(C.srcFile ctx :: ".html#" :: l)
	  (* end case *))
      | concatURL (_, false, l) = String.concat l

  (* return the local specification URL *)
    fun specURL ctx = let
	  val bindStr = contextToString (C.bindingContext ctx)
	  fun mkURL {isRef, kind, id} = concatURL (ctx, isRef, [
		  bindStr, id, kindSuffix kind, ":SPEC"
		])
	  in
	    mkURL
	  end

  (* return the description URL *)
    fun descURL ctx = let
	  val bindStr = contextToString (C.bindingContext ctx)
	  fun mkURL {isRef, kind, id} = concatURL (ctx, isRef, [
		  bindStr, id, kindSuffix kind
		])
	  in
	    mkURL
	  end

  (* make a remote reference URL.  This will be a SPEC URL. *)
    fun remoteURL index {base, bindCtx, id, kind} = let
	  fun mkLink (NONE, _, _, _) = (
		Error.warning("unable to find %s \"%s%s\"", [
		    F.STR(kindName kind), F.STR(contextToString bindCtx),
		    F.STR id
		  ]);
		"")
	    | mkLink (SOME entry, getFile, getCtx, getId) = (case base
		 of NONE => String.concat [
			fileName(getFile entry), ".html#",
			contextToString(getCtx entry),
			getId entry,
			kindSuffix kind,
			":SPEC"
		      ]
		  | (SOME b) => String.concat [
			"http://", b, "/", fileName(getFile entry), ".html#",
			contextToString(getCtx entry),
			getId entry,
			kindSuffix kind,
			":SPEC"
		      ]
		(* end case *))
	  val name = Atom.atom id
	  fun id' _ = id
	  fun top _ = I.TOPbound
	  in
	    case (bindCtx, kind)
	     of (I.TOPbound, SigRef) =>
		  mkLink (I.findSig(index, name), I.Sig.file, top, id')
	      | (I.TOPbound, StrRef) =>
		  mkLink (I.findStr(index, name), I.Str.file, top, id')
	      | (I.TOPbound, FctRef) =>
		  mkLink (I.findFct(index, name), I.Fct.file, top, id')
	      | (_, StrRef) => mkLink (
		    I.findSubstr(index, (bindCtx, name)),
		    I.Str.file,
		    I.Str.context,
		    id')
	      | (_, TyRef) => mkLink (
		    I.findType(index, (bindCtx, name)),
		    I.Ty.file,
		    I.Ty.context,
		    id')
	      | (_, ExnRef) => mkLink (
		    I.findExn(index, (bindCtx, name)),
		    I.Exn.file,
		    I.Exn.context,
		    id')
	      | (_, ConRef) => mkLink (
		    I.findCon(index, (bindCtx, name)),
		    I.Ty.file,
		    I.Ty.context,
		    Atom.toString o I.Ty.name)
	      | (_, ValRef) => mkLink (
		    I.findVal(index, (bindCtx, name)),
		    I.Val.file,
		    I.Val.context,
		    id')
	      | _ => raise Fail "mkRemoteRef: bogus context/ref"
	    (* end case *)
	  end
 

    fun xrefURL ctx {xref, id} = let
	  fun trXRefKind ResolveRef.SigRef = SigRef
	    | trXRefKind ResolveRef.StrRef = StrRef
	    | trXRefKind ResolveRef.FctRef = FctRef
	    | trXRefKind ResolveRef.FctArgRef = raise Fail "trXRefKind: FxtArgRef"
	    | trXRefKind ResolveRef.TyRef = TyRef
	    | trXRefKind ResolveRef.ExnRef = ExnRef
	    | trXRefKind ResolveRef.ConRef = ConRef
	    | trXRefKind ResolveRef.ValRef = ValRef
	  in
	    case xref
	     of E.AREF{document=NONE, tag} => (case C.findAnchor (ctx, tag)
		   of (SOME anchorEntry) => SOME(F.format "%s.html#%s" [
			    F.STR(fileName(I.Anchor.file anchorEntry)),
			    F.STR tag
			  ])
		    | NONE => (
			Error.warning ("unable to locate anchor \"%s\"",
			  [F.STR tag]);
			NONE)
		  (* end case *))
	      | (E.AREF _) => (
		  Error.warning ("external documents not yet supported!", []);
		  NONE)
	      | _ => (case C.resolveXRef (ctx, xref, id)
		   of SOME{
			doc=SOME{info, baseURL, ...}, nolink=false,
			kind, path, name, ...
		      } => SOME(remoteURL info {
			  base = SOME baseURL, bindCtx = path,
			  id = name, kind = trXRefKind kind
			})
		    | SOME{nolink=false, kind, path, name, ...} =>
			SOME(remoteURL (C.index ctx) {
			    base = NONE, bindCtx = path,
			    id = name, kind = trXRefKind kind
			  })
		    | _ => NONE
		  (* end case *))
	    (* end case *)
	  end

  (* wrap an HTML hyperlink element around some text *)
    fun mkHRef tag htext = MakeHTML.mkA_HREF{href=tag, content=htext}

  (* wrap an HTML anchor element around some text *)
    fun mkAnchor tag = MakeHTML.mkA_NAME{name=tag, content=HTML.TextList[]}

    fun xrefToHTML ctx = let
	  val xrefURL = xrefURL ctx
	  in
	    fn arg => (case (xrefURL arg)
	       of SOME tag => mkHRef tag
	        | NONE => (fn htext => htext)
	      (* end case *))
	  end

  (* tags for sections *)
    fun sectionTag (SOME lab, id) = "section:" ^ lab
      | sectionTag (NONE, id) = "section:" ^ Int.toString id

  (* Anchors for sections *)
    fun mkSectAnchor (label, id) = mkAnchor(sectionTag(label, id))

  (* Hyperlinks for sections *)
    fun mkSectRef (file, lab, id, text) =
	  mkHRef (concat[file, ".html#", sectionTag(lab, id)]) text

  end;
