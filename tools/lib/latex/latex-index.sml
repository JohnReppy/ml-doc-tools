(* latex-index.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure LaTeXIndex : sig

    datatype xref_kind = datatype ResolveRef.xref_kind

  (* index entry for mention of identifier *)
    val indexXRef : (LaTeXContext.context * MLDocElem.element * string) -> string
  (* index entry for specification of identifier *)
    val indexSpec : (LaTeXContext.context * xref_kind * string) -> string
  (* index entry for description of identifier *)
    val indexDesc : (LaTeXContext.context * xref_kind * string) -> string

  (* "see" entry for signature instance *)
    val sigInstance : {sigId : string, strId : string} -> string

  (* raises-index entry for function that raises an exception *)
    val raises : {ctx : LaTeXContext.context, exn : string, f : string} -> string
  (* raises-index entry for definition point of exception *)
    val exnRaisesDesc : (LaTeXContext.context * string) -> string

  (* begin/end index entries for sections defining signatures, etc. *)
    val beginSect : (xref_kind * string list) -> string
    val endSect : (xref_kind * string list) -> string

  (* support for INDEX elements *)
    val index : {
	    pr : string -> unit,
	    inline : MLDocMarkup.markup -> unit
	  } -> {
	    key : string,
	    see : string option,
	    mark : MLDocElem.index_mark,
	    which : string option,
	    content : MLDocMarkup.markup list
	  } -> unit

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup
    structure F = Format

    datatype xref_kind = datatype ResolveRef.xref_kind

  (* translate index strings for the lhs of the "@" (i.e., the key) *)
    local
      val trans = M.transData {
	      escape = fn #"\\" => "\\\\" | c => String.str c,
	      sdata = EntitiesToLaTeX.transEntity,
	      special =
		  fn #"!" => SOME "\"!"
		   | #"@" => SOME "\"@"
		   | #"\"" => SOME "\"\""
		   | #"|" => SOME "\"|"
		   | _ => NONE
	    }
    in
    fun lhsIndexStr str = trans (LaTeXIdFamily.strip str)
    end

  (* translate index strings for the rhs of the "@" (the literal presentation) *)
    fun rhsIndexStr str = LaTeXIdFamily.fmt LaTeXTranslate.transIndexView str

  (* map a function over a list while reversing the list *)
    fun revmap f = List.foldl (fn (x, xs) => f x :: xs) []

  (* map a list of names to a qualified name *)
    fun joinPath path = F.STR(String.concatWith "." path)

  (* kinds of index entries: specification, description, and cross-reference *)
    datatype index_entry = SPECIX | DESCIX | XREFIX

  (* page formating *)
    fun indexFmt SPECIX = F.STR "|mldSPECIXPP"
      | indexFmt DESCIX = F.STR "|mldDESCIXPP"
      | indexFmt XREFIX = F.STR ""

    fun fmtId (cmd, id, idKind, ixKind) =
	  F.format "\\mldIDIX{%s %s@{\\texttt{%s} %s}%s}" [
	      F.STR(lhsIndexStr id), F.STR idKind,
	      F.STR(rhsIndexStr id), F.STR idKind,
	      indexFmt ixKind
	    ]

    fun fmtTopId (cmd, id, idKind, ixKind) =
	  F.format "\\mldIDIX{%s %s@{\\texttt{%s} %s}!top level%s}" [
	      F.STR(lhsIndexStr id), F.STR idKind,
	      F.STR(rhsIndexStr id), F.STR idKind,
	      indexFmt ixKind
	    ]

    fun fmtQId (cmd, id, idKind, path, ixKind) =
	  F.format "\\mldIDIX{%s %s@{\\texttt{%s} %s}!%s@\\texttt{%s}%s}" [
	      F.STR(lhsIndexStr id), F.STR idKind,
	      F.STR(rhsIndexStr id), F.STR idKind,
	      joinPath(revmap lhsIndexStr path),
	      joinPath(revmap rhsIndexStr path), 
	      indexFmt ixKind
	    ]

    fun indexCmd (ixKind, SigRef, [sigId]) =
	  fmtId ("\\mldSIGIDIX", sigId, "signature", ixKind)
      | indexCmd (ixKind, StrRef, [strId]) =
	  fmtId ("\\mldSTRIDIX", strId, "structure", ixKind)
      | indexCmd (ixKind, StrRef, strId::path) =
	  fmtQId ("\\mldSTRIDIX", strId, "structure", path, ixKind)
      | indexCmd (ixKind, FctRef, [fctId]) =
	  fmtId ("\\mldFCTIDIX", fctId, "functor", ixKind)
      | indexCmd (ixKind, FctArgRef, [fctId]) = raise Fail "FctArgRef not implemented"
      | indexCmd (ixKind, TyRef, [id]) =
	  fmtTopId ("\\mldTYIDIX", id, "type", ixKind)
      | indexCmd (ixKind, TyRef, id::path) =
	  fmtQId ("\\mldTYIDIX", id, "type", path, ixKind)
      | indexCmd (ixKind, ExnRef, [id]) =
	  fmtTopId ("\\mldEXNIDIX", id, "exception", ixKind)
      | indexCmd (ixKind, ExnRef, id::path) =
	  fmtQId ("\\mldEXNIDIX", id, "exception", path, ixKind)
      | indexCmd (ixKind, ConRef, [id]) =
	  fmtTopId ("\\mldCONIDIX", id, "constructor", ixKind)
      | indexCmd (ixKind, ConRef, id::path) =
	  fmtQId ("\\mldCONIDIX", id, "constructor", path, ixKind)
      | indexCmd (ixKind, ValRef, [id]) =
	  fmtTopId ("\\mldVALIDIX", id, "value", ixKind)
      | indexCmd (ixKind, ValRef, id::path) =
	  fmtQId ("\\mldVALIDIX", id, "value", path, ixKind)

    fun revPath MLDocIndex.TOPbound = []
      | revPath (MLDocIndex.SIGbound path) = revmap Atom.toString path
      | revPath (MLDocIndex.STRbound path) = revmap Atom.toString path
      | revPath (MLDocIndex.FCTbound path) = revmap Atom.toString path
      | revPath (MLDocIndex.ARGbound path) = revmap Atom.toString path

    fun concatPath (bctx, id) = id :: revPath bctx

    fun indexXRef (ctx, elem, id) = (case LaTeXContext.resolveXRef(ctx, elem, id)
	   of SOME{doc=NONE, noindex=false, kind, path, name, ...} =>
		indexCmd (XREFIX, kind, concatPath(path, name))
	    | SOME _ => ""
	    | NONE => (
		Error.warning ("unable to resolve reference to %s", [F.STR id]);
		"")
	   (* end case *))

    fun indexSpec (ctx, kind as (StrRef | TyRef | ExnRef | ConRef | ValRef), id) =
	  indexCmd (SPECIX, kind, concatPath (LaTeXContext.bindingContext ctx, id))

    fun indexDesc (ctx, kind as (StrRef | TyRef | ExnRef | ConRef | ValRef), id) =
	  indexCmd (DESCIX, kind, concatPath (LaTeXContext.bindingContext ctx, id))

  (* "see" entry for signature instance *)
    fun sigInstance {sigId, strId} = indexCmd (SPECIX, StrRef, [strId])
(* The following code needs hyphenation to work properly:
    fun sigInstance {sigId, strId} = F.format
	  "\\mldSEE[\\mldIdIndex]{%s structure@\\texttt{%s} structure}\
	  \{\\texttt{%s}}"
	  [F.STR(lhsIndexStr strId), F.STR(rhsIndexStr strId), F.STR(rhsIndexStr sigId)]
*)

  (* raises index entry for function that raises an exception *)
(* NOTE: if we had the exn element, then we could use the resolveXRef(ctx, elem, id) function here *)
    fun raises {ctx, f, exn} = let
	  val path = concatPath (LaTeXContext.bindingContext ctx, f)
	  in
	    F.format
	      "\\mldRAISEIX{%s exception@{\\texttt{%s} exception}!\
	      \raised by %s@{raised by \\texttt{%s}}}"
	      [ F.STR(lhsIndexStr exn), F.STR(rhsIndexStr exn),
		joinPath(revmap lhsIndexStr path), joinPath(revmap rhsIndexStr path)
	      ]
	  end

    fun exnRaisesDesc (ctx, exn) = let
	  val path = revPath (LaTeXContext.bindingContext ctx)
	  in
	    F.format
	      "\\mldRAISEIX{%s exception@{\\texttt{%s} exception}!\
	      \defined in %s@{defined in \\texttt{%s}}}"
	      [ F.STR(lhsIndexStr exn), F.STR(rhsIndexStr exn),
		joinPath(revmap lhsIndexStr path), joinPath(revmap rhsIndexStr path)
	      ]
	  end

    fun beginSect (kind as (SigRef | StrRef | FctRef), path) =
	  raise Fail "beginSect"
    fun endSect (kind as (SigRef | StrRef | FctRef), path) =
	  raise Fail "endSect"

  (* support for an INDEX element; the content of an index element
   * is:
   *	(KEY-VIEW?, (SUBINDEX, SUBINDEX?)?)
   *)
    fun index {pr, inline} {key, see, mark, which, content} = let
	  fun prWhich () = (case which
		 of NONE => ()
		  | SOME "topic" => pr "[\\mldTopicIndex]"
		  | SOME "id" => pr "[\\mldIdIndex]"
		  | SOME "raises" => pr "[\\mldRaisesIndex]"
		  | SOME ix => raise Fail(concat["unknown index \"", ix, "\""])
		(* end case *))
	(* take a list of keys and a list of optional views and construct an index
	 * entry.
	 *)
	  fun prKeys keys = let
		fun prKey (key, NONE) = let
		      val key = lhsIndexStr key
		      val view = rhsIndexStr key
		      in
			if (key = view)
			  then pr key
			  else pr(String.concat[key, "@{", view, "}"])
		      end
		  | prKey (key, SOME view) = let
		      val key = lhsIndexStr key
		      in
			pr (key ^ "@{");
			List.app inline view;
			pr "}"
		      end
		fun prWith [k] = prKey k
		  | prWith (k::r) = (prKey k; pr "!"; prWith r)
		in
		  pr "{"; prWith keys; pr "}"
		end
	  fun subkeys (M.ELEM{elem=E.SUBINDEX{key}, body, ...} :: r) = let
		val view = (case body
		       of [M.ELEM{elem=E.KEY_VIEW, body, ...}] => SOME body
			| _ => NONE
		      (* end case *))
		in
		  (key, view) :: subkeys r
		end
	    | subkeys [] = []
	  val keys = (case content
		 of [] => [(key, NONE)]
		  | (M.ELEM{elem=E.KEY_VIEW, body, ...}::r) =>
		      (key, SOME body) :: subkeys r
		  | _ => (key, NONE)::subkeys content
		(* end case *))
	  in
	    case (see, mark)
	     of (NONE, E.HERE) => (
		  pr "\\mldINDEX";
		  prWhich ();
		  prKeys keys;
		  pr "%\n")
	      | (NONE, E.START) => (
		  pr "\\mldSTART";
		  prWhich ();
		  prKeys keys;
		  pr "%\n")
	      | (NONE, E.STOP) => (
		  pr "\\mldSTOP";
		  prWhich ();
		  prKeys keys;
		  pr "%\n")
	      | (SOME see, _) => (
		  pr "\\mldSEE";
		  prWhich ();
		  prKeys keys;
		  pr(concat["{", lhsIndexStr see, "}%\n"]))
	    (* end case *)
	  end

  end
