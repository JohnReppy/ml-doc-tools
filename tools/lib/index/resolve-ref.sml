(* resolve-ref.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * TODO: map the path to the "true" path for the reference (i.e., change the
 * root context to the canonical context).
 *)

structure ResolveRef :> sig

    datatype xref_kind
      = SigRef | StrRef | FctRef | FctArgRef | TyRef | ExnRef | ConRef | ValRef

(*
    val canonical :
	  (MLDocIndex.index * MLDocIndex.binding_context) -> MLDocIndex.binding_context
*)

    val resolve : (MLDocIndex.index * Document.doc_cache)
	  -> (MLDocIndex.binding_context * MLDocElem.element * string)
	    -> {
	      doc : Document.doc_info option,
	      nolink : bool,
	      noindex : bool,
	      kind : xref_kind,
	      path : MLDocIndex.binding_context,
	      name : string
	    } option

  end = struct

    structure M = MLDocMarkup
    structure E = MLDocElem
    structure I = MLDocIndex

    datatype xref_kind
      = SigRef | StrRef | FctRef | FctArgRef | TyRef | ExnRef | ConRef | ValRef

    val splitId = String.fields (fn #"." => true | _ => false)
    val splitId' = (List.map Atom.atom) o splitId

    fun joinPath [] = []
      | joinPath [id] = [Atom.toString id, "."]
      | joinPath (id::r) = (Atom.toString id) :: "." :: joinPath r

(*
    fun canonical (_, I.TOPbound) = I.TOPbound
      | canonical (idx, bnd as I.SIGbound path) =
      | canonical (idx, bnd as I.STRbound path) = let
	  val 
      | canonical (idx, bnd as I.FCTbound path) =
      | canonical (idx, bnd as I.ARGbound path) = bnd
*)

  (* Merge the binding context and path of the identifier into a canonical
   * representation of the ID's context.
   * Specifically, let u, v, and w be (possibly empty) subsequences of an ML name
   * qualifier and let x be an ML identifier, then if the bindCtx is "uv" and the
   * qid is "vw.x", then the fully qualified name is taken to be "uvw.x".
   *)
    fun mergePaths (mkCxt, bindCxt, qid) = let
	  val (id :: path) = List.rev(splitId qid)
	  fun done (rest, suffix) =
		SOME(mkCxt(List.revAppend(rest, suffix)), id)
	  fun error () = (
		Error.warning(
		  "binding context \"%s\" and qualified ID %s disagree",
		  [Format.STR(concat(joinPath bindCxt)), Format.STR qid]);
		NONE)
	  fun overlap (m1::r1, m2::r2, suffix) = let
		val m2 = Atom.atom m2
		in
		  if Atom.sameAtom(m1, m2)
		    then overlap (r1, r2, m1::suffix)
		    else error()
		end
	    | overlap (r1, [], suffix) = done(r1, suffix)
	    | overlap ([], _, _) = error()
	  and merge (m1::r1, m2::r2, suffix) = let
		val m2 = Atom.atom m2
		in
		  if Atom.sameAtom(m1, m2)
		    then overlap (r1, r2, m1::suffix)
		    else merge (m1::r1, r2, m2::suffix)
		end
	    | merge (r1, [], suffix) = done(r1, suffix)
	    | merge ([], _, _) = error()
	  in
	    merge (List.rev bindCxt, path, [])
	  end

    fun mkTOPbound _ = I.TOPbound

    fun findRoot (bindCtx, home, id) = (case (bindCtx, home)
	   of (I.TOPbound, NONE) => mergePaths(mkTOPbound, [], id)
	    | (I.SIGbound ids, NONE) => mergePaths(I.SIGbound, ids, id)
	    | (I.STRbound ids, NONE) => mergePaths(I.STRbound, ids, id)
	    | (I.FCTbound ids, NONE) => mergePaths(I.FCTbound, ids, id)
	    | (I.ARGbound ids, NONE) => mergePaths(I.ARGbound, ids, id)
	    | (_, SOME E.TOPID) => mergePaths(mkTOPbound, [], id)
	    | (_, SOME(E.SIGID qid)) => mergePaths(I.SIGbound, splitId' qid, id)
	    | (_, SOME(E.STRID qid)) => mergePaths(I.STRbound, splitId' qid, id)
	    | (_, SOME(E.FCTID qid)) => mergePaths(I.FCTbound, splitId' qid, id)
	  (* end case *))

    fun resolve (idx, docCache) = let
	  fun docAndInfo NONE = (NONE, idx)
	    | docAndInfo (SOME docName) = let
	      (* strip the \| |\ that marks SDATA *)
		val docName = String.substring(docName, 2, size docName - 4)
		in
		  case Document.getDoc(docCache, docName)
		   of NONE => (NONE, idx)
		    | (someInfo as SOME{info, ...}) => (someInfo, info)
		  (* end case *)
		end
	  fun resolveRef (bindCtx, elem, id) = let
		fun mkTopRef (document, kind, nolink, noindex) = let
		      val (doc, idx) = docAndInfo document
		      in
			SOME{
			    doc = doc, kind = kind,
			    nolink = nolink, noindex = noindex,
			    path = I.TOPbound, name = id
			  }
		      end
		fun mkRef (document, kind, home, nolink, noindex) = let
		      val (doc, idx) = docAndInfo document
		      in
			case findRoot (bindCtx, home, id)
			 of NONE => NONE
			  | SOME(path, name) => SOME{
				doc = doc, kind = kind,
				nolink = nolink, noindex = noindex,
				path = path, name = name
			      }
			(* end case *)
		      end
		in
		  case elem
		   of E.SIGREF{document, nolink, noindex, ...} =>
			mkTopRef (document, SigRef, nolink, noindex)
		    | E.FCTREF{document, nolink, noindex, ...} =>
			mkTopRef (document, FctRef, nolink, noindex)
		    | E.FCTARGREF{document, nolink, noindex, ...} =>
			raise Fail "FCTARGREG not implemented"
		    | E.STRREF{document, home, nolink, noindex, ...} =>
			mkRef (document, StrRef, home, nolink, noindex)
		    | E.TYREF{document, home, nolink, noindex, ...} =>
			mkRef (document, TyRef, home, nolink, noindex)
		    | E.EXNREF{document, home, nolink, noindex, ...} =>
			mkRef (document, ExnRef, home, nolink, noindex)
		    | E.CONREF{document, home, nolink, noindex, ...} =>
			mkRef (document, ConRef, home, nolink, noindex)
		    | E.VALREF{document, home, nolink, noindex, ...} =>
			mkRef (document, ValRef, home, nolink, noindex)
		    | _ => Error.bogusElem("XREF", elem)
		  (* end case *)
		end
	  in
	    resolveRef
	  end

  end
