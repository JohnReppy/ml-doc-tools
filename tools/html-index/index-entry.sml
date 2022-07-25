(* index-entry.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure IndexEntry : sig

    datatype entry = E of {
	id : string,
	linkId : string,
	kind : HRefs.xref_kind,
	context : MLDocIndex.binding_context
      }

    datatype index_kind
      = AllIdx		(* unified alphapetical index *)
      | SigIdx		(* signature index *)
      | StrIdx		(* structure and functor index *)
      | ExnIdx		(* exception index *)
      | TyIdx		(* type index *)
      | ValIdx		(* value index *)

    val buildList : (MLDocIndex.index * index_kind) -> entry list

    val prEntry : entry -> unit

  end = struct

    structure I = MLDocIndex

    datatype entry = E of {
	id : string,
	linkId : string,
	kind : HRefs.xref_kind,
	context : I.binding_context
      }

    val transData = TextToHTML.transData

    fun prEntry (E{id, linkId, kind, context}) = let
	  fun pr s = TextIO.output(TextIO.stdOut, s)
	  fun prKind HRefs.SigRef = pr "SIG"
	    | prKind HRefs.StrRef = pr "STR"
	    | prKind HRefs.FctRef = pr "FCT"
	    | prKind HRefs.ExnRef = pr "EXN"
	    | prKind HRefs.TyRef  = pr "TY"
	    | prKind HRefs.ConRef = pr "CON"
	    | prKind HRefs.ValRef = pr "VAL"
	  fun prPath l = List.app (fn id => (pr(Atom.toString id); pr ".")) l
	  fun prContext (I.TOPbound) = pr "<top-bound>"
	    | prContext (I.SIGbound p) = (pr "SIG:"; prPath p)
	    | prContext (I.STRbound p) = (pr "STR:"; prPath p)
	    | prContext (I.FCTbound p) = (pr "FCT:"; prPath p)
	    | prContext (I.ARGbound p) = (pr "ARG:"; prPath p)
	  in
	    prContext context; pr id; pr ":"; prKind kind;
	    TextIO.flushOut TextIO.stdOut
	  end

    datatype index_kind
      = AllIdx		(* unified alphapetical index *)
      | SigIdx		(* signature index *)
      | StrIdx		(* structure and functor index *)
      | ExnIdx		(* exception index *)
      | TyIdx		(* type index *)
      | ValIdx		(* value index *)

  (* build a list of entries from an index *)
    fun buildList (index, indexKind) = let
	  fun doSig sigRec = E{
		  id = Atom.toString(I.Sig.name sigRec),
		  linkId = Atom.toString(I.Sig.name sigRec),
		  kind = HRefs.SigRef,
		  context = I.TOPbound
		}
	  fun doStr strRec = E{
		  id = Atom.toString(I.Str.name strRec),
		  linkId = Atom.toString(I.Str.name strRec),
		  kind = HRefs.StrRef,
		  context = I.Str.context strRec
		}
	  fun doFct fctRec = E{
		  id = Atom.toString(I.Fct.name fctRec),
		  linkId = Atom.toString(I.Fct.name fctRec),
		  kind = HRefs.FctRef,
		  context = I.TOPbound
		}
	  fun doExn exnRec = E{
		  id = Atom.toString(I.Exn.name exnRec),
		  linkId = Atom.toString(I.Exn.name exnRec),
		  kind = HRefs.ExnRef,
		  context = I.Exn.context exnRec
		}
	  fun doTy tyRec = E{
		  id = Atom.toString(I.Ty.name tyRec),
		  linkId = Atom.toString(I.Ty.name tyRec),
		  kind = HRefs.TyRef,
		  context = I.Ty.context tyRec
		}
	  fun doVal valRec = E{
		  id = transData(Atom.toString(I.Val.name valRec)),
		  linkId = Atom.toString(I.Val.name valRec),
		  kind = HRefs.ValRef,
		  context = I.Val.context valRec
		}
	  fun doCon tyRec = (case I.Ty.kind tyRec
		 of (I.Ty.DATATYPE cl) => let
		      val ctx = I.Ty.context tyRec
		      fun mkE id = E{
			      id = transData(Atom.toString id),
			      linkId = Atom.toString id,
			      kind = HRefs.ConRef,
			      context = ctx
			    }
		      in
			List.map mkE cl
		      end
		  | _ => []
		(* end case *))
	  fun inclItems k = (indexKind = AllIdx) orelse (indexKind = k)
	  fun fold k f l1 l2 =
		if (inclItems k)
		  then List.foldl (fn (x, l) => (f x)::l) l2 l1
		  else l2
	  val list = []
	  val list = fold SigIdx doSig (I.listSigs index) list
	  val list = fold StrIdx doStr (I.listTopStrs index) list
	  val list = fold StrIdx doFct (I.listFcts index) list
	  val list = fold StrIdx doStr (I.listAll I.Env.listStrs index) list
	  val list = fold ExnIdx doExn (I.listAll I.Env.listExns index) list
	  val list = fold TyIdx doTy (I.listAll I.Env.listTys index) list
	  val list = fold ValIdx doVal (I.listAll I.Env.listVals index) list
	  val list = if (inclItems ValIdx)
		then List.foldl (fn (x, l) => (doCon x) @ l) list
		  (I.listAll I.Env.listTys index)
		else list
	  in
	    list
	  end

  end;

