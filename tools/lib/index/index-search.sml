(* index-search.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 *)

structure IndexSearch : sig

    type name = IndexRep.name
    type index = IndexRep.index
    type env = IndexRep.env

    type qual_id = (IndexRep.binding_context * name)

    val findContext : (index * name list) -> IndexRep.binding_context option
	(* given a path, resolve it to a context *)

    val contextToEnv : (index * IndexRep.binding_context) -> IndexRep.env
	(* return the environment associated with the context.  Raise Fail
	 * on TOPbound and undefined contexts.
	 *)

  (* search for global objects *)
    val findFile   : (index * name) -> IndexRep.file_entry option
    val findAnchor : (index * name) -> IndexRep.anchor_entry option
    val findLabel  : (index * name) -> IndexRep.label_entry option

  (* search for top-level modules *)
    val findSig : (index * name) -> IndexRep.sig_entry option
    val findStr : (index * name) -> IndexRep.str_entry option
    val findFct : (index * name) -> IndexRep.fct_entry option

  (* find qualified objects *)
    val findSubstr : (index * qual_id) -> IndexRep.str_entry option
    val findType   : (index * qual_id) -> IndexRep.ty_entry option
    val findExn    : (index * qual_id) -> IndexRep.exn_entry option
    val findCon    : (index * qual_id) -> IndexRep.ty_entry option
    val findVal    : (index * qual_id) -> IndexRep.val_entry option

  end  =  struct

    structure IR = IndexRep

    type name = IR.name
    type index = IR.index
    type env = IR.env

    type qual_id = (IndexRep.binding_context * name)

  (* search for global objects *)
    fun findFile (IR.IDX{fileTbl, ...}, id) = AtomTable.find fileTbl id
    fun findAnchor (IR.IDX{anchorTbl, ...}, id) = AtomTable.find anchorTbl id
    fun findLabel (IR.IDX{labelTbl, ...}, id) = AtomTable.find labelTbl id

  (* search for top-level modules *)
    fun findSig (IR.IDX{sigTbl, ...}, id) = AtomTable.find sigTbl id
    fun findStr (IR.IDX{strTbl, ...}, id) = AtomTable.find strTbl id
    fun findFct (IR.IDX{fctTbl, ...}, id) = AtomTable.find fctTbl id

  (* map a binding context to the corresponding environment.  We return a
   * list of environments, since the topEnv is represented as a list.
   *)
    fun ctxToEnv (IR.IDX{topEnv, ...}, IR.TOPbound) = !topEnv
      | ctxToEnv (index, ctx) = let
	  fun sigRefToEnv (IR.NamedSig sigEntry) = sigToEnv sigEntry
	    | sigRefToEnv (IR.AnonSig env) = SOME env
	  and sigToEnv (IR.SIGentry{body, ...}) = !body
	  fun walkPath (SOME env, []) = [env]
	    | walkPath (NONE, _) = []
	    | walkPath (SOME(IR.ENV{strTbl, includes, ...}), id::r) = (
		case AtomTable.find strTbl id
		 of NONE => let
		      fun chkIncl [] = []
			| chkIncl (inclSig::rest) = (
			    case walkPath (sigToEnv inclSig, id::r)
			     of [] => chkIncl rest
			      | el => el
			    (* end case *))
		      in
			chkIncl (! includes)
		      end
		  | SOME(IR.STRentry{bodySig, ...}) =>
		      walkPath (sigRefToEnv bodySig, r)
		(* end case *))
	  in
	    case ctx
	     of IR.SIGbound(id::path) => (case findSig(index, id)
		   of NONE => []
		    | (SOME sigEntry) => walkPath (sigToEnv sigEntry, path)
		  (* end case *))
	      | IR.STRbound(id::path) => (case findStr(index, id)
		   of NONE => []
		    | SOME(IR.STRentry{bodySig, ...}) =>
			walkPath (sigRefToEnv bodySig, path)
		  (* end case *))
	      | IR.FCTbound(id::path) => (case findFct(index, id)
		   of NONE => []
		    | SOME(IR.FCTentry{bodySig, ...}) =>
			walkPath (sigRefToEnv bodySig, path)
		  (* end case *))
	      | IR.ARGbound(id::path) => (case findFct(index, id)
		   of NONE => []
		    | SOME(IR.FCTentry{argSig, ...}) =>
			walkPath (sigRefToEnv argSig, path)
		  (* end case *))
	      | _ => raise Fail "bogus context"
	    (* end case *)
	  end

    fun searchEnvList selFn (index, (ctx, id) : qual_id) = let
	  fun sigToEnv (IR.SIGentry{body, ...}) = !body
	  fun search [] = NONE
	    | search ((env as IR.ENV{includes, ...})::r) = (
		case AtomTable.find (selFn env) id
		 of NONE => search ((List.mapPartial sigToEnv (!includes)) @ r)
		  | entry => entry
		(* end case *))
	  in
	    search (ctxToEnv (index, ctx))
	  end

    val findSubstr = searchEnvList (fn (IR.ENV{strTbl, ...}) => strTbl)
    val findType = searchEnvList (fn (IR.ENV{tyTbl, ...}) => tyTbl)
    val findExn = searchEnvList (fn (IR.ENV{exnTbl, ...}) => exnTbl)
    val findCon = searchEnvList (fn (IR.ENV{conTbl, ...}) => conTbl)
    val findVal = searchEnvList (fn (IR.ENV{valTbl, ...}) => valTbl)

  (* given a path, resolve it to a context *)
    fun findContext (_, []) = SOME IR.TOPbound
      | findContext (index as IR.IDX{sigTbl, strTbl, fctTbl, ...}, path as (h::_)) =
	  let
	  fun getContext ctx = (case ctxToEnv(index, ctx)
		 of [IR.ENV{context, ...}] => SOME context
		  | _ => NONE
		(* end case *))
	  in
	    case findSig(index, h)
	     of NONE => (case findStr(index, h)
		  of NONE => (case findFct(index, h)
		       of NONE => NONE
			| (SOME fctEntry) => getContext(IR.FCTbound path)
		      (* end case *))
		   | (SOME strEntry) => getContext(IR.STRbound path)
		(* end case *))
	      | (SOME sigEntry) => getContext(IR.SIGbound path)
	    (* end case *)
	  end

    fun contextToEnv (index, ctx) = (case ctxToEnv(index, ctx)
	   of [env] => env
	    | _ => raise Fail "contextToEnv: bogus context"
	  (* end case *))

  end
