(* index-rep.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 * This module contains the definition of the internal representations of an index.
 *)

structure IndexRep =
  struct

    type name = Atom.atom

    datatype binding_context
      = TOPbound
      | SIGbound of name list	(* signature body *)
      | STRbound of name list	(* structure body *)
      | FCTbound of name list	(* functor body *)
      | ARGbound of name list	(* functor argument *)

  (* extend a binding context with a substructure ID.  This raises Fail
   * on TOPbound contexts.
   *)
    fun extendContext (TOPbound, id) =
	  raise Fail(String.concat["extendContext(TOPbound, ", Atom.toString id, ")"])
      | extendContext (SIGbound l, id) = SIGbound(l @ [id])
      | extendContext (STRbound l, id) = STRbound(l @ [id])
      | extendContext (FCTbound l, id) = FCTbound(l @ [id])
      | extendContext (ARGbound l, id) = ARGbound(l @ [id])

    fun compareContext (ctx1, ctx2) = let
	  val cmpPath = List.collate Atom.compare
	  in
	    case (ctx1, ctx2)
	     of (TOPbound, TOPbound) => EQUAL
	      | (TOPbound, _) => LESS
	      | (_, TOPbound) => GREATER
	      | (SIGbound p1, SIGbound p2) => cmpPath(p1, p2)
	      | (SIGbound _, _) => LESS
	      | (_, SIGbound _) => GREATER
	      | (STRbound p1, STRbound p2) => cmpPath(p1, p2)
	      | (STRbound _, _) => LESS
	      | (_, STRbound _) => GREATER
	      | (FCTbound p1, FCTbound p2) => cmpPath(p1, p2)
	      | (FCTbound _, _) => LESS
	      | (_, FCTbound _) => GREATER
	      | (ARGbound p1, ARGbound p2) => cmpPath(p1, p2)
	    (* end case *)
	  end

    datatype file_content
      = SECTION of {
	    title : string,
	    label : name option,
	    content : file_content list
	  }
      | INCLFILE of name
(* eventually, we should also include INTERFACE items *)

    and file_entry = FILEentry of {
	name : name,
	isEmpty : bool,
	title : string,
(** NEED TO DO SOMETHING ABOUT CONTENTS:
 **   Sections, InclFiles, Floats, and ??
 **)
	content : file_content list ref
      }

    datatype anchor_entry = ANCHORentry of {
	file : file_entry,
	tag : name
      }

    datatype label_kind = Section | Table | Figure | Code | Interface

    datatype label_entry = LABELentry of {
	name : name,
	kind : label_kind,
	file : file_entry
      }

    datatype index = IDX of {
	fileTbl : file_entry AtomTable.hash_table,
	anchorTbl : anchor_entry AtomTable.hash_table,
	labelTbl : label_entry AtomTable.hash_table,
	topEnv : env list ref,
	sigTbl : sig_entry AtomTable.hash_table,
	strTbl : str_entry AtomTable.hash_table,
	fctTbl : fct_entry AtomTable.hash_table
      }

    and env = ENV of {
	index : index,			(* the root index *)
	file : file_entry,		(* the file this is defined in *)
	context : binding_context,
	strTbl : str_entry AtomTable.hash_table,
	exnTbl : exn_entry AtomTable.hash_table,
	tyTbl : ty_entry AtomTable.hash_table,
	conTbl : ty_entry AtomTable.hash_table,
	valTbl : val_entry AtomTable.hash_table,
	includes : sig_entry list ref
      }

    and sig_entry = SIGentry of {
	id : name,
	body : env option ref,		(* NONE, if the environment is a *)
					(* place-holder. *)
	instances : binding_context list ref
					(* instances of this signature (should not be *)
					(* TOPbound or SIGbound). *)
      }

    and sig_ref
      = NamedSig of sig_entry		(* a signature name *)
      | AnonSig of env			(* an anonymous (explicit) signature *)
      | ExternSig			(* defined in some other document *)

    and str_entry = STRentry of {
	id : name,
	bodySig : sig_ref,
	file : file_entry,		(* the file this is defined in *)
	binding : env option		(* NONE for top-level structures, *)
					(* otherwise this is the containing env *)
      }

    and fct_entry = FCTentry of {
	id : name,
	argSig : sig_ref,
	bodySig : sig_ref,
	file : file_entry		(* the file this is defined in *)
      }

    and exn_entry = EXNentry of {
	id : name,
	binding : env
      }

    and ty_entry = TYentry of {
	id : name,
	kind : ty_kind,
	binding : env
      }
    and ty_kind = EQTYPE | TYPE | DATATYPE of name list | DATATYPEDEF

    and val_entry = VALentry of {
	id : name,
	binding : env
      }

    fun findFile (IDX{fileTbl, ...}, fileName) = (
	  case AtomTable.find fileTbl fileName
	   of NONE => raise Fail(concat[
		  "unable to find file \"", Atom.toString fileName, "\""
		])
	    | (SOME f) => f
	  (* end case *))

    fun findSig (IDX{sigTbl, ...}, sigId) = (
	  case (AtomTable.find sigTbl sigId)
	   of NONE => let
		val entry = SIGentry{id = sigId, body = ref NONE, instances = ref[]}
		in
		  AtomTable.insert sigTbl (sigId, entry);
		  entry
		end
	    | (SOME entry) => entry
	  (* end case *))

    fun defineSig (IDX{sigTbl, ...}, sigId, env) = (
	  case (AtomTable.find sigTbl sigId)
	   of NONE => let
		val entry = SIGentry{id = sigId, body = ref(SOME env), instances = ref[]}
		in
		  AtomTable.insert sigTbl (sigId, entry); entry
		end
	    | (SOME(entry as SIGentry{id, body as ref NONE, ...})) => (
		body := SOME env; entry)
	    | _ => raise Fail "redefinition of signature"
	  (* end case *))

    fun addSigInstance (NamedSig(SIGentry{instances, ...}), inst) = let
	  fun ins [] = [inst]
	    | ins (inst' :: r) = (case compareContext(inst, inst')
		 of LESS => inst' :: ins r
		  | EQUAL => inst' :: r		(* should we generate an error here? *)
		  | GREATER => inst :: inst' :: r
		(* end case *))
	  in
	    instances := ins (!instances)
	  end
      | addSigInstance _ = ()

    fun mkEnv {index, file, context} = ENV{
	    index = index,
	    file = file,
	    context = context,
	    strTbl = AtomTable.mkTable (4, Fail "substrTbl"),
	    exnTbl = AtomTable.mkTable (4, Fail "exnTbl"),
	    tyTbl = AtomTable.mkTable (8, Fail "tyTbl"),
	    conTbl = AtomTable.mkTable (8, Fail "conTbl"),
	    valTbl = AtomTable.mkTable (16, Fail "valTbl"),
	    includes = ref []
	  }

    fun mkIndex () = IDX{
	    fileTbl = AtomTable.mkTable (8, Fail "fileTbl"),
	    anchorTbl = AtomTable.mkTable (8, Fail "anchorTbl"),
	    labelTbl = AtomTable.mkTable (8, Fail "labelTbl"),
	    topEnv = ref[],
	    sigTbl = AtomTable.mkTable (8, Fail "sigTbl"),
	    strTbl = AtomTable.mkTable (8, Fail "strTbl"),
	    fctTbl = AtomTable.mkTable (8, Fail "fctTbl")
	  }

  end (* IndexRep *)
