(* mldoc-index-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 * This is the external interface of an index.
 *
 * NOTE: we might want to split index queries from index construction, since
 * most tools only use the former.
 *)

signature MLDOC_INDEX =
  sig

    type index	(* the index of a collection of ML-Doc files *)
    type env	(* a binding environment *)
    type name = Atom.atom

    datatype binding_context
      = TOPbound
      | SIGbound of name list	(* signature body *)
      | STRbound of name list	(* structure body *)
      | FCTbound of name list	(* functor body *)
      | ARGbound of name list	(* functor argument *)

    type qual_id = (binding_context * name)

    val compareContext : (binding_context * binding_context) -> order

    val extendContext : (binding_context * name) -> binding_context
	(* extend a binding context with a substructure ID.  This raises Fail
	 * on TOPbound contexts.
	 *)

    val bindingToString : binding_context -> string

  (** Abstract interfaces to the various objects in the index **)

    structure File : sig
	type entry
	datatype file_content
	  = SECTION of {
		title : string,
		label : name option,
		content : file_content list
	      }
	  | INCLFILE of name
(* eventually, we should include INTERFACE items *)
	val name    : entry -> name
	val dir     : entry -> string
	val base    : entry -> string
	val isEmpty : entry -> bool
	val title   : entry -> string
	val content : entry -> file_content list
      end

    structure Anchor : sig
	type entry
	val tag  : entry -> name
	val file : entry -> File.entry
      end

    structure Label : sig
	type entry
	datatype kind = Section | Table | Figure | Code | Interface
	val name : entry -> name
	val kind : entry -> kind
	val file : entry -> File.entry
      end

    structure Sig : sig
	type entry
	val name : entry -> name
	val file : entry -> File.entry
	val env  : entry -> env
	val instances : entry -> binding_context list
      end

    structure Str : sig
	type entry
	val name : entry -> name
	val file : entry -> File.entry
	val context : entry -> binding_context
      end

    structure Fct : sig
	type entry
	val name : entry -> name
	val file : entry -> File.entry
      end

    structure Ty : sig
	datatype ty_kind = EQTYPE | TYPE | DATATYPE of name list | DATATYPEDEF
	type entry
	val name : entry -> name
	val kind : entry -> ty_kind
	val file : entry -> File.entry
	val context : entry -> binding_context
      end

    structure Exn : sig
	type entry
	val name : entry -> name
	val file : entry -> File.entry
	val context : entry -> binding_context
      end

    structure Val : sig
	type entry
	val name : entry -> name
	val file : entry -> File.entry
	val context : entry -> binding_context
      end

    structure Env : sig
	val file 	: env -> File.entry
	val listStrs	: env -> Str.entry list
	val listTys	: env -> Ty.entry list
	val listExns	: env -> Exn.entry list
	val listVals	: env -> Val.entry list
	val listCons	: env -> (name * Ty.entry) list
      end

    val listFiles	: index -> File.entry list
    val listAnchors	: index -> Anchor.entry list
    val listLabels	: index -> Label.entry list
    val listSigs	: index -> Sig.entry list
    val listTopStrs	: index -> Str.entry list
    val listFcts	: index -> Fct.entry list

    val listAll : (env -> 'a list) -> index -> 'a list

    val findContext      : (index * name list) -> binding_context option
    val contextToEnv     : (index * binding_context) -> env
    val canonicalContext : (index * binding_context) -> binding_context

  (* search for global objects *)
    val findFile   : (index * name) -> File.entry option
    val findAnchor : (index * name) -> Anchor.entry option
    val findLabel  : (index * name) -> Label.entry option

  (* search for top-level modules *)
    val findSig : (index * name) -> Sig.entry option
    val findStr : (index * name) -> Str.entry option
    val findFct : (index * name) -> Fct.entry option

  (* find qualified objects *)
    val findSubstr : (index * qual_id) -> Str.entry option
    val findType   : (index * qual_id) -> Ty.entry option
    val findExn    : (index * qual_id) -> Exn.entry option
    val findCon    : (index * qual_id) -> Ty.entry option
    val findVal    : (index * qual_id) -> Val.entry option

  (* Other index operations *)
    val parse   : string -> index
    val print   : (string * index) -> unit

  end;
