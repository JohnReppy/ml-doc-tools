(* ml-spec.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Utility routines for extracting ML specifications from the
 * ML-Doc tree representation.
 *)

structure MLSpec : sig

    structure M : MARKUP

    datatype tagged_id
      = ID of string
      | TAG of M.markup

    type ml_type = tagged_id MLDocType.ty

    datatype spec
      = BRspec of bool
      | INCLspec of (tagged_id * where_ty list) list
      | STRspec of (string * tagged_id * where_ty list)
      | STRSIGspec of (string * (spec * M.markup list) list)
      | SHARINGspec of sharing_spec list
      | EXNspec of (string * ml_type option) list
      | TYspec of {
	    eq : bool,
	    params : string option,
	    id : string,
	    def : ml_type option
	  } list
      | DTspec of {		(* a list of mutually recursive datatype specs *)
	    compact : bool,
	    params : string option,
	    id : string,
	    cons : (string * ml_type option * M.markup list) list
	  } list
      | DTDEFspec of {		(* a definition of a datatype *)
	    id : string,
	    def : tagged_id
	  } list
      | VALspec of (string * ml_type * M.markup list) list
    and sharing_spec
      = STRshare of string list
      | TYshare of string list
    and where_ty = WHEREty of {
	    params : string option,
	    id : string,
	    def : ml_type
	  }

    val getIdAndDocument : tagged_id -> (string * string option)
	(* gets the ID and optional external document
	 * from the markup of tagged ID.
	 *)

    val getTypeCons : M.markup list -> (string option * string * M.markup list)

    val getMLType : M.markup -> ml_type

    val getSpecContents : M.markup -> (spec * M.markup list)
	(* parse a SPEC element returning its representation as a spec
	 * value, and the contents of its COMMENT part.
	 *)

    val getSigbodyContents : M.markup list -> (spec * M.markup list) list
	(* given the body of a SIGBODY element, return the list of
	 * SPEC elements (each of which has one or more specs).
	 *)

    val emptyDescription : (spec * M.markup list) list -> bool
	(* return true if the given SIGBODY contents has no description
	 * elements.
	 *)

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup
    structure Ty = MLDocType
    structure SS = Substring

    datatype tagged_id
      = ID of string
      | TAG of M.markup

    type ml_type = tagged_id MLDocType.ty

    datatype spec
      = BRspec of bool
      | INCLspec of (tagged_id * where_ty list) list
      | STRspec of (string * tagged_id * where_ty list)
      | STRSIGspec of (string * (spec * M.markup list) list)
      | SHARINGspec of sharing_spec list
      | EXNspec of (string * ml_type option) list
      | TYspec of {
	    eq : bool,
	    params : string option,
	    id : string,
	    def : ml_type option
	  } list
      | DTspec of {		(* a list of mutually recursive datatype specs *)
	    compact : bool,
	    params : string option,
	    id : string,
	    cons : (string * ml_type option * M.markup list) list
	  } list
      | DTDEFspec of {		(* a definition of a datatype *)
	    id : string,
	    def : tagged_id
	  } list
      | VALspec of (string * ml_type * M.markup list) list
    and sharing_spec
      = STRshare of string list
      | TYshare of string list
    and where_ty = WHEREty of {
	    params : string option,
	    id : string,
	    def : ml_type
	  }

    local
      val sdataMark = SS.full "\\|"
    in
    val transData = M.stripData {
	    escape = String.str,
	    sdata = fn ss => SS.concat[sdataMark, ss, sdataMark],
	    special = fn _ => NONE
	  }
    end

    fun getID (M.ELEM{elem=E.ID, body=[M.DATA id], ...}) = transData id
      | getID _ = raise Fail "getID: not an ID"

    fun getTaggedId (elem as M.ELEM{elem=E.TYREF{...}, ...}) = TAG elem
      | getTaggedId (elem as M.ELEM{elem=E.SIGREF{...}, ...}) = TAG elem
      | getTaggedId (M.ELEM{elem=E.ID, body=[M.DATA id], ...}) = ID id
      | getTaggedId (M.DATA id) = ID id
      | getTaggedId (M.ELEM{elem, ...}) = Error.bogusElem("TYREF/SIGREF/DATA", elem)

    fun getIdAndDocument (ID id) = (id, NONE)
      | getIdAndDocument (TAG elem) = (case elem
	   of (M.ELEM{elem=E.SIGREF{document, ...}, body=[M.DATA id], ...}) =>
		(id, document)
	    | (M.ELEM{elem=E.TYREF{document, ...}, body=[M.DATA id], ...}) =>
		(id, document)
	  (* end case *))

    fun getTypeCons (M.ELEM{elem=E.TYPARAM, body=[M.DATA p], ...} :: id :: r) =
	  (SOME p, getID id, r)
      | getTypeCons (id::r) = (NONE, getID id, r)
      | getTypeCons _ = raise Fail "getTypeCons"

    fun getMLType (M.ELEM{elem=E.TY, body, ...}) = Ty.parseTy getTaggedId body
      | getMLType _ = raise Fail "getMLType: not PCDATA"

    fun getWhereTy l = let
	  fun get (l, M.ELEM{elem=E.WHERETYPE, body, ...} :: r) = let
		val (optParams, id, [def]) = getTypeCons body
		in
		  get (WHEREty{params=optParams, id=id, def=getMLType def} :: l, r)
		end
	    | get (l, r) = (List.rev l, r)
	  in
	    get ([], l)
	  end

    fun getINCLUDE (_, id::r, cxt) = let
	  val (whereTys, _) = getWhereTy r
	  in
	    ((getTaggedId id, whereTys), cxt)
	  end
      | getINCLUDE _ = raise Fail "getINCLUDE"

    fun getSHARING (elem, body, cxt) = let
	  fun getList [id] = [getID id]
	    | getList (id :: M.ELEM{elem=E.EQU, ...} :: r) = (getID id) :: getList r
	    | getList _ = raise Fail "getSHARING.getList"
	  in
	    case elem
	     of E.SHARING{ty=true} => (TYshare(getList body), cxt)
	      | E.SHARING{ty=false} => (STRshare(getList body), cxt)
	      | _ => Error.bogusElem("SHARING", elem)
	    (* end case *)
	  end

    fun getEXN (_, [id], cxt) = ((getID id, NONE), cxt)
      | getEXN (_, [id, ty], cxt) = ((getID id, SOME(getMLType ty)), cxt)
      | getEXN _ = raise Fail "getEXN"

    fun getTYPE (elem, body, cxt) = let
	  val (params, id, body) = getTypeCons body
	  val eq = (case elem of E.EQTYPE => true | _ => false)
	  val def = (case body
		 of [] => NONE
		  | [ty] => SOME(getMLType ty)
		  | _ => raise Fail "getTYPE"
		(* end case *))
	  in
	    ({eq = eq, params = params, id = id, def = def}, cxt)
	  end

    fun getDATATYPE (E.DATATYPE{compact, recursive}, body, cxt) = let
	  val (params, id, body) = getTypeCons body
	(* CONS ::= ID, TY?, COMMENT? *)
	  fun getCONS (M.ELEM{elem=E.CONS, body=id::r, ...}) = let
		val (ty, r) = (case r
		       of ((e as M.ELEM{elem=E.TY, body, ...})::r) =>
			    (SOME(getMLType e), r)
			| _ => (NONE, r)
		    (* end case *))
		val comment = (case r
		       of (M.ELEM{elem=E.COMMENT, body, ...}::_) => body
			| _ => []
		      (* end case *))
		in
		  (getID id, ty, comment)
		end
	    | getCONS _ = raise Fail "getCONS"
	  in
	    ({	compact = compact,
		params = params,
		id = id,
		cons = List.map getCONS body
	      }, cxt)
	  end

    fun getDATATYPEDEF (E.DATATYPEDEF, body, cxt) = let
	(* NOTE: the DTD guarantees that there will be not type params, and
	 * that the body will be a single element.
	 *)
	  val (_, id, [body]) = getTypeCons body
	  in
	    ({id = id, def = getTaggedId body}, cxt)
	  end

    fun getRAISES (M.ELEM{elem=E.RAISES, body, ...}) = body
      | getRAISES elem = Error.bogusMarkup ("RAISES", elem)

    fun getVAL (_, [id, ty], cxt) = ((getID id, getMLType ty, []), cxt)
      | getVAL (_, [id, ty, raises], cxt) =
	  ((getID id, getMLType ty, getRAISES raises), cxt)
      | getVAL _ = raise Fail "getVAL"

    fun getSpecList (getSpec, cons) body = let
	  fun f ([], specs) = (cons(List.rev specs), [])
	    | f ([M.ELEM{elem=E.COMMENT, body, ...}], specs) =
		  (cons(List.rev specs), body)
	    | f (M.ELEM{elem, body, ...}::r, specs) = let
		  val (s, r) = getSpec(elem, body, r)
		  in
		    f (r, s::specs)
		  end
	    | f _ = raise Fail "getSpecList: unexpected PCDATA"
	  in
	    f (body, [])
	  end

    fun getSpecContents (M.ELEM{elem=E.SPECBREAK{newline}, ...}) =
	  (BRspec newline, [])
      | getSpecContents (M.ELEM{elem=E.SPEC, body, ...}) = (case body
	   of (M.ELEM{elem=E.INCLUDE, ...}::_) =>
		getSpecList (getINCLUDE, INCLspec) body
	    | (M.ELEM{elem=E.SUBSTRUCT, body, ...}::r) => getSUBSTRUCT (body, r)
	    | (M.ELEM{elem=E.SHARING _, ...}::r) =>
		getSpecList (getSHARING, SHARINGspec) body
	    | (M.ELEM{elem=E.EXN, ...}::_) =>
		getSpecList (getEXN, EXNspec) body
	    | (M.ELEM{elem=E.TYPE, ...}::_) =>
		getSpecList (getTYPE, TYspec) body
	    | (M.ELEM{elem=E.EQTYPE, ...}::_) =>
		getSpecList (getTYPE, TYspec) body
	    | (M.ELEM{elem=E.DATATYPE _, ...}::_) =>
		getSpecList (getDATATYPE, DTspec) body
	    | (M.ELEM{elem=E.DATATYPEDEF, ...}::_) =>
		getSpecList (getDATATYPEDEF, DTDEFspec) body
	    | (M.ELEM{elem=E.VAL, ...}::_) =>
		(getSpecList (getVAL, VALspec) body)
	    | _ => Error.bogusMarkupList ("SPEC contents", body)
	  (* end case *))
      | getSpecContents m = Error.bogusMarkup("SPEC", m)

    and getCOMMENT ([M.ELEM{elem=E.COMMENT, body, ...}]) = body
      | getCOMMENT _ = []

    and getSUBSTRUCT (body, cxt) = let
	  val comment = getCOMMENT cxt
	  in
	    case body
	     of (id :: M.ELEM{elem=E.SIGBODY _, body, ...} :: rest) =>
		  (STRSIGspec(getID id, getSigbodyContents body), comment)
	      | (id :: sigId :: rest) => let
		  val rest = (case rest
			 of (M.ELEM{elem=E.OPAQUE, ...}::r) => r
			  | r => r
			(* end case *))
		  val (wtl, []) = getWhereTy rest
		  in
		    (STRspec(getID id, getTaggedId sigId, wtl), comment)
		  end
	      | _ => raise Fail "getSUBSTRUCT"
	    (* end case *)
	  end

(** NOTE: we still need to do something about joining mutually recursive datatypes
 ** in adjacent spec groups.
 **)
    and getSigbodyContents body = List.map getSpecContents body

  (* return true if the given SIGBODY contents has no description
   * elements.
   *)
    fun emptyDescription l = let
	  fun chk (_, _::_) = false
	    | chk (STRSIGspec(_, body), []) = List.all chk body
	    | chk (DTspec cl, []) = let
		fun chkSpec {compact, params, id, cons} =
		      List.all (fn (_, _, []) => true | _ => false) cons
		in
		  List.all chkSpec cl
		end
	    | chk (_, []) = true
	  and chkList [] = true
	    | chkList (item::r) = chk item andalso chkList r
	  in
	    List.all chk l
	  end

  end (* MLSpec *)
