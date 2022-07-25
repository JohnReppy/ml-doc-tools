(* mldoc-module.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Utility routines for finding module elements in an ML-Doc markup tree.
 *)

structure MLDocModule : sig

    datatype status = REQUIRED | OPTIONAL | PROPOSED | OTHER

    datatype where_ty = datatype MLSpec.where_ty

    datatype sigbody
      = SIG_ID of {
	    sigId : string, doc : string option, whereTypes : where_ty list
	  }
      | SIG_SPEC of {sigId : string option, specs : MLDocMarkup.markup list}

    datatype fct_arg
      = FCTARG_ID of {strId : string, sigId : string, sigDoc : string option}
      | FCTARG_SPEC of {sigId : string option, specs : MLDocMarkup.markup list}

    datatype module
      = SIG of {
	    sigId : string,
	    status : status,
	    specs : MLDocMarkup.markup list,
	    structs : {
		strId : string,
		status : status,
		opaque : bool,
		whereTypes : where_ty list,
		comment : MLDocMarkup.markup list
	      } list
	  }
      | TOPSTR of {	(* top-level structure: <STRUCTURE> element *)
	    strId : string,
	    status : status,
	    opaque : bool,
	    sigBody : sigbody
	  }
      | SUBSTR of {	(* substructure: <STRUCTURE> element with qualified ID *)
	    context : string list,	(* path of parent module *)
	    strId : string,		(* unqualified structure name *)
	    opaque : bool,
	    sigBody : sigbody
	  }
      | FCT of {
	    fctId : string,
	    status : status,
	    argSpecs : fct_arg,
	    opaque : bool,
	    resSpecs : sigbody
	  }

    val parseModule : MLDocMarkup.markup -> module

    val findModules : MLDocContent.doc_body -> module list

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup

  (* a translation function for filenames *)
    val transData = M.stripData {
	    escape = String.str,
	    sdata = Substring.string,
	    special = fn _ => NONE
	  }

    datatype status = REQUIRED | OPTIONAL | PROPOSED | OTHER

    datatype where_ty = datatype MLSpec.where_ty

    datatype sigbody
      = SIG_ID of {
	    sigId : string, doc : string option, whereTypes : where_ty list
	  }
      | SIG_SPEC of {sigId : string option, specs : MLDocMarkup.markup list}

    datatype fct_arg
      = FCTARG_ID of {strId : string, sigId : string, sigDoc : string option}
      | FCTARG_SPEC of {sigId : string option, specs : MLDocMarkup.markup list}

    datatype module
      = SIG of {
	    sigId : string,
	    status : status,
	    specs : MLDocMarkup.markup list,
	    structs : {
		strId : string,
		status : status,
		opaque : bool,
		whereTypes : where_ty list,
		comment : MLDocMarkup.markup list
	      } list
	  }
      | TOPSTR of {	(* top-level structure *)
	    strId : string,
	    status : status,
	    opaque : bool,
	    sigBody : sigbody
	  }
      | SUBSTR of {	(* substructure *)
	    context : string list,
	    strId : string,
	    opaque : bool,
	    sigBody : sigbody
	  }
      | FCT of {
	    fctId : string,
	    status : status,
	    argSpecs : fct_arg,
	    opaque : bool,
	    resSpecs : sigbody
	  }

    fun getOpaque (M.ELEM{elem=E.OPAQUE, ...} :: rest) = (true, rest)
      | getOpaque stuff = (false, stuff)

(* NOTE: this code could probably be shared with ml-spec.sml *)
    fun getWhereTypes (M.ELEM{elem=E.WHERETYPE, body, ...} :: rest) = let
	  val (params, id, [ty]) = MLSpec.getTypeCons body
	  val def = MLSpec.getMLType ty
	  val (wtl, rest) = getWhereTypes rest
	  in
	    (WHEREty{params=params, id=id, def=def} :: wtl, rest)
	  end
      | getWhereTypes stuff = ([], stuff)

    fun cvtStatus NONE = OTHER
      | cvtStatus (SOME E.REQUIRED) = REQUIRED
      | cvtStatus (SOME E.OPTIONAL) = OPTIONAL
      | cvtStatus (SOME E.PROPOSED) = PROPOSED

    fun parseModule elem = let
	  fun getID (M.ELEM{elem=E.ID, body=[M.DATA id], ...}) = transData id
	    | getID elem = Error.bogusMarkup("ID", elem)
	  fun getSigID (M.ELEM{elem=E.ID, body=[M.DATA id], ...}) =
		(transData id, NONE)
	    | getSigID (M.ELEM{elem=E.SIGREF{document, ...}, body=[M.DATA id], ...}) =
		(transData id, document)
	    | getSigID elem = Error.bogusMarkup("%SIGID", elem)
	  fun getSigInstances body = let
		fun get ((M.ELEM{elem as E.SIGINSTANCE _, body, ...}) :: r, el) = let
		      val E.SIGINSTANCE{status, opaque} = elem
		      val (id, rest) = (case body
			     of (M.ELEM{elem=E.ID, body=[M.DATA id], ...} :: r) =>
				  (transData id, r)
			    (* end case *))
		      val (whereTypes, rest) = getWhereTypes rest
		      val comment = (case rest
			     of [] => []
			      | [M.ELEM{elem=E.COMMENT, body, ...}] => body
			    (* end case *))
		      val inst = {
			      strId = id,
			      status = cvtStatus status,
			      opaque = opaque,
			      whereTypes = whereTypes,
			      comment = comment
			    }
		      in
			get (r, inst::el)
		      end
		  | get (r, el) = (rev el, r)
		in
		  get (body, [])
		end
	  fun getSigBody elems = let
		val (opaque, elems) = getOpaque elems
		in
		  case elems
		   of (M.ELEM{elem=E.SIGBODY{sigid, ...}, body, ...} :: r) =>
			(opaque, SIG_SPEC{sigId=sigid, specs=body}, r)
		    | (idElem :: r) => let
			val (wtl, r) = getWhereTypes r
			val (id, doc) = getSigID idElem
			in
			  ( opaque,
			    SIG_ID{sigId = id, doc = doc, whereTypes = wtl},
			    r
			  )
			end
		  (* end case *)
		end
	  fun getFctArg (M.ELEM{elem=E.SIGBODY{sigid, ...}, body, ...} :: r) =
		(FCTARG_SPEC{sigId=sigid, specs=body}, r)
	    | getFctArg (id1 :: id2 :: r) = let
		val (sigId, doc) = getSigID id2
		in
		  (FCTARG_ID{strId=getID id1, sigId=sigId, sigDoc=doc}, r)
		end
	    | getFctArg _ = raise Fail "findModule.getFctArg"
	  val (module, r) = (case elem
		 of (M.ELEM{
			elem=E.SIGNATURE{sigid, status, ...},
			body=M.ELEM{elem=E.SIGBODY _, body, ...}::r, ...
		      }) => let
		      val (instances, r) = getSigInstances r
		      in
			(SIG{
			    sigId=sigid, status = cvtStatus status,
			    specs=body, structs=instances
			  }, r)
		      end
	          | (M.ELEM{
			elem=E.STRUCTURE{strid, status, ...}, body, ...
		      }) => (case String.fields (fn #"." => true | _ => false) strid
			 of [id] => let
			      val (opaque, sigBody, r) = getSigBody body
			      in
				( TOPSTR{
				      strId=id, status = cvtStatus status,
				      opaque=opaque, sigBody=sigBody
				    },
				  r
				)
			      end
			  | ids => let
			      fun f ([id], l) = (rev l, id)
				| f (id::r, l) = f (r, id::l)
			      val (path, id) = f (ids, [])
			      val (opaque, sigBody, r) = getSigBody body
			      in
				( SUBSTR{
				      context=path, strId=id,
				      opaque=opaque,
				      sigBody=sigBody
				    },
				  r
				)
			      end
			(* end case *))
		  | (M.ELEM{elem=E.FUNCTOR{fctid, status, ...}, body, ...}) => let
		      val (fctArg, r) = getFctArg body
		      val (opaque, resSpecs, r) = getSigBody r
		      in
			(FCT{
			    fctId=fctid,
			    status = cvtStatus status,
			    argSpecs=fctArg,
			    opaque=opaque,
			    resSpecs=resSpecs
			  }, r)
		      end
		  | _ => Error.bogusMarkup("module", elem)
		(* end case *))
	  in
	    module
	  end

  (* return the list of modules that are defined in a ML-Doc document *)
    fun findModules docBody = let
	  fun walkSect (MLDocContent.SECTION{content, ...}, l) =
		foldr walkElem l content
	    | walkSect (MLDocContent.INCLFILE _, l) = l
	    | walkSect (MLDocContent.INTERFACE{module, ...}, l) =
		(parseModule module) :: l
	  and walkElem (MLDocContent.DOCSECT s, l) = walkSect (s, l)
	    | walkElem (_, l) = l
	  in
	    List.foldr walkSect [] docBody
	  end

  end;
