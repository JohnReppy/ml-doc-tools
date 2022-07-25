(* mldoc-content.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Get a high-level view of the content of an ML-Doc file.
 *)

structure MLDocContent : sig

    type header = {
	title : string,
	author : {
	    name : string, email : string,
	    year : int, month : int option, day : int option
	  } option,
	version : {
	    verid : string option,
	    year : int, month : int option, day : int option
	   } option,
	copyrights : {owner : string, year : int} list
      }

    datatype sect_style
      = NUMBER_SEC		(* normal section with section number *)
      | NONUMBER_SEC		(* section without number *)
      | NOTOC_SEC		(* section without number and no toc entry *)

    datatype doc_sect
      = SECTION of {
	    title : MLDocMarkup.markup list,
	    style : sect_style,
	    label : string option,
	    id : int,
	    content : element list
	  }
      | INCLFILE of string
      | INTERFACE of {
	    title : MLDocMarkup.markup list,
	    label : string option,
	    id : int,
	    seealso : MLDocMarkup.markup list,
	    pre : element list,			(* but not DOCSECT *)
	    module : MLDocMarkup.markup,	(* STRUCTURE | FUNCTOR | SIGNATURE *)
	    post : element list			(* but not DOCSECT *)
	  }

    and element
      = DOCSECT of doc_sect
      | FLOAT of MLDocFloat.float
      | PARA of MLDocMarkup.markup

    type doc_body = doc_sect list

    val getHeader : MLDocMarkup.markup list -> {
	    hdr : header,
	    rest : MLDocMarkup.markup list
	  }

    val getCopyrights : MLDocMarkup.markup list -> {
	    copyrights : {owner : string, year : int} list,
	    rest : MLDocMarkup.markup list
	  }

    val getContents : MLDocMarkup.markup list -> {
	    hdr : header,
	    body : doc_body
	  }

    val emptyBody : doc_body -> bool
	(* return true if the document body contains no text (i.e., it just
	 * contains structural elements).
	 *)

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup

    type header = {
	title : string,
	author : {
	    name : string, email : string,
	    year : int, month : int option, day : int option
	  } option,
	version : {
	    verid : string option,
	    year : int, month : int option, day : int option
	   } option,
	copyrights : {owner : string, year : int} list
      }

    datatype sect_style
      = NUMBER_SEC		(* normal section with section number *)
      | NONUMBER_SEC		(* section without number *)
      | NOTOC_SEC		(* section without number and no toc entry *)

    datatype doc_sect
      = SECTION of {
	    title : MLDocMarkup.markup list,
	    style : sect_style,
	    label : string option,
	    id : int,
	    content : element list
	  }
      | INCLFILE of string
      | INTERFACE of {
	    title : MLDocMarkup.markup list,
	    label : string option,
	    id : int,
	    seealso : MLDocMarkup.markup list,
	    pre : element list,			(* but not DOCSECT *)
	    module : MLDocMarkup.markup,
	    post : element list			(* but not DOCSECT *)
	  }

    and element
      = DOCSECT of doc_sect
      | FLOAT of MLDocFloat.float
      | PARA of MLDocMarkup.markup

    type doc_body = doc_sect list

    fun getHeader [M.ELEM{elem=E.ML_DOC, body, ...}] = let
	  val title = ref ""
	  val author = ref NONE
	  val version = ref NONE
	  val copyrights = ref []
	  fun walk (elem::r) = (case elem
		 of (M.ELEM{elem=E.COPYRIGHT cpy, ...}) => (
		      copyrights := cpy :: !copyrights;
		      walk r)
		  | (M.ELEM{elem=E.TITLE, body=[M.DATA s], ...}) => (
		      title := s;
		      walk r)
		  | (M.ELEM{elem=E.AUTHOR info, ...}) => (
		      author := SOME info;
		      walk r)
		  | (M.ELEM{elem=E.VERSION info, ...}) => (
		      version := SOME info;
		      walk r)
		  | _ => elem::r
		(* end case *))
	    | walk [] = []
	  val rest = walk body
	  in
	    { hdr = {
		  title = ! title,
		  author = !author,
		  version = !version,
		  copyrights = rev(!copyrights)
		},
	      rest = rest
	    }
	  end
      | getHeader _ = raise Fail "expected ML-DOC"

    fun getCopyrights elems = let
	  fun find (cpys, elems) = (case elems
		 of (M.ELEM{elem=E.COPYRIGHT cpy, ...} :: r) =>
		      find (cpy::cpys, r)
		  | (M.ELEM{elem=E.TITLE, ...} :: r) => find (cpys, r)
		  | (M.ELEM{elem=E.AUTHOR _, ...} :: r) => find (cpys, r)
		  | (M.ELEM{elem=E.VERSION _, ...} :: r) => find (cpys, r)
		  | _ => {copyrights = rev cpys, rest = elems}
		(* end case *))
	  in
	    find ([], elems)
	  end

    fun getContents elems = let
	  val {hdr, rest} = getHeader elems
	  val nextId = let
		val n = ref 0
		in
		  fn () => (!n before n := !n+1)
		end
	  fun getHEAD (M.ELEM{elem=E.HEAD, body, ...} :: r) = (body, r)
	    | getHEAD _ = Error.bogusMarkupList("HEAD", rest)
	  fun mkSect walkFn (label, nonumber, notoc, body) = let
		val (title, rest) = getHEAD body
		in
		  SECTION{
		      title = title, label = label,
		      style = if nonumber
			then if notoc then NOTOC_SEC else NONUMBER_SEC
			else NUMBER_SEC,
		      id = nextId(), content = walkFn rest
		    }
		end
	  fun mkInterface (label, body) = let
		val (title, rest) = getHEAD body
		val (seealso, rest) = (case rest
		       of (M.ELEM{elem=E.SEEALSO, body, ...} :: r) => (body, r)
			| _ => ([], rest)
		      (* end case *))
		fun walk (elem :: r, l) = (case elem
		       of (M.ELEM{elem=E.PP, ...}) => walk (r, PARA elem :: l)
			| (M.ELEM{elem=E.FLOAT{label, ...}, body, ...}) =>
			    walk (r, FLOAT(MLDocFloat.getFloatContents elem) :: l)
			| _ => (rev l, elem::r)
		      (* end case *))
		  | walk ([], l) = (rev l, [])
		val (pre, module::rest) = walk (rest, [])
		val (post, _) = walk (rest, [])
		in
		  INTERFACE{
		      title = title,
		      label = label,
		      id = nextId(),
		      seealso = seealso,
		      pre = pre,
		      module = module,
		      post = post
		    }
		end
	  fun walkContents ml = let
		fun walk ([], l) = List.rev l
		  | walk ((m as M.ELEM{elem, body, ...}) :: r, l) = let
		      val elem = (case elem
			     of (E.SECTION{label, nonumber, notoc}) =>
				  DOCSECT(mkSect walkContents
				    (label, nonumber, notoc, body))
			      | (E.INCLFILE{file}) => DOCSECT(INCLFILE file)
			      | (E.INTERFACE{label}) =>
				  DOCSECT(mkInterface (label, body))
			      | (E.FLOAT{...}) =>
				  FLOAT(MLDocFloat.getFloatContents m)
			      | _ => PARA m
			    (* end case *))
		      in
			walk (r, elem::l)
		      end
		in
		  walk (ml, [])
		end
	  fun walkBody [] = []
	    | walkBody (
		M.ELEM{elem=E.SECTION{label, nonumber, notoc}, body, ...} :: r
	      ) =
		(mkSect walkContents (label, nonumber, notoc, body)) :: walkBody r
	    | walkBody (M.ELEM{elem=E.INCLFILE{file}, ...} :: r) =
		(INCLFILE file) :: walkBody r
	    | walkBody (M.ELEM{elem=E.INTERFACE{label}, body, ...} :: r) =
		(mkInterface (label, body)) :: walkBody r
	    | walkBody ml = Error.bogusMarkupList ("document body", ml)
	  in
	    {hdr = hdr, body = walkBody rest}
	  end

  (* return true if the document body contains no text (i.e., it just
   * contains structural elements).
   *)
    fun emptyBody docBody = let
	  fun isEmptySect (SECTION{content, ...}) = isEmpty content
	    | isEmptySect (INCLFILE _) = true
	    | isEmptySect (INTERFACE _) = false
	  and isEmpty [] = true
	    | isEmpty (DOCSECT s :: r) = isEmptySect s andalso isEmpty r
	    | isEmpty (_ :: _) = false	(* FLOAT | PARA *)
	  in
	    List.all isEmptySect docBody
	  end

  end;
