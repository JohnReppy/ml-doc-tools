(* document.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Support for getting information about external documents.
 *)

structure Document : sig

    type doc_cache

    type doc_info = {
	info : MLDocIndex.index,
	baseURL : string,
	rootURL : string
      }

    val mkCache : MLDocConfig.configuration -> doc_cache

    val getDoc : doc_cache * string -> doc_info option

    val docIndex : doc_cache * string -> MLDocIndex.index option
    val docBaseURL : doc_cache * string -> string option
    val docRootURL : doc_cache * string -> string option

  end = struct

    structure I = MLDocIndex
    structure ATbl = AtomTable

    type doc_info = {
	info : I.index,
	baseURL : string,
	rootURL : string
      }

    datatype doc_cache = C of {
	config : MLDocConfig.configuration,
	docCache : doc_info ATbl.hash_table
      }

    fun mkCache cf = C{
	    config = cf,
	    docCache = ATbl.mkTable(4, Fail "document-cache")
	  }

    fun getDoc (C{docCache, config}, docName) = let
	  val name = Atom.atom docName
	  in
	    case (AtomTable.find docCache name)
	     of NONE => (case MLDocConfig.getDB'(config, [name])
		   of NONE => (
			Error.warning(
			  "external document \"%s\" is not specified \
			  \in configuration",
			[Format.STR docName]);
			NONE)
		    | (SOME db) => let
			fun getAttr name = (case MLDocConfig.getStr(db, [name])
			       of NONE => Error.error'[
				      "no \"", name, "\" attribute for document \"",
				      docName, "\""
				    ]
				| (SOME s) => s
			      (* end case *))
			val infoFile = getAttr "InfoFile"
			val baseURL = getAttr "BaseURL"
			val rootURL = getAttr "RootURL"
			val info = I.parse infoFile
			val item = {info=info, baseURL=baseURL, rootURL=rootURL}
			in
			  AtomTable.insert docCache (name, item);
			  SOME item
			end
			  handle Option => (
			    Error.warning(
			      "incomplete specification of external document \"%s\"",
			      [Format.STR docName]);
			    NONE)
		  (* end case *))
	      | someItem => someItem
	    (* end case *)
	  end

  (* returns the index of an external document (if it can be found).
   * Once the index has been loaded, it is cached.
   *)
    fun docIndex arg = Option.map #info (getDoc arg)

    fun docBaseURL arg = Option.map #baseURL (getDoc arg)

    fun docRootURL arg = Option.map #rootURL (getDoc arg)

  end
