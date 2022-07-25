(* build-index-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 * This is the interface for building an index.
 *)

signature BUILD_INDEX =
  sig

    structure I : MLDOC_INDEX

    type name = I.name
    type index = I.index
    type env = I.env

    datatype sig_ref
      = AnonSig of env
      | NamedSig of I.Sig.entry
      | ExternSig

    val index : unit -> index
	(* create a new index *)

    val findSig : (index * name) -> I.Sig.entry
	(* find the named signature; if it doesn't exist in the index,
	 * the introduce a dummy entry for it.
	 *)

    val newEnv    : (index * I.File.entry * I.binding_context) -> env
    val topEnv	  : (index * {file : I.File.entry}) -> env
    val copyEnv	  : (index * env) -> env

    val newSig	  : (index * {sigId : name, file : I.File.entry}) -> I.Sig.entry
	  (* create an environment for a signature and insert the signature
	   * into the index.  If there is a dummy entry for the signature,
	   * replace it with the new definition.
	   *)
    val newStr	  : (index * {
	    strId : name, bodySig : sig_ref, file : I.File.entry
	  }) -> unit
    val newSubstr : (env * {strId : name, bodySig : sig_ref}) -> unit
    val newFct	  : (index * {
	    fctId : name, argSig : sig_ref, bodySig : sig_ref, file : I.File.entry
	  }) -> unit

    val includeSig	: (env * name) -> unit
	(* extend the environment by "including" a signature *)


    val insType		: (env * {eq : bool, id : name}) -> unit
    val insDatatype	: (env * {id : name, cons : name list}) -> unit
    val insDatatypeDef	: (env * {id : name}) -> unit
    val insExn		: (env * name) -> unit
    val insVal		: (env * name) -> unit

  (* Support for other global objects *)
    val insFile		: (index * {name : name, isEmpty : bool, title : string})
			    -> I.File.entry
    val setFileContent  : (I.File.entry * I.File.file_content list) -> unit
    val insAnchor	: (index * {tag : name, file : I.File.entry}) -> unit
    val insSecLabel	: (index * {name : name, file : I.File.entry}) -> unit
    val insTblLabel	: (index * {name : name, file : I.File.entry}) -> unit
    val insFigLabel	: (index * {name : name, file : I.File.entry}) -> unit
    val insCodeLabel	: (index * {name : name, file : I.File.entry}) -> unit
    val insIfaceLabel	: (index * {name : name, file : I.File.entry}) -> unit

  (* Merge the first index into the second index *)
    val merge : (index * index) -> unit

  end;

