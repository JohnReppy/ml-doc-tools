(* ppspec-sig.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

signature PP_SPEC =
  sig

    structure PPStrm : PP_STREAM
    structure Style : ML_STYLE

    type device = PPStrm.device
    type stream = PPStrm.stream
    type token = PPStrm.token
    type context = Style.context

    type ml_type = MLSpec.ml_type

    val ppSignature : (stream * string) -> unit
    val ppStructure : (context * stream)
	  -> (string * bool * string option * MLSpec.where_ty list)
	    -> unit
    val ppFunctor : (context * stream)
	  -> (string * string option * bool * string option * MLSpec.where_ty list)
	    -> unit

    val ppSpecs : (context * stream * bool)
	  -> (MLSpec.spec * MLSpec.M.markup list) list -> unit
    val ppSpec : (context * stream * bool) -> MLSpec.spec -> unit
    val ppInclSpec : (context * stream * bool)
	  -> (MLSpec.tagged_id * MLSpec.where_ty list) list -> unit
    val ppStrSpec : (context * stream * bool)
	  -> (string * MLSpec.tagged_id * MLSpec.where_ty list) -> unit
    val ppStrSigSpec : (context * stream)
	  -> (string * (MLSpec.spec * MLSpec.M.markup list) list)
	    -> unit
    val ppWhereTys : (context * stream) -> MLSpec.where_ty list -> unit
    val ppSharingSpec : (context * stream) -> MLSpec.sharing_spec list -> unit
    val ppTySpec : (context * stream) -> {
	    eq : bool,
	    params : string option,
	    id : string,
	    def : ml_type option
	  } list -> unit
    val ppDTSpec : (context * stream) -> {
	    compact : bool, params : string option,
	    id : string,
	    cons : (string * ml_type option * MLSpec.M.markup list) list
	  } list -> unit
    val ppDTLHS : (context * stream) -> (string option * string) -> unit
    val ppConsSpec : (context * stream) -> {
	    isFirst : bool,
	    id : string,
	    ty : ml_type option
	  } -> unit
    val ppDTDefSpec : (context * stream) -> {
	    id : string,
	    def : MLSpec.tagged_id
	  } list -> unit
    val ppExnSpec : (context * stream)
	  -> (string * MLSpec.ml_type option) list -> unit
    val ppValSpec : (context * stream * bool)
	  -> (string * ml_type * MLDocMarkup.markup list) list -> unit

    val ppType : (context * stream) -> ml_type -> unit

 (* some utility print routines *)
    val ppTyParams : (stream * string option) -> unit
    val ppKW : (stream * string) -> unit
    val ppPunct : (stream * string) -> unit
    val ppPageBreak : (stream * bool) -> unit

  end
