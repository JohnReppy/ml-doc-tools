(* walk-markup.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Some utilities for walking a markup tree.
 *)

structure WalkMarkup : sig

    type name
    type attr_val
    type element

    type ('a, 'b) element_fn = {
	ctxt : 'a,
	name : name, 
	attrs : (name * attr_val) list,
	elems : element list,
	err : string -> unit
      } -> 'b

    type ('a, 'b) element_map

    val mkMap : {
	  dataFn : ('_a * string) -> '_b,
	  dfltFn : ('_a, '_b) element_fn,
	  fns : (name * (('_a, '_b) element_map -> ('_a, '_b) element_fn)) list
	} -> ('_a, '_b) element_map

    val apply : (('a, 'b) element_map * 'a) -> element -> 'b

  end = struct

    structure P = Parser
    structure N = Name
    structure F = Format

    type name = P.name
    type attr_val = P.attr_val
    type element = P.element

    type ('a, 'b) element_fn = {
	ctxt : 'a,
	name : name, 
	attrs : (name * attr_val) list,
	elems : element list,
	err : string -> unit
      } -> 'b

    datatype ('a, 'b) element_map = EM of 'a -> element -> 'b

    fun errorMsg (file, l1, l2) msg =
	  if (l1 = l2)
	    then IO.output(IO.std_err, F.format "Error [%s:%d] %s\n" [
		F.STR file, F.INT l1, F.STR msg
	      ])
	    else IO.output(IO.std_err, F.format "Error [%s:%d-%d] %s\n" [
		F.STR file, F.INT l1, F.INT l2, F.STR msg
	      ])

    fun mkMap {dataFn, dfltFn, fns} = let
	  val map = N.mkNameTbl (length fns, Fail "element map")
	  fun look ctxt (P.ELEMENT{name, attrs, elements, pos}) = let
		val f = (case N.peek map name
		       of (SOME f) => f
			| NONE => dfltFn
		      (* end case *))
		in
		  f {
		      name = name, ctxt = ctxt,
		      attrs=attrs, elems=elements,
		      err=errorMsg pos
		    }
		end
	    | look ctxt (P.DATA data) = dataFn (ctxt, data)
	  in
	    app (fn (name, f) => N.insert map (name, f(EM look))) fns;
	    EM look
	  end

    fun apply (EM look, ctxt)  = look ctxt

  end;

