(* sgmls.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure SGMLS =
  struct

  (* the types of external and internal entities; (NDATA for external only) *)
    datatype entity_type = CDATA | NDATA | SDATA

    datatype attr_val
      = IMPLIED_VAL
      | CDATA_VAL of string
      | NOTATION_VAL of Atom.atom
      | ENTITY_VAL of Atom.atom list
      | TOKEN_VAL of Atom.atom list
      | ID_VAL of Atom.atom		(* only if -oid option is specified *)

  end

