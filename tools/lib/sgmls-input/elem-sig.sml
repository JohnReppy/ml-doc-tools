(* elem-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature ELEM =
  sig

    type attr_val = ScanSGMLS.attr_val
    type element

    datatype entity
      = ExternIntity of (ScanSGMLS.entity_type * Atom.atom)
      | InternEntity of (ScanSGMLS.entity_type * string)

    type env = {
	entityMap : Atom.atom -> entity option,
	publicIds : Atom.atom -> bool,
	systemIds : Atom.atom -> bool
      }

    val mkElement : env -> (Atom.atom * (Atom.atom * attr_val) list) -> element
    val elemName  : element -> string
    val preserveWS : element -> bool

  end;

