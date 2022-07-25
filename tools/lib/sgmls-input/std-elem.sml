(* std-elem.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A default representation for the SGMLS elements.
 *)

structure StdElem : sig

    type attr_val = ScanSGMLS.attr_val

    datatype entity
      = ExternIntity of (ScanSGMLS.entity_type * Atom.atom)
      | InternEntity of (ScanSGMLS.entity_type * string)

    type env = {
	entityMap : Atom.atom -> entity option,
	publicIds : Atom.atom -> bool,
	systemIds : Atom.atom -> bool
      }

    type element = {
	name : Atom.atom,
	attrs : (Atom.atom * attr_val) list
      }

    val mkElement : env -> (Atom.atom * (Atom.atom * attr_val) list) -> element
    val elemName : element -> string
    val preserveWS : element -> bool

  end = struct

    type attr_val = ScanSGMLS.attr_val

    datatype entity
      = ExternIntity of (ScanSGMLS.entity_type * Atom.atom)
      | InternEntity of (ScanSGMLS.entity_type * string)

    type env = {
	entityMap : Atom.atom -> entity option,
	publicIds : Atom.atom -> bool,
	systemIds : Atom.atom -> bool
      }

    type element = {
	name : Atom.atom,
	attrs : (Atom.atom * attr_val) list
      }

    fun mkElement env (elemName, attrList) = {name=elemName, attrs=attrList}

(** NOTE: we should also format the attribute list in some way **)
    fun elemName {name, attrs} = Atom.toString name

    fun preserveWS _ = true

  end;
