(* index-parse-tree.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the representation of an INDEX file, as produced by the parser.
 *)

structure IndexParseTree =
  struct

    datatype file_item
      = SECTION of {
	    title : string,
	    label : Atom.atom option,
	    content : file_item list
	  }
      | INTERFACE of {title : string}
      | INCLFILE of Atom.atom
      | FIGURE of Atom.atom
      | TABLE of Atom.atom

    datatype file_nd = FILE of {
	name : Atom.atom,
	isEmpty : bool,
	title : string,
	content : file_item list
      }

    datatype anchor_nd = ANCHOR of {
	tag : Atom.atom,
	file : Atom.atom
      }

    datatype label_nd = LABEL of {
	name : Atom.atom,
	kind : IndexRep.label_kind,
	file : Atom.atom
      }

    datatype ty_nd
      = DTY of {
	  id : Atom.atom,
	  cons : Atom.atom list
	}
      | DTYDEF of {id : Atom.atom}
      | TY of {
	  id : Atom.atom,
	  eq : bool
	}

    datatype env_nd = ENV of {
	  incl : Atom.atom list,
	  strs : str_nd list,
	  tys : ty_nd list,
	  exns : Atom.atom list,
	  vals : Atom.atom list
	}

    and interface_nd
      = SIGREF of Atom.atom
      | SIGENV of env_nd
      | SIGEXTERN		(* defined in some other document *)

    and str_nd = STR of {
	  id : Atom.atom,
	  bodySig : interface_nd
	}

    datatype sig_nd
      = SIGNATURE of {
	  id : Atom.atom,
	  file : Atom.atom,
	  env : env_nd
	}
      | UNDEF_SIG of Atom.atom

    datatype module_nd
      = STRUCTURE of {
	  file : Atom.atom,
	  str : str_nd
	}
      | FUNCTOR of {
	  id : Atom.atom,
	  file : Atom.atom,
	  argSig : interface_nd,
	  bodySig : interface_nd
	}

    datatype index_tree = INDEX of {
	files : file_nd list,
	anchors : anchor_nd list,
	labels : label_nd list,
	sigs : sig_nd list,
	modules : module_nd list
      }

  end;
