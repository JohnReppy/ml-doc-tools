(* config-parse.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Parser for configuration files.
 *
 * NOTE: we will probably merge in the ConfigScan structure into
 * this one, to aid in error handling.
 *)

structure ConfigParse :> sig

    datatype value
      = NAME of Atom.atom
      | SVAL of string
      | NVAL of int
      | BVAL of bool
      | LIST of value list
      | OBJ of (Atom.atom * value) list

    val parse : {
	  name : string,
	  errMsg : string -> unit
        } -> (Atom.atom * value) list

  end = struct

    structure S = ConfigScan

    datatype value
      = NAME of Atom.atom
      | SVAL of string
      | NVAL of int
      | BVAL of bool
      | LIST of value list
      | OBJ of (Atom.atom * value) list

    exception ERR of (string * S.instream)

    fun parseList parseElem strm = let
	  fun parse (l, strm) = (case parseElem strm
		 of NONE => (rev l, strm)
		  | (SOME(elem, strm')) => parse (elem::l, strm')
		(* end case *))
	  in
	    parse ([], strm)
	  end

    fun parseValue strm = (case S.nextTok strm
	   of NONE => NONE
	    | SOME(S.NAME n, strm') => SOME(NAME n, strm')
	    | SOME(S.NUM n, strm') => SOME(NVAL n, strm')
	    | SOME(S.STR s, strm') => SOME(SVAL s, strm')
	    | SOME(S.BOOL b, strm') => SOME(BVAL b, strm')
	    | SOME(S.LB, strm') => let
		val (vs, strm'') = parseList parseValue strm'
		in
		  case S.nextTok strm''
		   of SOME(S.RB, strm''') => SOME(LIST vs, strm''')
		    | _ => raise ERR("excepted ']' token", strm'')
		  (* end case *)
		end
	    | SOME(S.LCB, strm') => let
		val (nvs, strm'') = parseList parseNameValue strm'
		in
		  case S.nextTok strm''
		   of SOME(S.RCB, strm''') => SOME(OBJ nvs, strm''')
		    | _ => raise ERR("excepted ']' token", strm'')
		  (* end case *)
		end
	    | SOME(tok, _) =>
		raise ERR(concat["unexpected ", S.toString tok, " token"], strm)
	  (* end case *))

    and parseNameValue strm = (case S.nextTok strm
	   of SOME(S.NAME n, strm') => (case parseValue strm'
		 of SOME(v, strm'') => SOME((n, v), strm'')
		  | NONE => raise ERR("expected value", strm')
		(* end case *))
	    | _ => NONE
	  (* end case *))

    fun parse arg = let
	  val strm = S.mkStream arg
	  in
	    #1(parseList parseNameValue strm)
	  end
	    handle ERR(msg, strm) => let
	      val (name, lnum) = S.pos strm
	      in
		(#errMsg arg)(concat[
		    "Error [", name, ":", Int.toString lnum, "]: ",
		    msg, "\n"
	  	  ]);
		raise Fail "parsing error"
	      end

  end
