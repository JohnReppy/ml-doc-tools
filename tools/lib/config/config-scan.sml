(* config-scan.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Scanner for configuration files.
 *)

structure ConfigScan :> sig

    datatype token
      = NAME of Atom.atom
      | NUM of int
      | STR of string
      | BOOL of bool
      | LB | RB			(* "[" "]" *)
      | LCB | RCB		(* "{" "}" *)

    val toString : token -> string

    type instream

    val mkStream : {
	  name : string,
	  errMsg : string -> unit
        } -> instream
    val nextTok : instream -> (token * instream) option

    val pos : instream -> (string * int)
	(* returns file/line-num of instream *)

  end = struct

    structure SIO = TextIO.StreamIO

    datatype token
      = NAME of Atom.atom
      | NUM of int
      | STR of string
      | BOOL of bool
      | LB | RB			(* "[" "]" *)
      | LCB | RCB		(* "{" "}" *)

    fun toString (NAME a) = concat["'", Atom.toString a, "'"]
      | toString (NUM n) = concat["'", Int.toString n, "'"]
      | toString (STR s) = concat["\"", String.toString s, "\""]
      | toString (BOOL b) = concat["'", Bool.toString b, "\""]
      | toString LB = "'['"
      | toString RB = "']'"
      | toString LCB = "'{'"
      | toString RCB = "'}'"

    datatype info = INFO of {
	name : string,
	errMsg : string -> unit,
	parent : instream option
      }

    and instream = IS of {
	info : info,
	lnum : int,
	strm : SIO.instream
      }

    exception ERR of (string * instream)

    fun popStrm (IS{info = INFO{parent, ...}, strm, ...}) = (
	  SIO.closeIn strm; parent)
    fun pushStrm (name, parent as IS{info=INFO{errMsg, ...}, ...}) = IS{
	    info = INFO{name=name, errMsg=errMsg, parent=SOME parent},
	    lnum = 1,
	    strm = TextIO.getInstream(TextIO.openIn name)
	  }
	    handle _ => raise ERR("bad include file", parent)

    fun getc (IS{info, lnum, strm}) = (case SIO.input1 strm
	   of NONE => NONE
	    | SOME(#"\n", strm) =>
		SOME(#"\n", IS{info=info, lnum=lnum+1, strm=strm})
	    | SOME(c, strm) =>
		SOME(c, IS{info=info, lnum=lnum, strm=strm})
	  (* end case *))

    fun skipWS strm = (case getc strm
	   of NONE => strm
	    | (SOME(c, strm')) => if Char.isSpace c then skipWS strm' else strm
	  (* end case *))

    fun skipToEOL strm = (case getc strm
	   of NONE => strm
	    | (SOME(#"\n", strm')) => strm'
	    | (SOME(c, strm')) => skipToEOL strm'
	  (* end case *))

    fun isNameChr #"-" = true
      | isNameChr #"_" = true
      | isNameChr c = (Char.isAlpha c orelse Char.isDigit c)

    fun pos (IS{info=INFO{name, ...}, lnum, ...}) = (name, lnum)

    fun errorMsg (msg, IS{info=INFO{name, errMsg, ...}, lnum, ...}) =
	  errMsg(concat[
	      "Error [", name, ":", Int.toString lnum, "]: ", msg, "\n"
	    ])

  (* scan a name, assuming that we already know that the first character
   * is a letter.
   *)
    fun scanName strm = let
	  fun scan (l, strm) = (case getc strm
		 of NONE => mkName(l, strm)
		  | SOME(c, strm') => if isNameChr c
		      then scan (c::l, strm')
		      else mkName(l, strm')
		(* end case *))
	  and mkName (l, strm) =
		(Atom.atom(String.implode(rev l)), strm)
	  in
	    scan ([], strm)
	  end

  (* scan a number *)
    fun scanNum strm = (case Int.scan StringCvt.DEC getc strm
	   of NONE => raise ERR("bad number", strm)
	    | SOME(n, strm') => SOME(NUM n, strm')
	  (* end case *))
	    handle Overflow => raise ERR("number too large", strm)

    val scanMLChr = Char.scan getc

    fun scanString strm0 = let
	  fun scan (l, strm) = (case scanMLChr strm
		 of NONE => (case getc strm
		       of SOME(#"\"", strm') => mkString(l, strm')
			| _ => raise ERR("bad string literal", strm0)
		      (* end case *))
		  | SOME(c, strm') => scan(c::l, strm')
		(* end case *))
	  and mkString (l, strm) = SOME(STR(implode(rev l)), strm)
	  in
	    scan ([], strm0)
	  end

    val kwInclude = Atom.atom "include"
    val kwTRUE = Atom.atom "TRUE"
    val kwTrue = Atom.atom "true"
    val kwFALSE = Atom.atom "FALSE"
    val kwFalse = Atom.atom "false"

    fun mkStream {name, errMsg} = IS{
	    info = INFO{name = name, errMsg = errMsg, parent = NONE},
	    lnum = 1,
	    strm = TextIO.getInstream(TextIO.openIn name)
	  }

    fun nextTok strm = let
	  val strm = skipWS strm
	  in
	    case getc strm
	     of NONE => (case popStrm strm
		 of NONE => NONE
		  | (SOME strm) => nextTok strm
		(* end case *))
	      | SOME(#"#", strm') => nextTok(skipToEOL strm')
	      | SOME(#"\"", strm') => scanString strm'
	      | SOME(#"{", strm') => SOME(LCB, strm')
	      | SOME(#"}", strm') => SOME(RCB, strm')
	      | SOME(#"[", strm') => SOME(LB, strm')
	      | SOME(#"]", strm') => SOME(RB, strm')
	      | SOME(#"-", _) => scanNum strm
	      | SOME(c, _) =>
		  if (Char.isAlpha c)
		    then let
		      val (name, strm'') = scanName strm
		      in
			if Atom.sameAtom(name, kwInclude)
			  then (case nextTok strm''
			     of SOME(STR fname, strm''') =>
				  nextTok(pushStrm(fname, strm'''))
			      | _ => raise ERR("missing include filename", strm'')
			    (* end case *))
			else if Atom.sameAtom(name, kwTRUE)
			orelse Atom.sameAtom(name, kwTrue)
			  then SOME(BOOL true, strm'')
			else if Atom.sameAtom(name, kwFALSE)
			orelse Atom.sameAtom(name, kwFalse)
			  then SOME(BOOL false, strm'')
			  else SOME(NAME name, strm'')
		      end
		  else if (Char.isDigit c)
		    then scanNum strm
		    else raise ERR(concat[
			"invalid character #\"", Char.toString c, "\""
		      ], strm)
	    (* end case *)
	  end
	    handle ERR(msg, strm) => (
	      errorMsg(msg, strm);
	    (* simple error recovery --- skip to the next line and try again *)
	      nextTok(skipToEOL strm))

  end;
