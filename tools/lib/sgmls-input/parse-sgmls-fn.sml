(* parse-sgmls-fn.sml
 *
 * COPYRIGHT (c) 2000 Bell Laboratories, Lucent Technologies.
 *
 * A simple parser for the SGMLS tool output.
 *)

signature PARSE_SGMLS =
  sig

    structure Markup : MARKUP

    val parse : TextIO.instream -> Markup.markup list

  end;

functor ParseSGMLS (M : MARKUP) : PARSE_SGMLS =
  struct

    structure Markup = M
    structure E = Markup.Elem
    structure Scan = ScanSGMLS
    structure SS = Substring

    fun prefix (ss, n) = SS.slice(ss, 0, SOME n)
    fun suffix (ss, n) = SS.slice(ss, n, NONE)

  (* replace "{ws}*\\n{ws}*" with a single space *)
    fun compressWS ss = let
	  val space = SS.full " "
	  val empty = SS.full ""
	  val trimLeftWS = SS.dropl Char.isSpace
	  val trimRightWS = SS.dropr Char.isSpace
	  fun splitAtSpecialWS ss = let
		fun find (n, rest) = (case SS.getc rest
		       of NONE => (ss, empty)
			| SOME(#"\\", rest') => (case SS.getc rest'
			     of NONE => raise Fail "bad escape"
			      | SOME(#"n", rest'') =>
				  (prefix(ss, n), rest'')
			      | SOME(#"0", rest'') =>
				  if ((SS.size rest'' >= 2)
				  andalso (SS.sub(rest'', 0) = #"1")
				  andalso (SS.sub(rest'', 1) = #"1"))
				    then (prefix(ss, n), suffix(rest'', 2))
				    else find(n+2, rest'')
			      | SOME(_, rest'') =>
				  find(n+2, rest'')
			    (* end case *))
			| SOME(_, rest') => find (n+1, rest')
		      (* end case *))
		in
		  find (0, ss)
		end
	  fun trans (ss, frags) = let
		val (ss', rest) = splitAtSpecialWS ss
		in
		  if (SS.isEmpty rest)
		    then if (SS.size ss = SS.size ss')
		      then rev(ss' :: frags)
		      else rev(space :: ss' :: frags)
		    else let
		      val rest = trimLeftWS rest
		      val ss' = trimRightWS ss'
		      in
			trans (rest, space :: ss' :: frags)
		      end
		end
	  in
	    SS.concat(trans(ss, []))
	  end

  (* map "\\n" to "\n" and "\11" to "\t" *)
    fun cvtNewlines ss = let
	  val newline = SS.full "\n"
	  val tab = SS.full "\t"
	  val empty = SS.full ""
	  fun splitAtNewline ss = let
		fun find (n, rest) = (case SS.getc rest
		       of NONE => NONE
			| SOME(#"\\", rest') => (case SS.getc rest'
			     of NONE => raise Fail "bad escape"
			      | SOME(#"n", rest'') =>
				  SOME(prefix(ss, n), newline, rest'')
			      | SOME(#"0", rest'') =>
				  if ((SS.size rest'' >= 2)
				  andalso (SS.sub(rest'', 0) = #"1")
				  andalso (SS.sub(rest'', 1) = #"1"))
				    then SOME(prefix(ss, n), tab, suffix(rest'', 2))
				    else find(n+2, rest'')
			      | SOME(_, rest'') =>
				  find(n+2, rest'')
			    (* end case *))
			| SOME(_, rest') => find (n+1, rest')
		      (* end case *))
		in
		  find (0, ss)
		end
	  fun trans (ss, frags) = (case splitAtNewline ss
		 of NONE => rev(ss :: frags)
		  | SOME(pre, c, rest) => if (SS.size rest = 0)
		      then rev(c :: pre :: frags)
		      else trans (rest, c :: pre :: frags)
		(* end case *))
	  in
	    SS.concat(trans(ss, []))
	  end


    fun parse instream = let
	(* line/file info *)
	  val line = ref 0 and file = ref ""
	  fun setLineInfo (ln, NONE) = (line := ln)
	    | setLineInfo (ln, SOME f) = (line := ln; file := f)
	(* the token stream *)
	  fun scan strm = (case (Scan.scanStrm strm)
		 of (Scan.LINE info, strm) => (setLineInfo info; scan strm)
		  | stuff => stuff
		(* end case *))
	  fun peek strm = #1(scan strm)
	  fun advance strm = #2(scan strm)
	(* the entity map *)
	  val entityMap = AtomTable.mkTable (16, Fail "EntityMap")
	  val insEntity = AtomTable.insert entityMap
	(* The element construction operation *)
	  val mkElement = E.mkElement {
		  entityMap = AtomTable.find entityMap,
		  publicIds = fn _ => false,
		  systemIds = fn _ => false
		}
	(* the parsing routines *)
	  fun parseAttrs (attrs, strm) = (case (scan strm)
		 of (Scan.INTERN(name, kind, value), strm) => (
		    (* add the entity to the entity map *)
		      insEntity (name, E.InternEntity(kind, value));
		      parseAttrs (attrs, strm))
		  | (Scan.ATTR(name, aval), strm) =>
		      parseAttrs ((name, aval)::attrs, strm)
		  | (Scan.GI_START name, strm) =>
		      parseElement (rev attrs, name, strm)
		  | (ln, _) => (
		      TextIO.output(TextIO.stdErr, concat[
			  "Internal error in ParseSGMLS.parseAttrs; scan line is:\n",
			  Scan.lineToString ln, "\n"
			]);
		      raise Fail "parseAttrs")
		(* end case *))
	  and parseElement (attrs, name, strm) = let
		val startLn = !line
		val file = !file
		val elem =  mkElement(name, attrs)
		val (elems, strm) = parseBody (E.preserveWS elem, [], strm)
		val strm = (case (peek strm)
		       of (Scan.GI_END name') =>
			    if (Atom.sameAtom(name, name'))
			      then advance strm
			      else raise Fail "parseElement: mismatched start/stop"
			| ln => raise Fail("parseElement: unexpected " ^ Scan.lineToString ln)
		      (* end case *))
		in
		  ( M.ELEM{elem = elem, body=elems, pos=(file, startLn, !line)},
		    strm)
		end
	  and parseBody (preserveWS, elems, strm) = (case (peek strm)
		 of (Scan.GI_START name) => let
		      val (elem, strm) = parseElement ([], name, advance strm)
		      in
			parseBody(preserveWS, elem :: elems, strm)
		      end
		  | (Scan.INTERN _) => let
		      val (elem, strm) = parseAttrs ([], strm)
		      in
			parseBody(preserveWS, elem :: elems, strm)
		      end
		  | (Scan.ATTR _) => let
		      val (elem, strm) = parseAttrs ([], strm)
		      in
			parseBody(preserveWS, elem :: elems, strm)
		      end
		  | (Scan.DATA data) => let
		      val data =
			    if preserveWS then cvtNewlines data else compressWS data
		      in
			parseBody(preserveWS, (M.DATA data)::elems, advance strm)
		      end
		  | _ => (rev elems, strm)
		(* end case *))
	  val (elems, strm) = parseBody (false, [], Scan.mkStream instream)
	  in
	    case (peek strm)
	     of Scan.CORRECT => elems
	      | _ => raise Fail "parser"
	    (* end case *)
	  end

  end;

