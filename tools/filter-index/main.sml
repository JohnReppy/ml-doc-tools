(* filter-index.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

(* format of entries is in one of two formats:
 *
 * \indexentry {key}{page}
 * \indexentry {key|fmt}{page}
 *
 * We sort by <key, page> order.  Note that page numbers may be
 * Roman numerals, but we are only interested in coalescing items
 * that have the same key and page number, but different formats.
 *
 * The formats are: <none>, "mldSPECIXPP", or "mldDESCIXPP".
 *)

structure Main =
  struct

    datatype fmt = REGULAR | SPEC | DESC | SPECDESC

    fun mergeFmt (REGULAR, fmt) = fmt
      | mergeFmt (fmt, REGULAR) = fmt
      | mergeFmt (SPEC, SPEC) = SPEC
      | mergeFmt (DESC, DESC) = DESC
      | mergeFmt _ = SPECDESC

    datatype entry = E of {
	key : string,
	fmt : fmt,
	page : string
      }

    fun compare (E{key=k1, page=p1, ...}, E{key=k2, page=p2, ...}) = (
	  case String.compare (k1, k2)
	   of EQUAL => String.compare (p1, p2)
	    | order => order
	  (* end case *))

    fun gtr (e1, e2) = (case compare(e1, e2)
	   of GREATER => true
	    | _ => false
	  (* end case *))

    val sort = ListMergeSort.sort gtr

    structure SS = Substring

    fun scanLine str = let
	  fun bogus () = raise Fail("bogus line: "^String.toString str)
	  fun extract (start, n) = SS.string(SS.slice(start, 0, SOME n))
	  fun scanPre ss = (case SS.getc ss
		 of SOME(#"{", ss') => scanKey (ss', ss', 0, 1)
		  | SOME(_, ss') => scanPre ss'
		  | NONE => bogus()
		(* end case *))
	  and scanKey (start, ss, n, nest) = (case SS.getc ss
		 of SOME(#"{", ss') => scanKey(start, ss', n+1, nest+1)
		  | SOME(#"}", ss') => if (nest = 1)
			then scanPage(extract(start, n), REGULAR, ss')
			else scanKey(start, ss', n+1, nest-1)
		  | SOME(#"|", ss') => if (nest = 1)
			then scanFmt(extract(start, n), ss', ss', 0)
			else scanKey(start, ss', n+1, nest-1)
		  | SOME(_, ss') => scanKey(start, ss', n+1, nest)
		  | NONE => bogus()
		(* end case *))
	  and scanFmt (key, start, ss, n) = (case SS.getc ss
		 of SOME(#"}", ss) => (case extract(start, n)
		       of "mldDESCIXPP" => scanPage (key, DESC, ss)
			| "mldSPECIXPP" => scanPage (key, SPEC, ss)
			| _ => bogus()
		      (* end case *))
		  | SOME(_, ss) => scanFmt(key, start, ss, n+1)
		  | NONE => bogus()
		(* end case *))
	  and scanPage (key, fmt, ss) = let
		val start = SS.triml 1 ss (* remove initial "{" *)
		fun scan (ss, n) = (case SS.getc ss
		       of SOME(#"}", _) =>
			    E{key=key, fmt=fmt, page=extract(start, n)}
			| SOME(_, ss) => scan(ss, n+1)
			| NONE => bogus()
		      (* end case *))
		in
		  scan (start, 0)
		end
	  in
	    scanPre (SS.full str)
	  end

    fun pr outS (E{key, fmt, page}) = TextIO.output (outS, concat [
	    "\\indexentry {", key,
	    case fmt
	     of REGULAR => ""
	      | SPEC => "|mldSPECIXPP"
	      | DESC => "|mldDESCIXPP"
	      | SPECDESC => "|mldSPECDESCIXPP"
	    (* end case *),
	    "}{", page, "}", "\n"
	  ])

    fun readFile inS = let
	  fun lp entries = (case TextIO.inputLine inS
		 of NONE => List.rev entries
		  | SOME ln => lp(scanLine ln :: entries)
		(* end case *))
	  in
	    lp[]
	  end

    fun filter (l, e1 as E{key, fmt, page}, (e2 as E{fmt=f2, ...})::r) = (
	  case compare(e1, e2)
	   of EQUAL =>
		filter(l, E{key = key, page=page, fmt=mergeFmt(fmt, f2)}, r)
	    | _ => filter(e1::l, e2, r)
	  (* end case *))
      | filter (l, e, []) = List.rev(e::l)

    fun main _ = let
	  val entries = readFile TextIO.stdIn
	  val entries = sort entries
	  val entries = filter ([], hd entries, tl entries)
	  in
	    List.app (pr TextIO.stdOut) entries;
	    OS.Process.exit OS.Process.success
	  end;

(*
    fun doit (inF, outF) = let
	  val inS = TextIO.openIn inF
	  val entries = readFile inS
	  val _ = TextIO.closeIn inS
	  val entries = sort entries
	  val entries = filter ([], hd entries, tl entries)
	  val outS = TextIO.openOut outF
	  in
	    List.app (pr outS) entries;
	    TextIO.closeOut outS
	  end
*)

  end;
