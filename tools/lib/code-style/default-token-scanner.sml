(* default-token-scanner.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * The default scanner takes care of tab expansion and indent trimming,
 * does not stylize text.
 *)

structure DefaultTokenScanner : TOKEN_SCANNER =
  struct

    structure SS = Substring

    type state = SS.substring

    val initial = SS.full ""

    fun addString (data, s) =
	  if SS.isEmpty data
	    then SS.full s
	    else raise Fail "addString to non-empty state"

    val getc = SS.getc

    fun scan start = let
	  fun tok (n, ss) = SOME(NONE, SS.slice(start, 0, SOME n), ss)
	  fun scan' (n, ss) = (case SS.getc ss
		 of NONE => if n = 0 then NONE else tok(n, ss)
		  | SOME(c, ss') =>
		      if Char.isSpace c
			then tok(n, ss)
			else scan' (n+1, ss')
		(* end case *))
	  in
	    scan' (0, start)
	  end

  end

structure DefaultScanner = CodeScannerFn (DefaultTokenScanner);
