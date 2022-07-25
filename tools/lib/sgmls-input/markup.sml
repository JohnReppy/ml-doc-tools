(* markup.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A simple tree representation of document markup
 *)

functor MarkupFn (E : ELEM) : MARKUP =
  struct

    structure Elem = E

    structure SS = Substring
    structure F = Format

    datatype markup
      = ELEM of {
	  elem : Elem.element,
	  body : markup list,
	  pos : (string * int * int)
	}
      | DATA of string

    datatype iter = ITER of markup list list

    fun iterate m = ITER[[m]]
    fun iterateList ml = ITER[ml]

    fun next (ITER[]) = NONE
      | next (ITER([]::r)) = next(ITER r)
      | next (ITER((m::r)::r')) = (case m
	   of (DATA _) => SOME(m, ITER([]::r::r'))
	    | (ELEM{body, ...}) => SOME(m, ITER(body::r::r'))
	  (* end case *))

    fun next' (ITER[]) = raise Fail "Markup.next': unexpected end"
      | next' (ITER([]::r)) = next'(ITER r)
      | next' (ITER((m::r)::r')) = (case m
	   of (DATA _) => (m, ITER([]::r::r'))
	    | (ELEM{body, ...}) => (m, ITER(body::r::r'))
	  (* end case *))
(****
val next = fn iter => let
	val nxt = next iter
	in
	  case nxt
	   of NONE => IO.output(IO.std_out, "next: NONE\n")
	    | (SOME(DATA _, _)) => IO.output(IO.std_out, "next: SOME <data>\n")
	    | (SOME(ELEM{elem, ...}, _)) =>
		IO.output(IO.std_out,
		  Format.format "next: SOME(%s)\n" [Format.STR(Elem.elemName elem)])
	  (* end case *);
	  nxt
	end
****)

    fun right (ITER[]) = ITER[]
      | right (ITER(_::r)) = ITER r

    fun cut (ITER[]) = ITER[]
      | cut (ITER(ml::_)) = ITER[ml]

(*
    datatype iter = ITER of (unit -> (markup * iter) option)

    local
      fun iter (m as ELEM{body, ...}, k) = SOME(m, ITER(fn () => iterList(body, k)))
	| iter (m as DATA _, k) = SOME(m, ITER k)
      and iterList ([], k) = k()
	| iterList (m::r, k) = iter (m, fn () => iterList(r, k))
    in
    fun iterate m = ITER(fn () => iter(m, fn () => NONE))
    fun iterateList ml = ITER(fn () => iterList(ml, fn () => NONE))
    end

    fun next (ITER it) = it()
*)

  (* given a translation function on escape sequences and one for
   * sdata, translate the given DATA string.
   *)
    fun transData {escape, sdata, special} src = let
	  fun isNormal #"\\" = false
	    | isNormal c = (case (special c) of NONE => true | (SOME _) => false)
	  fun scan (strs, rest) = let
		val (ss, rest') = SS.splitl isNormal rest
		val strs = if (SS.isEmpty ss) then strs else ss::strs
		in
		  if (SS.isEmpty rest')
		    then SS.concat(rev strs)
		  else if (SS.sub(rest', 0) = #"\\")
		    then let
		      val (transStr, rest'') = (case SS.sub(rest', 1)
			   of #"|" => let
			        fun scanSData i = (
				      case (SS.sub(rest', i), SS.sub(rest', i+1))
				       of (#"\\", #"|") => let
					    val d = SS.slice(rest', 2, SOME(i-2))
					    val rest'' = SS.triml (i+2) rest'
					    in
					      (sdata d, rest'')
					    end
				        | (_, #"\\") => scanSData(i+1)
				        | _ => scanSData(i+2)
				      (* end case *))
			        in
				  scanSData 2
			        end
			    | #"\\" => (escape #"\\", SS.triml 2 rest')
			    | #"n" => (escape #"\n", SS.triml 2 rest')
			    | _ => let (* Octal character code *)
				val scan = Int.scan StringCvt.OCT Substring.getc
				val (oct, rest'') = (SS.splitAt(SS.triml 1 rest', 3))
				in
				  ( String.str(Char.chr(#1(valOf(scan oct)))),
				    rest''
				  )
			        end
			  (* end case *))
		      in
			scan ((SS.full transStr)::strs, rest'')
		      end
		    else let
		      val (SOME s) = special(SS.sub(rest', 0))
		      in
			scan ((SS.full s)::strs, SS.triml 1 rest')
		      end
		end (* scan *)
	  in
	    scan ([], SS.full src)
	  end

    fun stripData transFns s = let
	  val s = transData transFns s
	  in
	    SS.string(SS.dropl Char.isSpace (SS.dropr Char.isSpace (SS.full s)))
	  end

  end;

