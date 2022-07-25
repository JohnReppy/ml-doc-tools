(* regexp.sml
 *
 * COPYRIGHT (c) 1991 Cornell University
 *
 * AUTHOR: William Aitken
 *         Cornell University
 *         Ithaca, NY 14853
 *)

structure Regexp : REGEXP =
  struct

  (* tokens in the regular expression specification *)
    datatype token
      = XTOKEN of char list 
      | RTOKEN of char list 
      | LITOKEN of char
      | LPTOKEN of int
      | RPTOKEN of int
      | HATOKEN | DOLLARTOKEN | PLUSTOKEN | STARTOKEN 
      | DOTOKEN | QUESTOKEN | PIPETOKEN

  (* a compiled representation of a regular expression *)
    datatype re
      = XRANGE of (char list * re)   (* [^...] *)
      | RANGE of (char list * re)    (* [...] *)
      | LITERAL of (char * re)	    (* a single character *)
      | WILD of re		    (* . *)
      | STAR of re		    (* a*, where a matches a single character *)
      | PLUS of re		    (* a+, where a matches a single character *)
      | BRANCH of (re * re)	    (* a|b *)
      | BEGIN of (int * re)	    (* ( *)
      | END of (int * re)	    (* ) *)
      | EOL of re		    (* $ *)
      | SOL of re		    (* ^ *)
      | LOOP of (re * re)	    (* LOOP(a,b) is (b|a LOOP(a,b)) *)
      | POOL of re		    (* end of loops *)
      | FAIL			    (* never matches *)
      | DONE			    (* end of the re *)

    datatype parens
      = SP of {index : int, cpos : int}
      | EP of {index : int, cpos : int}
    type matchdesc = parens list

    datatype reSyntax = 
        Backslash | RBracket | LBracket | RParen | Star | Plus | Quest
    datatype reError = 
        RERange of int | RESyntax of reSyntax * int | REEmptyLoop | REBug of int | REMatch
    exception REError of reError 

    exception MatchFail

    val charOf = String.sub

    fun scanRegExp s = let
	  fun scanRange n = let
		fun make_sequence (last, b) = let
                      val first = hd b
		      fun doit (c, d) = if c = last then c::d 
                                        else doit(Char.succ c, c::d)
		      in
                        if first = last then b 
                        else doit (Char.succ first, b)
		      end
		fun do_range (n, rv) = (
		      case charOf(s, n)
		       of #"-" => (case charOf(s, n + 1)
			     of #"]" => (n + 2, #"-"::rv)
			      |  c  => 
                                   if c < hd rv then raise REError(RERange n)
                                   else do_range(n + 2, make_sequence(c, rv))
			    (* end case *))
			| #"]" => (n + 1, rv)
			|  c  => do_range(n + 1, c::rv)
		      (* end case *))
		in
		  (case (charOf (s, n))
		   of #"^" => let val (n', r) = do_range(n + 2, [charOf(s, n + 1)])
		     in (n', XTOKEN r)
		     end
		    | c => let val (n', r) = do_range(n + 1, [c])
		     in (n', RTOKEN r)
		     end
		  (* end case *))
		    handle Subscript => raise REError(RESyntax (LBracket, n))
		         | Empty => raise REError(REBug 1)
		end (* scanRange *)
	  fun scan (n,l,nextp,activeps) = (
		case charOf(s, n)
		 of #"(" => scan(n+1, (LPTOKEN nextp)::l, nextp +1, nextp::activeps)
		  | #")" => scan(n+1, (RPTOKEN(hd activeps))::l, nextp, tl activeps)
		  | #"\\" =>
		    (scan(n+2, (LITOKEN(charOf(s, n + 1)))::l, nextp, activeps)
		       handle Ord => raise REError(RESyntax (Backslash, n)))
		  | #"^" => scan(n + 1, HATOKEN::l, nextp, activeps)
		  | #"$" => scan(n + 1, DOLLARTOKEN::l, nextp, activeps)
		  | #"." => scan(n + 1, DOTOKEN::l, nextp, activeps)
		  | #"*" => scan(n + 1, STARTOKEN::l, nextp, activeps)
		  | #"+" => scan(n + 1, PLUSTOKEN::l, nextp, activeps)
		  | #"?" => scan(n + 1, QUESTOKEN::l, nextp, activeps)
		  | #"|" => scan(n + 1, PIPETOKEN::l, nextp, activeps)
		  | #"[" => let
		      val(n', tok) = scanRange (n + 1) 
		      in
			scan(n', tok::l, nextp, activeps)
		      end
		  | #"]" => raise REError(RESyntax (RBracket, n))
		  |  c  => scan(n + 1, (LITOKEN c)::l, nextp, activeps)
		(* end case *))
(*** NOTE: probably don't want this here ****)
		  handle Subscript => l
		       | Empty => raise REError(RESyntax (RParen, n))
	  in
	    scan (0, nil, 1, nil)
	  end (* scanRegExp *)

(* translates a list of tokens (in right-to-left order) to a re.
 *   tokens -- list of tokens
 *   pcont  -- re
 *   bcont  --
 *   recont --
 *   pempt  --
 *   bempt  --
 *)
  fun compile_re (tokens, pcont, bcont, recont, pempt, bempt) = 
    case tokens of 
	nil =>
	  (case bcont of 
	     SOME p => (nil, BRANCH(pcont, p), pempt orelse bempt)
	   | NONE => (nil, pcont, pempt))
      |	(XTOKEN r)::rest => 
	  compile_re(rest, XRANGE( r, pcont), bcont, recont, false, bempt)
      | (RTOKEN r)::rest => 
          compile_re(rest, RANGE(r, pcont), bcont, recont, false, bempt)
      | (LITOKEN n)::rest => 
	  compile_re(rest, LITERAL(n, pcont), bcont, recont, false, bempt)
      | HATOKEN::rest => 
	  compile_re(rest, SOL pcont, bcont, recont, false, bempt)
      | DOLLARTOKEN::rest => 
	  compile_re(rest, EOL pcont, bcont, recont, pempt, bempt)
      | DOTOKEN::rest => 
	  compile_re(rest, WILD pcont, bcont, recont, pempt, bempt)
      | PIPETOKEN::rest => 
	  (case bcont of 
	       SOME bc =>
		 compile_re(rest, recont, SOME(BRANCH(pcont,bc)), 
		            recont, true, pempt orelse bempt)
	     | NONE =>
		 compile_re(rest, recont, SOME pcont, recont, true, pempt))
      | (LPTOKEN n)::rest => 
	  (case bcont of 
	       SOME bc => (rest, BEGIN(n, BRANCH(pcont,bc)), 
	                   pempt orelse bempt)
	     | NONE => (rest, BEGIN(n, pcont), pempt))
      | (RPTOKEN n)::rest => 
	  let val pcont' = END(n, pcont)
	      val (rest', pcont'', emptiness) = 
	        compile_re(rest, pcont', NONE, pcont', true, false)
	  in compile_re(rest', pcont'', bcont, recont, emptiness, bempt)
	  end
      | STARTOKEN::nil => 
	  raise REError(RESyntax (Star, 0))
      | STARTOKEN::HATOKEN::rest =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | STARTOKEN::DOLLARTOKEN::rest =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | STARTOKEN::(XTOKEN r)::rest => 
	  compile_re(rest, STAR(XRANGE (r, pcont)), 
	  	     bcont, recont, pempt, bempt)      
      | STARTOKEN::(RTOKEN r)::rest => 
	  compile_re(rest, STAR(RANGE (r, pcont)), bcont, recont, pempt, bempt)
      | STARTOKEN::(LITOKEN n)::rest => 
	  compile_re(rest, STAR(LITERAL (n, pcont)), 
	             bcont, recont, pempt, bempt)      
      | STARTOKEN::(DOTOKEN)::rest => 
	  compile_re(rest, STAR(WILD pcont), bcont, recont, pempt, bempt)      
      | STARTOKEN::(RPTOKEN n)::rest => 
	  let val pcont' = END(n, POOL pcont)
	  in 
	      case compile_re(rest, pcont', NONE, pcont', true, false) of
		  (rest', pcont'', false) => 
		      compile_re(rest', LOOP(pcont'', pcont), 
		                 bcont, recont, pempt, bempt)
		| _ => raise REError(REEmptyLoop)
	  end
      | STARTOKEN::(LPTOKEN n)::rest => 
	  raise REError(RESyntax (Star, n))      
      | STARTOKEN::_ =>
	  raise REError(RESyntax (Star, ~1))      
      | PLUSTOKEN::nil => 
	  raise REError(RESyntax (Plus, 0))
      | PLUSTOKEN::(rest as (HATOKEN::_)) =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | PLUSTOKEN::(rest as (DOLLARTOKEN::_)) =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | PLUSTOKEN::(XTOKEN r)::rest => 
	  compile_re(rest, PLUS(XRANGE (r, pcont)), 
	             bcont, recont, false, bempt)      
      | PLUSTOKEN::(RTOKEN r)::rest => 
	  compile_re(rest, PLUS(RANGE (r, pcont)), bcont, recont, false, bempt)      
      | PLUSTOKEN::(LITOKEN n)::rest => 
	  compile_re(rest, PLUS(LITERAL (n, pcont)), 
	             bcont, recont, false, bempt)      
      | PLUSTOKEN::DOTOKEN::rest => 
	  compile_re(rest, PLUS(WILD pcont), bcont, recont, false, bempt)
      | PLUSTOKEN::(RPTOKEN n)::rest => 
	  let val pcont' = END(n, POOL pcont)
	  in 
	      case compile_re(rest, pcont', NONE, pcont', true, false) of
		  (rest', pcont'', false) => 
		      compile_re(rest', LOOP(pcont'', FAIL), 
		                 bcont, recont, false, bempt)
		| _ => raise REError(REEmptyLoop)
	  end
      | PLUSTOKEN::(LPTOKEN n)::_ => 
	  raise REError(RESyntax (Plus, n))      
      | PLUSTOKEN::_ => 
	  raise REError(RESyntax (Plus, ~1))   
      | QUESTOKEN::nil => 
	  raise REError(RESyntax (Quest, 0))
      | QUESTOKEN::HATOKEN::rest =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | QUESTOKEN::DOLLARTOKEN::rest =>
	  compile_re(rest, pcont, bcont, recont, pempt, bempt)
      | QUESTOKEN::(XTOKEN r)::rest => 
	  compile_re(rest, BRANCH(XRANGE(r, pcont), pcont), 
	             bcont, recont, pempt, bempt)
      | QUESTOKEN::(RTOKEN r)::rest => 
	  compile_re(rest, BRANCH(RANGE(r, pcont), pcont), 
	             bcont, recont, pempt, bempt)
      | QUESTOKEN::(LITOKEN n)::rest => 
	  compile_re(rest, BRANCH(LITERAL(n, pcont), pcont), 
	             bcont, recont, pempt, bempt)
      | QUESTOKEN::DOTOKEN::rest => 
	  compile_re(rest, BRANCH(WILD pcont, pcont), 
	             bcont, recont, pempt, bempt)
      | QUESTOKEN::(RPTOKEN n)::rest => 
	  let val pcont' = END(n, pcont)
	      val (rest', pcont'',_) = 
	        compile_re(rest, pcont', NONE, pcont',true,false)
	  in compile_re(rest', BRANCH(pcont'', pcont), 
	                bcont, recont, pempt, bempt)
	  end
      | QUESTOKEN::(LPTOKEN n)::_ =>
	  raise REError(RESyntax (Quest, n))
      | QUESTOKEN::_ =>
	  raise REError(RESyntax (Quest, ~1))

  fun exec_re (s, n, re, l, st) = 
    let fun match_range (x, nil) = raise MatchFail
	  | match_range (x, a::b) = x = a orelse match_range(x, b)
        fun exec_loop(n, m, x) = 
          let val (f, cont) =
	      (case x of 
		   XRANGE(r, k) => ((fn x => not (match_range(x, r))), k)
                 | RANGE(r, k) => ((fn x => match_range(x, r)), k)
		 | LITERAL(m, k) => ((fn x => m = x), k)
		 | WILD k => ((fn x => true), k)
                 | _ => raise REError(REBug 2))		  
	      fun matchup n =
	        (if f (charOf(s, n)) then matchup (n + 1) else n)
	         handle MatchFail => n | Subscript => n
	      fun loop k =
		if k < m then raise MatchFail 
		else (do_exec_re(k, cont) handle MatchFail => loop (k - 1))
	    in
	      loop (matchup n)
	  end  
      and do_exec_re(n, XRANGE(r, cont)) =
	    if match_range (charOf(s, n),r) handle Subscript => raise MatchFail 
	    then raise MatchFail 
	    else do_exec_re(n + 1, cont)
	| do_exec_re(n, RANGE(r, cont)) =
	    if match_range (charOf(s, n),r) handle Subscript => raise MatchFail 
	    then do_exec_re(n + 1, cont)
	    else raise MatchFail
	| do_exec_re(n, LITERAL(m, cont)) =
	    if m = charOf(s, n) handle Subscript => raise MatchFail 
	    then do_exec_re(n + 1, cont) 
	    else raise MatchFail 
	| do_exec_re(n, WILD cont) = 
	    do_exec_re(n + 1, cont)
	| do_exec_re(n, cont as LOOP (cont', cont'')) =
	    (exec_re(s, n, cont', l, cont'::st) 
	     handle MatchFail => do_exec_re(n, cont''))
	| do_exec_re(n, SOL cont) = 
	    if n = 0 then do_exec_re(n, cont) else raise MatchFail
	| do_exec_re(n, EOL cont) = 
	    if n = size s then do_exec_re(n, cont) else raise MatchFail
	| do_exec_re(n, POOL cont) =
	    (case st of 
		 (cont'::rest) => 
		     (do_exec_re(n, cont') 
		      handle MatchFail => exec_re(s, n, cont, l, rest)) 
	       | nil => 
		     raise REError(REBug 5))
	| do_exec_re(n, FAIL) = 
	    raise MatchFail
	| do_exec_re(n, BRANCH(cont, cont')) =
	    (do_exec_re(n, cont) 
	     handle MatchFail => do_exec_re(n, cont'))
	| do_exec_re(n, BEGIN(m, cont)) =
	    exec_re(s, n, cont, (SP{index=m, cpos=n})::l, st) 	
	| do_exec_re(n, END(m, cont)) =
	    exec_re(s, n, cont, (EP{index=m, cpos=n})::l, st)
	| do_exec_re(n, STAR cont) =
	    exec_loop (n, n, cont)
	| do_exec_re(n, PLUS cont) =
	    exec_loop (n, n + 1, cont)
        | do_exec_re(n, DONE) = (EP{index=0,cpos=n})::l
  in do_exec_re(n, re) 
  end

  fun match re s = exec_re(s, 0, re, [SP{index=0,cpos=0}],nil)
        handle MatchFail => nil
  fun compile res =
        #2 (compile_re(scanRegExp res, DONE, NONE, DONE, true, false))

  fun getlimits l n =
    let fun dgl ((SP{index=m,cpos=s})::r, n, ee as SOME e) = 
	      if n = m then (s, e - s) else dgl(r, n, ee)
	  | dgl ((EP{index=m,cpos=e})::r, n, NONE) =
	      if m = n then dgl(r, n, SOME e) else dgl(r, n, NONE)
	  | dgl (_::r, n, ee) = dgl(r, n, ee)
	  | dgl (nil, n, ee) = raise (REError REMatch)
    in case (l,n) of 
	(nil,_) => raise (REError REMatch)
      | (_,SOME n') => dgl (l,n',NONE) 
      | _ => dgl (l,0,NONE)
    end

  end (* RegExp *)

