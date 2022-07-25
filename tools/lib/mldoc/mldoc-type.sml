(* mldoc-type.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * TODO: modify this so that a markup list is accepted and that
 * TYREF elements are retained.
 *)

structure MLDocType : sig

  (* SML types parameterized over the representation of type identifiers *)
    datatype 'a ty
      = VarTy of string
      | BaseTy of ('a ty list * 'a)
      | FnTy of ('a ty * 'a ty)
      | TupleTy of 'a ty list
      | RecordTy of (string * 'a ty) list
      | ParenTy of 'a ty

    val parseTy : (MLDocMarkup.markup -> 'a) -> MLDocMarkup.markup list -> 'a ty
	(* Given a function for mapping a qualified ID; return a function
	 * for parsing SML types.  Qualified IDs are represented as either
	 * PCDATA, or a <TYREF> element.  The function scanTyId can be used
	 * to process the contents of the DATA element into a list of
	 * strings.  The parser introduces ParenTy nodes into the resulting
	 * tree to reflect the required parentheses.
	 *)

    val scanTyId : MLDocMarkup.markup -> string list
	(* given PCDATA or <TYREF> element, return the qualified type
	 * ID as a list of strings.
	 *)

    val scanTyParams : string -> string list
	(* given a string representation of the type parameters of a type
	 * constructor, return the list of type variables.
	 *)

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup
    structure SS = Substring

    datatype 'a ty
      = VarTy of string
      | BaseTy of ('a ty list * 'a)
      | FnTy of ('a ty * 'a ty)
      | TupleTy of 'a ty list
      | RecordTy of (string * 'a ty) list
      | ParenTy of 'a ty

    datatype tok
      = LP | RP			(* "(" ")" *)
      | LCB | RCB		(* "{" "}" *)
      | DOT			(* "." *)
      | COLON			(* ":" *)
      | COMMA			(* "," *)
      | ARROW			(* "->" *)
      | STAR			(* "*" *)
      | ID of string		(* type id/constructor *)
      | VAR of string		(* type variable *)
      | TYREF of M.markup	(* <TYREF> element *)

    datatype tok_stream = TS of substring * M.markup list

    fun isRP RP = true | isRP _ = false
    fun isRCB RCB = true | isRCB _ = false
    fun isCOLON COLON = true | isCOLON _ = false
    fun isSTAR STAR = true | isSTAR _ = false
    fun isDOT DOT = true | isDOT _ = false
    fun isCOMMA COMMA = true | isCOMMA _ = false

(* do we need to translate the data?? *)
    fun tokenStream ((M.DATA s)::r) = TS(SS.full s, r)
      | tokenStream r = TS(SS.full "", r)

(*DEBUG*)
fun dumpStrm (TS(s, r)) = let
      fun f [] = print "\n"
	| f (M.DATA s :: r) = (
	      print(concat["DATA \"", String.toString s, "\""]);
	      f r)
 	| f (M.ELEM{body, ...} :: r) = (print " <ELEM>"; f body; print "</>"; f r)
      in
	print(concat["TS: \"", SS.string s, "\""]); f r
      end
(*DEBUG*)
    exception LexError

    fun notIdChar c = (Char.isAlpha c) orelse (Char.isDigit c)
	  orelse (c = #"'") orelse (c = #"_")

    fun nextTok (TS(ss, rest)) = (case SS.getc ss
	   of NONE => (case rest
		 of [] => NONE
		  | ((e as M.ELEM{elem=E.TYREF _, ...}) :: r) =>
		      SOME(TYREF e, tokenStream r)
		  | (M.ELEM{elem=E.ID, body=[M.DATA x], ...} :: r) =>
		      SOME(ID x, tokenStream r)
		  | (M.ELEM _ :: _) => raise LexError
		  | _ => nextTok(tokenStream rest)
		(* end case *))
	    | SOME(#"(", ss) => SOME(LP, TS(ss, rest))
	    | SOME(#")", ss) => SOME(RP, TS(ss, rest))
	    | SOME(#"{", ss) => SOME(LCB, TS(ss, rest))
	    | SOME(#"}", ss) => SOME(RCB, TS(ss, rest))
	    | SOME(#"*", ss) => SOME(STAR, TS(ss, rest))
	    | SOME(#".", ss) => SOME(DOT, TS(ss, rest))
	    | SOME(#",", ss) => SOME(COMMA, TS(ss, rest))
	    | SOME(#":", ss) => SOME(COLON, TS(ss, rest))
	    | SOME(#"-", ss) => (case SS.getc ss
		 of SOME(#">", ss) => SOME(ARROW, TS(ss, rest))
		  | _ => raise LexError
		(* end case *))
	    | SOME(#"'", ss') => let
		val (v, ss'') = SS.splitl notIdChar ss
		in
		  if (SS.size v >= 2)
		    then SOME(VAR(SS.string v), TS(ss'', rest))
		    else raise LexError
		end
	    | SOME(c, ss') =>
		if (Char.isSpace c) then nextTok(TS(ss', rest))
		else if (Char.isAlpha c) then let
		  val (v, ss'') = SS.splitl notIdChar ss
		  in
		    SOME(ID(SS.string v), TS(ss'', rest))
		  end
		else raise LexError
	  (* end case *))

    fun endOfStrm strm = (case nextTok strm
	   of NONE => true
	    | _ => false
	  (* end case *))

    fun mkFnTy (ty1 as FnTy _, ty2) = FnTy(ParenTy ty1, ty2)
      | mkFnTy (ty1, ty2) = FnTy(ty1, ty2)

    fun mkTupleTy tyl = let
	  fun f (TupleTy ty) = ParenTy(TupleTy ty)
	    | f ty = ty
	  in
	    TupleTy(map f tyl)
	  end

    fun parseListTail {isSep, parseItem} strm = let
	  fun parse (strm, l) = (case nextTok strm
		 of SOME(tok, strm') =>
		      if (isSep tok)
			then let val (item, strm) = parseItem strm'
			  in
			    parse (strm, item::l)
			  end
			else (rev l, strm)
		  | NONE => (rev l, strm)
		(* end case *))
	  in
	    parse (strm, [])
	  end

    fun reportErr ty = let
	  fun tok2str (M.DATA s, l) = s :: l
	    | tok2str (M.ELEM{body, ...}, l) = List.foldr tok2str l body
	  in
	    Error.error'(
	      "bogus ML type \"" :: (List.foldr tok2str ["\""] ty))
	  end

    fun parseTy mkId ty = let
	  fun error () = reportErr ty
	  fun eat isTok strm = (case nextTok strm
		 of SOME(tok', strm) => if (isTok tok') then strm else error()
		  | NONE => error()
		(* end case *))
	  fun mkBaseTy ([ty as TupleTy _], qid) = BaseTy([ParenTy ty], mkId qid)
	    | mkBaseTy ([ty as FnTy _], qid) = BaseTy([ParenTy ty], mkId qid)
	    | mkBaseTy (tys, qid) = BaseTy(tys, mkId qid)
	  fun parseTy1 strm = let
		val (t, rest) = parseTy2 strm
		in
		  case nextTok rest
		   of SOME(ARROW, strm) => let
			val (t', strm) = parseTy1 strm
			in
			  (mkFnTy(t, t'), strm)
			end
		    | _ => (t, rest)
		  (* end case *)
		end
	  and parseTy2 strm = let
		val (t, rest) = parseTy3 strm
		in
		  case nextTok rest
		   of SOME(STAR, _) => let
			val (l, rest) = parseListTail {
				isSep = isSTAR, parseItem = parseTy3
			      } rest
			in
			  (mkTupleTy(t::l), rest)
			end
		    | _ => (t, rest)
		  (* end case *)
		end
	  and parseTy3 strm = (case nextTok strm
		 of SOME(LP, strm) => let
		      val (t, rest) = parseTy1 strm
		      in
			case nextTok rest
			 of SOME(RP, strm) => (case (nextTok strm)
			       of SOME(ID v, strm) => let
				    val (qid, strm) = parseLongId(v, strm)
				    in
				      (mkBaseTy([t], qid), strm)
				    end
				| SOME(TYREF m, strm) => (mkBaseTy([t], m), strm)
				| _ => (t, strm)
			      (* end case *))
			  | SOME(COMMA, _) => let
			      val (params, strm) = parseListTail {
				      isSep=isCOMMA, parseItem=parseTy1
				    } rest
			      val strm = eat isRP strm
			      in
				case nextTok strm
				 of SOME(ID v, strm) => let
				      val (qid, strm) = parseLongId(v, strm)
				      in
					(BaseTy(t::params, mkId qid), strm)
				      end
				  | SOME(TYREF m, strm) =>
					(BaseTy(t::params, mkId m), strm)
				  | _ => error()
				(* end case *)
			      end
			  | _ => error()
			(* end case *)
		      end
		  | _ => let
		      fun getTyCons (argTy, strm) = (case nextTok strm
			     of SOME(ID v, strm) => let
				  val (qid, strm) = parseLongId(v, strm)
				  in
				    getTyCons (mkBaseTy([argTy], qid), strm)
				  end
			      | SOME(TYREF m, strm) =>
				  getTyCons (mkBaseTy([argTy], m), strm)
			      | _ => (argTy, strm)
			    (* end case *))
		      in
			getTyCons (parseTy4 strm)
		      end
		(* end case *))
	  and parseTy4 strm = (case nextTok strm
		 of SOME(LCB, strm) => let
		      fun parseField strm = (case nextTok strm
			     of SOME(ID lab, strm) => let
				  val strm = eat isCOLON strm
				  val (t, strm) = parseTy1 strm
				  in
				    ((lab, t), strm)
				  end
			      | _ => error()
			    (* end case *))
		      in
			case nextTok strm
			 of SOME(RCB, strm) => (RecordTy[], eat isRCB strm)
			  | _ => let
			      val (f, strm) = parseField strm
			      val (l, strm) = parseListTail {
				      isSep=isCOMMA, parseItem=parseField
				    } strm
			      in
				(RecordTy(f::l), eat isRCB strm)
			      end
			(* end case *)
		      end
		  | SOME(VAR v, strm) => (VarTy v, strm)
		  | SOME(ID v, strm) => let
		      val (qid, strm) = parseLongId(v, strm)
		      in
			(BaseTy([], mkId qid), strm)
		      end
		  | SOME(TYREF m, strm) => (BaseTy([], mkId m), strm)
		  | _ => error()
		(* end case *))
	  and parseLongId (id, strm) = let
		fun parseId strm = (case nextTok strm
		       of SOME(ID v, strm) => (v, strm)
			| _ => error()
		      (* end case *))
		val (l, strm) = parseListTail {isSep=isDOT, parseItem=parseId} strm
		fun qualify [] = []
		  | qualify [id] = [id]
		  | qualify (id::r) = id :: "." :: qualify r
		in
		  (M.DATA(String.concat(qualify(id::l))), strm)
		end
	  val (t, rest) = parseTy1 (tokenStream ty) handle LexError => error()
	  in
	    case nextTok rest
	     of NONE => t
	      | _ => error()
	    (* end case *)
	  end

    fun scanTyId (d as (M.DATA _)) = let
	  fun error () = Error.error "MLDocType.scanTyId: bogus ML type ID"
	  val strm = tokenStream[d]
	  fun parseId strm = (case nextTok strm
		 of SOME(ID v, strm) => (v, strm)
		  | _ => error()
		(* end case *))
	  in
	    case (nextTok strm)
	     of SOME(ID v, strm) =>
		  v :: #1(parseListTail {isSep=isDOT, parseItem=parseId} strm)
	      | _ => error ()
	    (* end case *)
	  end
      | scanTyId (M.ELEM{elem=E.TYREF _, body=[x], ...}) = scanTyId x
      | scanTyId (M.ELEM{elem=E.ID, body=[M.DATA x], ...}) =
	(* The data should be a single identifier (probably an ID family) *)
	  [x]
      | scanTyId _ = raise Fail "MLDocType.scanTyId"

  (* given a string representation of the type parameters of a type
   * constructor, return the list of type variables.
   *)
    fun scanTyParams s = let
	  fun error () =  raise Fail "MLDocType.scanTyParams"
	  fun scanParams (strm, params) = (case nextTok strm
		 of SOME(VAR tyvar, strm) => (case nextTok strm
		       of SOME(COMMA, strm) => scanParams(strm, tyvar::params)
			| SOME(RP, strm) => if (endOfStrm strm)
			    then rev(tyvar::params)
			    else error()
			| _ => error()
		      (* end case *))
		  | _ => error()
		(* end case *))
	  in
	    case nextTok(TS(SS.full s, []))
	     of NONE => []
	      | SOME(LP, strm) => scanParams (strm, [])
	      | SOME(VAR tyvar, strm) => if (endOfStrm strm)
		  then [tyvar]
		  else error()
	      | _ => error()
	    (* end case *)
	  end

  end;

