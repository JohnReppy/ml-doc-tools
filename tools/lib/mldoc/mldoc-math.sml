(* mldoc-math.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Support for analysing <MATH> and <DISPLAYMATH> elements.
 *)

structure MLDocMath : sig

    structure M : MARKUP

    datatype math_markup
      = Text of M.markup list
      | Data of string
      | Group of math_markup list
      | Sum of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Prod of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Union of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Intersect of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Frac of {num : math_markup, denom : math_markup}
      | Subscript of (math_markup * math_markup)
      | Superscript of (math_markup * math_markup)
      | Mod of (math_markup * math_markup)
      | Norm of math_markup
      | Floor of math_markup
      | Ceiling of math_markup
      | Set of math_markup list

    val getMathContents : M.markup list -> math_markup list
	(* parse the contents of a <MATH> or <DISPLAYMATH> element, returning
	 * a list of math_markup.
	 *)

    val getEqnarrayContents : M.markup list -> (math_markup list * math_markup list * math_markup list) list
	(* parse the contents of a <EQNARRAY> element *)

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup

    datatype math_markup
      = Text of M.markup list
      | Data of string
      | Group of math_markup list
      | Sum of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Prod of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Union of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Intersect of {ll : math_markup, ul : math_markup, opd : math_markup option}
      | Frac of {num : math_markup, denom : math_markup}
      | Subscript of (math_markup * math_markup)
      | Superscript of (math_markup * math_markup)
      | Mod of (math_markup * math_markup)
      | Norm of math_markup
      | Floor of math_markup
      | Ceiling of math_markup
      | Set of math_markup list

    fun error msg = raise Fail("MLDocMath: "^msg)

    fun parseMathText (elems, iter) = (case (M.next iter)
	   of (SOME(e as M.ELEM{elem, body, ...}, iter')) => (
		case elem
		 of E.SUM => parseMathText (
		      parseSummation (Sum, body) :: elems, M.right iter')
		  | E.PROD => parseMathText (
		      parseSummation (Sum, body) :: elems, M.right iter')
		  | E.UNION => parseMathText (
		      parseSummation (Sum, body) :: elems, M.right iter')
		  | E.INTERSECT => parseMathText (
		      parseSummation (Sum, body) :: elems, M.right iter')
		  | E.MGROUP => parseMathText (
		      Group(parseMathText([], M.iterateList body)) :: elems,
		      M.right iter')
		  | E.FRAC => parseMathText (
		      parseFrac body :: elems, M.right iter')
		  | E.SUB => parseMathText (
		      parseScript (Subscript, body, elems), M.right iter')
		  | E.SUP => parseMathText (
		      parseScript (Superscript, body, elems), M.right iter')
		  | E.MOD => parseMathText (
		      parseScript (Mod, body, elems), M.right iter')
		  | E.NORM => parseMathText (
		      Norm(parseGroup body) :: elems, M.right iter')
		  | E.FLOOR => parseMathText (
		      Floor(parseGroup body) :: elems, M.right iter')
		  | E.CEILING => parseMathText (
		      Ceiling(parseGroup body) :: elems, M.right iter')
		  | E.SET => parseMathText (
		      Set(parseList body) :: elems, M.right iter')
		  | E.MTEXT => parseMathText (
		      Text body :: elems, M.right iter')
		  | E.ARG => parseMathText (
                      Text[e] :: elems, M.right iter')
		  | e => Error.bogusElem("%MATHTEXT", e)
		(* end case *))
	    | (SOME(M.DATA s, iter')) => parseMathText (Data s :: elems, iter')
	    | NONE => List.rev elems
	  (* end case *))

    and parseList body = parseMathText([], M.iterateList body)

    and parseGroup body = (case (parseList body)
	   of [mm] => mm
	    | l => Group l
	  (* end case *))

    and parseSummation (cons, [M.ELEM{body=ll, ...}, M.ELEM{body=ul, ...}]) =
	  cons{ll = parseGroup ll, ul = parseGroup ul, opd = NONE}
      | parseSummation (
	  cons,
	  [M.ELEM{body=ll, ...}, M.ELEM{body=ul, ...}, M.ELEM{body=opd, ...}]
	) =
	  cons{
	      ll = parseGroup ll, ul = parseGroup ul,
	      opd = SOME(parseGroup opd)
	    }
      | parseSummation _ = error "badly formed summation"

    and parseFrac body = let
	  fun getNumerator (M.ELEM{elem=E.OVER, ...}::r, elems) =
		(List.rev elems, r)
	    | getNumerator (e::r, elems) = getNumerator(r, e::elems)
	    | getNumerator ([], _) = Error.error "missing <OVER> in <FRAC>"
	  val (num, denom) = getNumerator (body, [])
	  in
	    Frac{num = parseGroup num, denom = parseGroup denom}
	  end

    and parseScript (cons, body, elem::rest) =
	(* we always make the rhs a group so that the output parses
	 * correctly.
	 *)
	  cons(elem, Group(parseList body))::rest
      | parseScript _ = error "badly formed subscript/superscript"

    fun getMathContents body = parseMathText([], M.iterateList body)

  (* parse the contents of a <EQNARRAY> element *)
    fun getEqnarrayContents [] = []
      | getEqnarrayContents (M.ELEM{elem=E.EQN, body, ...} :: rest) = let
	(* divide the body into lhs, relation, rhs *)
	  fun split (M.ELEM{elem=E.EQNREL, body, ...} :: r, lhs) =
		(List.rev lhs, body, r)
	    | split (e :: r, lhs) = split (r, e::lhs)
	    | split ([], _) = error "bogus <EQN> content; missing <EQNREL>"
	  val (lhs, rel, rhs) = split (body, [])
	  in
	    (getMathContents lhs, getMathContents rel, getMathContents rhs)
	      :: getEqnarrayContents rest
	  end

  end;

