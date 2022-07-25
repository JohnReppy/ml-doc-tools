(* mldoc-grammar.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parse <GRAMMAR>, <RE>, and <REGEXP> elements.
 *)

structure MLDocGrammar : sig

    structure M : MARKUP

    type symbol = {id : int option, term : string, noindex : bool}

    datatype element
    (* symbols *)
      = NONTERM of symbol
      | TERM of symbol
      | KW of string
      | LIT of string
      | CSET of cset list
    (* elements *)
      | GRP of element list
      | OPT of element
      | STAR of {elem : element, sep : element list}
      | PLUS of {elem : element, sep : element list}
      | ALT of element list

    and cset = CHARS of string | RANGE of (string * string)

    type rhs = element list

    type prod = {id : Atom.atom option, lhs : symbol, rhs : rhs}

  (* parse a <GRAMMAR> element *)
    val getGrammarContents : M.markup -> prod list

  (* parse the contents of a <RE> or <REGEXP> element, returning a rhs *)
    val getREContents : M.markup list -> rhs

  end = struct

    structure E = MLDocElem
    structure M = MLDocParser.Markup


    type symbol = {id : int option, term : string, noindex : bool}

    datatype element
    (* symbols *)
      = NONTERM of symbol
      | TERM of symbol
      | KW of string
      | LIT of string
      | CSET of cset list
    (* elements *)
      | GRP of element list
      | OPT of element
      | STAR of {elem : element, sep : element list}
      | PLUS of {elem : element, sep : element list}
      | ALT of element list

    and cset = CHARS of string | RANGE of (string * string)

    type rhs = element list

    type prod = {id : Atom.atom option, lhs : symbol, rhs : rhs}

  (* parse a list of %GRAM.ELEMENTs *)
    fun getElements markup = let
	  fun getData [M.DATA s] = s
	    | getData m = Error.bogusMarkupList ("PCDATA", m)
	  fun getElem (M.ELEM{elem, body, ...}) = (case elem
		       of E.GRAM_NONTERM{id, noindex} =>
			    NONTERM{id = id, noindex = noindex, term = getData body}
			| E.GRAM_TERM{id, noindex} =>
			    TERM{id = id, noindex = noindex, term = getData body}
			| E.GRAM_KW => KW(getData body)
			| E.GRAM_LIT => LIT(getData body)
			| E.GRAM_CSET => let
			    fun getLit (M.ELEM{elem=E.GRAM_LIT, body=[M.DATA l], ...}) =
				  l
			    fun get [] = []
			      | get (M.ELEM{elem=E.GRAM_RANGE, body=[e1, e2], ...}::r) =
				  RANGE(getLit e1, getLit e2) :: get r
			      | get (elem :: r) =
				  CHARS(getLit elem) :: get r
			    in
			      CSET(get body)
			    end
			| E.GRAM_GRP{count} => let
			    fun getContent ([M.ELEM{elem=E.GRAM_SEP, body, ...}], l) =
				  (List.rev l, getElems body)
			      | getContent ([], l) = (List.rev l, [])
			      | getContent (elem::r, l) =
				  getContent(r, getElem elem :: l)
			    val (grp, sep) = (case getContent(body, [])
			    	   of ([element], sep) => (element, sep)
				    | (elements, sep) => (GRP elements, sep)
				  (* end case *))
			    in
			      case (count, sep)
			       of (E.ONE, []) => grp
				| (E.ZERO_OR_ONE, []) => OPT grp
				| (E.ZERO_OR_MORE, _) => STAR{elem=grp, sep=sep}
				| (E.ONE_OR_MORE, _) => PLUS{elem=grp, sep=sep}
			      (* end case *)
			    end
			| E.GRAM_ALT => ALT(getElems body)
			| elem => Error.bogusElem ("%GRAM.ELEMENT", elem)
		      (* end case *))
	  and getElems [] = []
	    | getElems (elem::r) = getElem elem :: getElems r
	  in
	    getElems markup
	  end

  (* parse a <GRAMMAR> element *)
    fun getGrammarContents _ = raise Fail "getGrammarContents"

  (* parse the contents of a <RE> or <REGEXP> element, returning a rhs *)
    fun getREContents markup = getElements markup

  end
