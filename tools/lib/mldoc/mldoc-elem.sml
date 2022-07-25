(* mldoc-elem.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The SGMLS elements for the ML-Doc DTD.
 *)

structure MLDocElem =
  struct

    type attr_val = SGMLS.attr_val

    datatype entity
      = ExternIntity of (SGMLS.entity_type * Atom.atom)
      | InternEntity of (SGMLS.entity_type * string)

    type env = {
	entityMap : Atom.atom -> entity option,
	publicIds : Atom.atom -> bool,
	systemIds : Atom.atom -> bool
      }

  (* caption alignments in floats *)
    datatype cap_align = TOP | BOTTOM

  (* column alignments in tables *)
    datatype tbl_align = ALIGN_CENTER | ALIGN_LEFT | ALIGN_RIGHT

  (* counts for groups of grammar symbols *)
    datatype item_count
      = ONE
      | ZERO_OR_ONE
      | ZERO_OR_MORE
      | ONE_OR_MORE

    datatype index_mark = START | STOP | HERE

    datatype module_id
      = TOPID
      | SIGID of string
      | STRID of string
      | FCTID of string

    datatype module_status
      = REQUIRED
      | OPTIONAL
      | PROPOSED

    type xref_info = {
	home : module_id option,
	document : string option,
	nolink : bool,
	noindex : bool
      }

  (* ML-Doc DTD elements (in alphabetical order) *)
    datatype element
      = ADEF of {tag : string}
      | AREF of {tag : string, document : string option}
      | ARG
      | AUTHOR of {
	    name : string, email : string,
	    year : int, month : int option, day : int option
	  }
      | BF
      | CAPTION
      | CD of {lang : string option}
      | CEILING
      | CITE of {key : string}
      | CODE of {lang : string option}
      | COL of {align : tbl_align option, parbox : string option}
      | COMMENT
      | CONREF of xref_info
      | CONS
      | COPYRIGHT of {owner : string, year : int}
      | DATATYPE of {compact : bool, recursive : bool}
      | DATATYPEDEF
      | DESCRIP
      | DISPLAYMATH
      | DOCREF of {document : string}
      | DTAG
      | EM
      | ENUM
      | EQN
      | EQNARRAY
      | EQNREL
      | EQTYPE
      | EQU
      | EVALTO
      | EXAMPLE
      | EXN
      | EXNREF of xref_info
      | FCTARGREF of {document : string option, nolink : bool, noindex : bool}
      | FCTREF of {document : string option, nolink : bool, noindex : bool}
      | FIGURE of {file : string}
      | FLOAT of {label : string, capalign : cap_align option}
      | FLOATREF of {label : string}
      | FLOOR
      | FRAC
      | FUNCTOR of {fctid : string, status : module_status option}
      | GRAM_ALT
      | GRAM_CSET
      | GRAM_GRP of {count : item_count}
      | GRAM_KW
      | GRAM_LIT
      | GRAM_NONTERM of {id : int option, noindex : bool}
      | GRAM_PROD of {id : Atom.atom option}
      | GRAM_RANGE
      | GRAM_RHS
      | GRAM_SEP
      | GRAM_TERM of {id : int option, noindex : bool}
      | GRAMMAR
      | HEAD
      | ID
      | IDREF of {kind : string option, noindex : bool}
      | IMPLNOTE
      | INCLFILE of {file : string}
      | INCLUDE
      | INDEX of {key : string, see : string option, mark : index_mark, which : string option}
      | INTERFACE of {label : string option}
      | INTERSECT
      | IT
      | ITEM
      | ITEMIZE
      | KEY_VIEW
      | KW
      | LL
      | MATH
      | MGROUP
      | ML_DOC
      | MOD
      | MTEXT
      | NORM
      | OPAQUE
      | OPD
      | OVER
      | PP
      | PROD
      | PROTO
      | PROTOTY
      | QUESTION
      | RAISES
      | RATIONALE
      | RE
      | REGEXP
      | SECREF of {label : string}
      | SECTION of {label : string option, nonumber : bool, notoc : bool}
      | SEEALSO
      | SET
      | SHARING of {ty : bool}
      | SIGBODY of {sigid : string option, file : string option}
      | SIGINSTANCE of {status : module_status option, opaque : bool}
      | SIGNATURE of {sigid : string, status : module_status option}
      | SIGREF of {document : string option, nolink : bool, noindex : bool}
      | SPEC
      | SPECBREAK of {newline : bool}
      | STRREF of xref_info
      | STRUCTURE of {strid : string, status : module_status option}
      | SUB
      | SUBINDEX of {key : string}
      | SUBSTRUCT
      | SUM
      | SUP
      | SYSNOTE of {opsys : string list, arch : string list}
      | TABLE of {long : bool, small : bool}
      | TD of {colspan : int option, align : tbl_align option}
      | TH of {colspan : int option, align : tbl_align option}
      | TITLE
      | TR
      | TT
      | TY
      | TYPARAM
      | TYPE
      | TYREF of xref_info
      | UL
      | UNION
      | URL of {href : string}
      | VAL
      | VALREF of xref_info
      | VERSION of {
	    verid : string option,
	    year : int, month : int option, day : int option
	  }
      | WHERETYPE

    local
      structure F = Format
      fun fmtIntOpt NONE = F.INT 0
	| fmtIntOpt (SOME n) = F.INT n
      fun fmtOpt NONE = F.STR "<none>"
	| fmtOpt (SOME s) = F.STR s
      fun fmtOptAtom NONE = F.STR "<none>"
	| fmtOptAtom (SOME a) = F.ATOM a
      fun fmtCapAlignOpt NONE = F.STR "<none>"
	| fmtCapAlignOpt (SOME TOP) = F.STR "TOP"
	| fmtCapAlignOpt (SOME BOTTOM) = F.STR "BOTTOM"
      fun fmtAlignOpt NONE = F.STR "<none>"
	| fmtAlignOpt (SOME ALIGN_CENTER) = F.STR "CENTER"
	| fmtAlignOpt (SOME ALIGN_LEFT) = F.STR "LEFT"
	| fmtAlignOpt (SOME ALIGN_RIGHT) = F.STR "RIGHT"
      fun fmtHome NONE = [F.STR "<none>", F.STR ""]
	| fmtHome (SOME TOPID) = [F.STR "TOPID", F.STR ""]
	| fmtHome (SOME(SIGID id)) = [F.STR "SIGID = ", F.STR id]
	| fmtHome (SOME(STRID id)) = [F.STR "STRID = ", F.STR id]
	| fmtHome (SOME(FCTID id)) = [F.STR "FCTID = ", F.STR id]
      fun fmtStatus NONE = F.STR "<none>"
	| fmtStatus (SOME REQUIRED) = F.STR "REQUIRED"
	| fmtStatus (SOME OPTIONAL) = F.STR "OPTIONAL"
	| fmtStatus (SOME PROPOSED) = F.STR "PROPOSED"
      fun fmtCount ONE = F.STR "ONE"
	| fmtCount ONE_OR_MORE = F.STR "ONE-OR-MORE"
	| fmtCount ZERO_OR_ONE = F.STR "ZERO-OR-ONE"
	| fmtCount ZERO_OR_MORE = F.STR "ZERO-OR-MORE"
      fun fmtIndexMark HERE = F.STR "HERE"
	| fmtIndexMark START = F.STR "START"
	| fmtIndexMark STOP = F.STR "STOP"
      fun fmtXRef (name, {home, document, nolink, noindex}) =
	    F.format "%s{%s%s, document = \"%s\", nolink = %b, noindex = %b}}"
	      (F.STR name ::
		((fmtHome home) @ [fmtOpt document, F.BOOL nolink, F.BOOL noindex]))
    in
    fun elemName (ADEF{tag}) = F.format "ADEF{tag = %s}" [F.STR tag]
      | elemName (AREF{tag, document}) =
	  F.format "AREF{tag = %s, document = \"%s\"}" [F.STR tag, fmtOpt document]
      | elemName ARG = "ARG"
      | elemName (AUTHOR{name, email, year, month, day}) =
	  F.format "AUTHOR{name = %s, email = %s, %02d-%02d-%02d}" [
	      F.STR name, F.STR email, F.INT(year - 1900),
	      fmtIntOpt month, fmtIntOpt day
	    ]
      | elemName BF = "BF"
      | elemName CAPTION = "CAPTION"
      | elemName (CD{lang}) = F.format "CD{lang = \"%s\"}" [fmtOpt lang]
      | elemName CEILING = "CEILING"
      | elemName (CITE{key}) = F.format "CITE{key = %s}" [F.STR key]
      | elemName (CODE{lang}) = F.format "CODE{lang = \"%s\"}" [fmtOpt lang]
      | elemName (COL{align, parbox}) =
	  F.format "COL{align = %s, parbox = %s}" [fmtAlignOpt align, fmtOpt parbox]
      | elemName COMMENT = "COMMENT"
      | elemName (CONREF arg) = fmtXRef("CONREF", arg)
      | elemName CONS = "CONS"
      | elemName (COPYRIGHT{owner, year}) =
	  F.format "COPYRIGHT{owner = %s, year = %d}" [F.STR owner, F.INT year]
      | elemName (DATATYPE{compact, recursive}) =
	  F.format "TYPE{compact = %b, recursive = %b}"
	    [F.BOOL compact, F.BOOL recursive]
      | elemName DATATYPEDEF = "DATATYPEDEF"
      | elemName DESCRIP = "DESCRIP"
      | elemName DISPLAYMATH = "DISPLAYMATH"
      | elemName (DOCREF{document}) =
	  F.format "DOCREF{document = %s}" [F.STR document]
      | elemName DTAG = "DTAG"
      | elemName EM = "EM"
      | elemName ENUM = "ENUM"
      | elemName EQN = "EQN"
      | elemName EQNARRAY = "EQNARRAY"
      | elemName EQNREL = "EQNREL"
      | elemName EQTYPE = "EQTYPE"
      | elemName EQU = "EQU"
      | elemName EVALTO = "EVALTO"
      | elemName EXAMPLE = "EXAMPLE"
      | elemName EXN = "EXN"
      | elemName (EXNREF arg) = fmtXRef("EXNREF", arg)
      | elemName (FCTARGREF{document, nolink, noindex}) =
	  F.format "FCTARGREF{document = \"%s\", nolink = %b, noindex = %b}"
	    [fmtOpt document, F.BOOL nolink, F.BOOL noindex]
      | elemName (FCTREF{document, nolink, noindex}) =
	  F.format "FCTREF{document = \"%s\", nolink = %b, noindex = %b}"
	    [fmtOpt document, F.BOOL nolink, F.BOOL noindex]
      | elemName (FIGURE{file}) = F.format "FIGURE{file = %s}" [F.STR file]
      | elemName (FLOAT{label, capalign}) =
	  F.format "FLOAT{label = %s, capalign = %s}"
	    [F.STR label, fmtCapAlignOpt capalign]
      | elemName (FLOATREF{label}) = F.format "FLOATREF{label = %s}" [F.STR label]
      | elemName FLOOR = "FLOOR"
      | elemName FRAC = "FRAC"
      | elemName (FUNCTOR{fctid, status}) =
	  F.format "FUNCTOR{fctid = %s, status = %s}" [
	      F.STR fctid, fmtStatus status
	    ]
      | elemName GRAM_ALT = "GRAM.ALT"
      | elemName GRAM_CSET = "GRAM.CSET"
      | elemName (GRAM_GRP{count}) = 
	  F.format "GRAM.GRP{count = %s}" [fmtCount count]
      | elemName GRAM_KW = "GRAM.KW"
      | elemName GRAM_LIT = "GRAM.LIT"
      | elemName (GRAM_NONTERM{id, noindex}) =
	  F.format "GRAM.NONTERM{id = %d, noindex = %b}"
	    [fmtIntOpt id, F.BOOL noindex]
      | elemName (GRAM_PROD{id}) =
	  F.format "GRAM_PROD{id = \"%s\"}" [fmtOptAtom id]
      | elemName GRAM_RHS = "GRAM.RHS"
      | elemName GRAM_RANGE = "GRAM.RANGE"
      | elemName GRAM_SEP = "GRAM.SEP"
      | elemName (GRAM_TERM{id, noindex}) =
	  F.format "GRAM.TERM{id = %d, noindex = %b}"
	    [fmtIntOpt id, F.BOOL noindex]
      | elemName GRAMMAR = "GRAMMAR"
      | elemName HEAD = "HEAD"
      | elemName ID = "ID"
      | elemName (IDREF{kind, noindex}) =
	  F.format "IDREF{kind = %s, noindex = %b}" [fmtOpt kind, F.BOOL noindex]
      | elemName IMPLNOTE = "IMPLNOTE"
      | elemName (INCLFILE{file}) = F.format "INCLFILE{file = \"%s\"}" [F.STR file]
      | elemName INCLUDE = "INCLUDE"
      | elemName (INDEX{key, see, mark, which}) =
	  F.format "INDEX{key = \"%s\", see = \"%s\", mark = %s, which = \"%s\"}"
	    [F.STR key, fmtOpt see, fmtIndexMark mark, fmtOpt which]
      | elemName (INTERFACE{label}) =
	  F.format "INTERFACE{label = %s}" [fmtOpt label]
      | elemName INTERSECT = "INTERSECT"
      | elemName IT = "IT"
      | elemName ITEM = "ITEM"
      | elemName ITEMIZE = "ITEMIZE"
      | elemName KEY_VIEW = "KEY-VIEW"
      | elemName KW = "KW"
      | elemName LL = "LL"
      | elemName MATH = "MATH"
      | elemName MGROUP = "MGROUP"
      | elemName ML_DOC = "ML-DOC"
      | elemName MOD = "MOD"
      | elemName MTEXT = "MTEXT"
      | elemName NORM = "NORM"
      | elemName OPAQUE = "OPAQUE"
      | elemName OPD = "OPD"
      | elemName OVER = "OVER"
      | elemName PP = "PP"
      | elemName PROD = "PROD"
      | elemName PROTO = "PROTO"
      | elemName PROTOTY = "PROTOTY"
      | elemName QUESTION = "QUESTION"
      | elemName RAISES = "RAISES"
      | elemName RATIONALE = "RATIONALE"
      | elemName RE = "RE"
      | elemName REGEXP = "REGEXP"
      | elemName (SECREF{label}) = F.format "SECREF{label = %s}" [F.STR label]
      | elemName (SECTION{label, nonumber, notoc}) =
	  F.format "SECTION{label = %s, nonumber = %b, notoc = %b}"
	    [fmtOpt label, F.BOOL nonumber, F.BOOL notoc]
      | elemName SEEALSO = "SEEALSO"
      | elemName SET = "SET"
      | elemName (SHARING{ty}) = F.format "SHARING{ty = %b}" [F.BOOL ty]
      | elemName (SIGBODY{sigid, file}) =
	  F.format "SIGBODY{sigid = %s, file = %s}" [fmtOpt sigid, fmtOpt file]
      | elemName (SIGINSTANCE{status, opaque}) =
	  F.format "SIGINSTANCE{status = %s, opaque = %b}"
	    [fmtStatus status, F.BOOL opaque]
      | elemName (SIGNATURE{sigid, status}) =
	  F.format "SIGNATURE{sigid = %s, status = %s}" [
	      F.STR sigid, fmtStatus status
	    ]
      | elemName (SIGREF{document, nolink, noindex}) =
	  F.format "SIGREF{document = \"%s\", nolink = %b, noindex = %b}"
	    [fmtOpt document, F.BOOL nolink, F.BOOL noindex]
      | elemName SPEC = "SPEC"
      | elemName (SPECBREAK{newline}) =
	  F.format "SPECBREAK{newline = %b}" [F.BOOL newline]
      | elemName (STRREF{home, document, nolink, noindex}) =
	  F.format "STRREF{%s%s, document = \"%s\", nolink = %b, noindex = %b}"
	    ((fmtHome home) @ [fmtOpt document, F.BOOL nolink, F.BOOL noindex])
      | elemName (STRUCTURE{strid, status}) =
	  F.format "STRUCTURE{strid = %s, status = %s}" [
	      F.STR strid, fmtStatus status
	    ]
      | elemName SUB = "SUB"
      | elemName (SUBINDEX{key}) = F.format "SUBINDEX{key = \"%s\"}" [F.STR key]
      | elemName SUBSTRUCT = "SUBSTRUCT"
      | elemName SUM = "SUM"
      | elemName SUP = "SUP"
      | elemName (SYSNOTE{opsys, arch}) = let
	  fun fmt l = F.STR(
		ListFormat.fmt {
		    init="[", sep=",", final="]", fmt = fn x => x
		  } l)
	  in
	    F.format "SYSNOTE{opsys=%s, arch=%s}" [fmt opsys, fmt arch]
	  end
      | elemName (TABLE{long, small}) =
	  F.format "TABLE{long = %b, small = %b}" [F.BOOL long, F.BOOL small]
      | elemName (TD{colspan=NONE, align=NONE}) = "TD"
      | elemName (TD{colspan, align}) =
	  F.format "TD{colspan = %d, align = %s}" [
	      fmtIntOpt colspan, fmtAlignOpt align
	    ]
      | elemName (TH{colspan, align}) =
	  F.format "TH{colspan = %d, align = %s}" [
	      fmtIntOpt colspan, fmtAlignOpt align
	    ]
      | elemName TITLE = "TITLE"
      | elemName TR = "TR"
      | elemName TT = "TT"
      | elemName TY = "TY"
      | elemName TYPARAM = "TYPARAM"
      | elemName TYPE = "TYPE"
      | elemName (TYREF arg) = fmtXRef("TYREF", arg)
      | elemName UL = "UL"
      | elemName UNION = "UNION"
      | elemName (URL{href}) = F.format "URL{href = \"%s\"}" [F.STR href]
      | elemName VAL = "VAL"
      | elemName (VALREF arg) = fmtXRef("VALREF", arg)
      | elemName (VERSION{verid, year, month, day}) =
	  F.format "VERSION{verid = %s, %02d-%02d-%02d}"
	    [fmtOpt verid, F.INT(year - 1900), fmtIntOpt month, fmtIntOpt day]
      | elemName WHERETYPE = "WHERETYPE"
    end

    local
      open MLDocDTD

      fun mk cons (_, []) = cons
	| mk cons _ = raise Fail("unexpected attributes" ^ elemName cons)

      fun find ([], attrName) = NONE
	| find ((an, av)::r, attrName) = if Atom.sameAtom(attrName, an)
	    then SOME av
	    else find (r, attrName)
      fun bindAttr (env : env, attrs, attrName) = let
	    fun error () = raise Fail(Format.format
		  "expected CDATA value for %s attribute"
		    [Format.ATOM attrName])
	    in
	      case (find(attrs, attrName))
	       of NONE => NONE
	        | (SOME(SGMLS.CDATA_VAL s)) => SOME s
	        | (SOME(SGMLS.ENTITY_VAL[entity])) => (
		    case (#entityMap env entity)
		     of SOME(InternEntity(SGMLS.CDATA, data)) => SOME data
		      | SOME(InternEntity(SGMLS.SDATA, data)) =>
			  SOME(concat["\\|", data, "\\|"])
		      | _ => error()
		    (* end case *))
		| (SOME SGMLS.IMPLIED_VAL) => NONE
	        | _ => error()
	      (* end case *)
	    end
      fun bindNames (attrs, attrName) = (case find(attrs, attrName)
	     of NONE => []
	      | (SOME(SGMLS.TOKEN_VAL(l as n::_))) => map Atom.toString l
	      | (SOME(SGMLS.IMPLIED_VAL)) => []
	      | _ =>  raise Fail "expected TOKEN/IMPLIED attribute value"
	    (* end case *))
      fun bindIntAttr (attrs, attrName) = (case find(attrs, attrName)
	     of NONE => NONE
	      | (SOME(SGMLS.TOKEN_VAL[s])) => Int.fromString(Atom.toString s)
	      | (SOME(SGMLS.IMPLIED_VAL)) => NONE
	      | _ => raise Fail "expected TOKEN/IMPLIED attribute value"
	    (* end case *))
      fun bindAttrFlag (attrs, attrName) = (case find(attrs, attrName)
	     of (SOME(SGMLS.TOKEN_VAL _)) => true
	      | (SOME SGMLS.IMPLIED_VAL) => false
	      | _ => raise Fail "expected TOKEN/IMPLIED attribute value"
	    (* end case *))
      fun bindTokenAttr (attrs, attrName, scanFn) = (case find(attrs, attrName)
	     of (SOME(SGMLS.TOKEN_VAL[v])) => scanFn v
	      | (SOME SGMLS.IMPLIED_VAL) => NONE
	      | _ => raise Fail "expected TOKEN/IMPLIED attribute value"
	    (* end case *))
      fun bindIdAttr (attrs, attrName) = (case find(attrs, attrName)
	     of (SOME(SGMLS.TOKEN_VAL[n])) => SOME n
	      | (SOME(SGMLS.ID_VAL n)) => SOME n
	      | (SOME(SGMLS.IMPLIED_VAL)) => NONE
	      | _ => raise Fail "expected ID/TOKEN attribute value"
	    (* end case *))
      fun require NONE = raise Fail "missing attribute"
	| require (SOME x) = x

      fun scanCapAlign tok =
	    if Atom.sameAtom(tok, tokTOP) then SOME TOP
	    else if Atom.sameAtom(tok, tokBOTTOM) then SOME BOTTOM
	    else raise Fail "bogus caption alignment value"

      fun scanAlign tok =
	    if Atom.sameAtom(tok, tokLEFT) then SOME ALIGN_LEFT
	    else if Atom.sameAtom(tok, tokCENTER) then SOME ALIGN_CENTER
	    else if Atom.sameAtom(tok, tokRIGHT) then SOME ALIGN_RIGHT
	    else raise Fail "bogus column alignment value"

      fun scanStatus tok =
	    if Atom.sameAtom(tok, tokOPTIONAL) then SOME OPTIONAL
	    else if Atom.sameAtom(tok, tokPROPOSED) then SOME PROPOSED
	    else if Atom.sameAtom(tok, tokREQUIRED) then SOME REQUIRED
	    else raise Fail "bogus status value"

      fun scanCount tok =
	    if Atom.sameAtom(tok, tokONE) then SOME ONE
	    else if Atom.sameAtom(tok, tokZERO_OR_ONE) then SOME ZERO_OR_ONE
	    else if Atom.sameAtom(tok, tokZERO_OR_MORE) then SOME ZERO_OR_MORE
	    else if Atom.sameAtom(tok, tokONE_OR_MORE) then SOME ONE_OR_MORE
	    else raise Fail "bogus item count value"

      fun scanIndexMark tok =
	    if Atom.sameAtom(tok, tokSTART) then SOME START
	    else if Atom.sameAtom(tok, tokSTOP) then SOME STOP
	    else raise Fail "bogus index mark value"

      fun mkADEF (env, attrs) = ADEF{
	      tag = require (bindAttr (env, attrs, attrTAG))
	    }
      fun mkAREF (env, attrs) = AREF{
	      tag = require (bindAttr (env, attrs, attrTAG)),
	      document = bindAttr (env, attrs, attrDOCUMENT)
	    }
      fun mkAUTHOR (env, attrs) = AUTHOR{
	      name = require (bindAttr (env, attrs, attrNAME)),
	      email = require (bindAttr (env, attrs, attrEMAIL)),
	      year = require (bindIntAttr (attrs, attrYEAR)),
	      month = bindIntAttr (attrs, attrMONTH),
	      day = bindIntAttr (attrs, attrDAY)
	    }
      fun mkCD (env, attrs) = CD{lang = bindAttr (env, attrs, attrLANG)}
      fun mkCITE (env, attrs) = CITE{
	      key = require (bindAttr (env, attrs, attrKEY))
	    }
      fun mkCODE (env, attrs) = CODE{lang = bindAttr (env, attrs, attrLANG)}
      fun mkCOL (env, attrs) = COL{
	      align = bindTokenAttr (attrs, attrALIGN, scanAlign),
	      parbox = bindAttr (env, attrs, attrPARBOX)
	    }
      fun mkCOPYRIGHT (env, attrs) = COPYRIGHT{
	      owner = require (bindAttr (env, attrs, attrOWNER)),
	      year = require (bindIntAttr (attrs, attrYEAR))
	    }
      fun mkDATATYPE (env, attrs) = DATATYPE{
	      compact = bindAttrFlag (attrs, attrCOMPACT),
	      recursive = bindAttrFlag (attrs, attrREC)
	    }
      fun mkDOCREF (env, attrs) = DOCREF{
	      document = require (bindAttr (env, attrs, attrDOCUMENT))
	    }
      fun mkFIGURE (env, attrs) = FIGURE{
	      file = require (bindAttr (env, attrs, attrFILE))
	    }
      fun mkFLOAT (env, attrs) = FLOAT{
	      label = require (bindAttr (env, attrs, attrLABEL)),
	      capalign = bindTokenAttr (attrs, attrCAPALIGN, scanCapAlign)
	    }
      fun mkFLOATREF (env, attrs) = FLOATREF{
	      label = require (bindAttr (env, attrs, attrLABEL))
	    }
      fun mkFUNCTOR (env, attrs) = FUNCTOR{
	      fctid = require (bindAttr (env, attrs, attrFCTID)),
	      status = bindTokenAttr (attrs, attrSTATUS, scanStatus)
	    }
      fun mkGRAM_GRP (env, attrs) = GRAM_GRP{
	      count = require (bindTokenAttr (attrs, attrCOUNT, scanCount))
	    }
      fun mkGramSymb cons (env, attrs) = cons{
	      id = bindIntAttr (attrs, attrID),
	      noindex = bindAttrFlag (attrs, attrNOINDEX)
	    }
      fun mkGRAM_PROD (env, attrs) = GRAM_PROD{
	      id = bindIdAttr (attrs, attrID)
	    }
      fun mkIDREF (env, attrs) = IDREF{
	      kind = bindAttr (env, attrs, attrKIND),
	      noindex = bindAttrFlag (attrs, attrNOINDEX)
	    }
      fun mkINCLFILE (env, attrs) = INCLFILE{
	      file = require (bindAttr (env, attrs, attrFILE))
	    }
      fun mkINDEX (env, attrs) = INDEX{
	      key = require (bindAttr (env, attrs, attrKEY)),
	      see = bindAttr (env, attrs, attrSEE),
	      mark = (case bindTokenAttr (attrs, attrMARK, scanIndexMark)
		 of NONE => HERE
		  | SOME m => m
		(* end case *)),
	      which = bindAttr (env, attrs, attrWHICH)
	    }
      fun mkINTERFACE (env, attrs) = INTERFACE{
	      label = bindAttr (env, attrs, attrLABEL)
	    }
      fun mkTopRef cons (env, attrs) = cons{
	      document = bindAttr (env, attrs, attrDOCUMENT),
	      nolink = bindAttrFlag (attrs, attrNOLINK),
	      noindex = bindAttrFlag (attrs, attrNOINDEX)
	    }
      fun mkRef cons (env, attrs) = let
	    val isTop = bindAttrFlag (attrs, attrTOPID)
	    val strId = bindAttr (env, attrs, attrSTRID)
	    val sigId = bindAttr (env, attrs, attrSIGID)
	    val fctId = bindAttr (env, attrs, attrFCTID)
	    in
	      cons{
		  home = (case (isTop, strId, sigId, fctId)
		     of (false, NONE, NONE, NONE) => NONE
		      | (true, NONE, NONE, NONE) => SOME TOPID
		      | (false, SOME id, NONE, NONE) => SOME(STRID id)
		      | (false, NONE, SOME id, NONE) => SOME(SIGID id)
		      | (false, NONE, NONE, SOME id) => SOME(FCTID id)
		      | _ => raise Fail "multiple homes for XREF"
		    (* end case *)),
		  document = bindAttr (env, attrs, attrDOCUMENT),
		  nolink = bindAttrFlag (attrs, attrNOLINK),
		  noindex = bindAttrFlag (attrs, attrNOINDEX)
		}
	    end
      fun mkSECREF (env, attrs) = SECREF{
	      label = require (bindAttr (env, attrs, attrLABEL))
	    }
      fun mkSIGBODY (env, attrs) = SIGBODY{
	      sigid = bindAttr (env, attrs, attrSIGID),
	      file = bindAttr (env, attrs, attrFILE)
	    }
      fun mkSIGINSTANCE (env, attrs) = SIGINSTANCE{
	      status = bindTokenAttr (attrs, attrSTATUS, scanStatus),
	      opaque = bindAttrFlag (attrs, attrOPAQUE)
	    }
      fun mkSIGNATURE (env, attrs) = SIGNATURE{
	      sigid = require (bindAttr (env, attrs, attrSIGID)),
	      status = bindTokenAttr (attrs, attrSTATUS, scanStatus)
	    }
      fun mkSPECBREAK (env, attrs) = SPECBREAK{
	      newline = bindAttrFlag (attrs, attrNEWLINE)
	    }
      fun mkSTRUCTURE (env, attrs) = STRUCTURE{
	      strid = require (bindAttr (env, attrs, attrSTRID)),
	      status = bindTokenAttr (attrs, attrSTATUS, scanStatus)
	    }
      fun mkSECTION (env, attrs) = SECTION{
	      label = bindAttr (env, attrs, attrLABEL),
	      nonumber = bindAttrFlag (attrs, attrNONUMBER),
	      notoc = bindAttrFlag (attrs, attrNOTOC)
	    }
      fun mkSHARING (env, attrs) = SHARING{
	      ty = bindAttrFlag (attrs, attrTYPE)
	    }
      fun mkSUBINDEX (env, attrs) = SUBINDEX{
	      key = require (bindAttr (env, attrs, attrKEY))
	    }
      fun mkSYSNOTE (env, attrs) = SYSNOTE{
	      opsys = bindNames (attrs, attrOPSYS),
	      arch = bindNames (attrs, attrARCH)
	    }
      fun mkTABLE (env, attrs) = TABLE{
	      long = bindAttrFlag (attrs, attrLONG),
	      small = bindAttrFlag (attrs, attrSMALL)
	    }
      fun mkTD (env, attrs) = TD{
	      align = bindTokenAttr (attrs, attrALIGN, scanAlign),
	      colspan = bindIntAttr (attrs, attrCOLSPAN)
	    }
      fun mkTH (env, attrs) = TH{
	      align = bindTokenAttr (attrs, attrALIGN, scanAlign),
	      colspan = bindIntAttr (attrs, attrCOLSPAN)
	    }
      fun mkURL (env, attrs) = URL{
	      href = require (bindAttr (env, attrs, attrHREF))
	    }
      fun mkVERSION (env, attrs) = VERSION{
	      verid = bindAttr (env, attrs, attrVERID),
	      year = require (bindIntAttr (attrs, attrYEAR)),
	      month = bindIntAttr (attrs, attrMONTH),
	      day = bindIntAttr (attrs, attrDAY)
	    }

      val tbl = let
	    val tbl = AtomTable.mkTable (128, Fail "")
	    fun ins (name, f) = AtomTable.insert tbl (name, f)
	    in
	      app ins [
		  (elemADEF, mkADEF),
		  (elemAREF, mkAREF),
		  (elemARG, mk ARG),
		  (elemAUTHOR, mkAUTHOR),
		  (elemBF, mk BF),
		  (elemCAPTION, mk CAPTION),
		  (elemCD, mkCD),
		  (elemCEILING, mk CEILING),
		  (elemCITE, mkCITE),
		  (elemCODE, mkCODE),
		  (elemCOL, mkCOL),
		  (elemCOMMENT, mk COMMENT),
		  (elemCONREF, mkRef CONREF),
		  (elemCONS, mk CONS),
		  (elemCOPYRIGHT, mkCOPYRIGHT),
		  (elemDATATYPE, mkDATATYPE),
		  (elemDATATYPEDEF, mk DATATYPEDEF),
		  (elemDESCRIP, mk DESCRIP),
		  (elemDISPLAYMATH, mk DISPLAYMATH),
		  (elemDOCREF, mkDOCREF),
		  (elemDTAG, mk DTAG),
		  (elemEM, mk EM),
		  (elemENUM, mk ENUM),
		  (elemEQN, mk EQN),
		  (elemEQNARRAY, mk EQNARRAY),
		  (elemEQNREL, mk EQNREL),
		  (elemEQTYPE, mk EQTYPE),
		  (elemEQU, mk EQU),
		  (elemEVALTO, mk EVALTO),
		  (elemEXAMPLE, mk EXAMPLE),
		  (elemEXN, mk EXN),
		  (elemEXNREF, mkRef EXNREF),
		  (elemFCTARGREF, mkTopRef FCTARGREF),
		  (elemFCTREF, mkTopRef FCTREF),
		  (elemFIGURE, mkFIGURE),
		  (elemFLOAT, mkFLOAT),
		  (elemFLOATREF, mkFLOATREF),
		  (elemFLOOR, mk FLOOR),
		  (elemFRAC, mk FRAC),
		  (elemFUNCTOR, mkFUNCTOR),
		  (elemGRAM_ALT, mk GRAM_ALT),
		  (elemGRAM_CSET, mk GRAM_CSET),
		  (elemGRAM_GRP, mkGRAM_GRP),
		  (elemGRAM_KW, mk GRAM_KW),
		  (elemGRAM_LIT, mk GRAM_LIT),
		  (elemGRAM_NONTERM, mkGramSymb GRAM_NONTERM),
		  (elemGRAM_PROD, mkGRAM_PROD),
		  (elemGRAM_RANGE, mk GRAM_RANGE),
		  (elemGRAM_RHS, mk GRAM_RHS),
		  (elemGRAM_SEP, mk GRAM_SEP),
		  (elemGRAM_TERM, mkGramSymb GRAM_TERM),
		  (elemGRAMMAR, mk GRAMMAR),
		  (elemHEAD, mk HEAD),
		  (elemID, mk ID),
		  (elemIDREF, mkIDREF),
		  (elemIMPLNOTE, mk IMPLNOTE),
		  (elemINCLFILE, mkINCLFILE),
		  (elemINCLUDE, mk INCLUDE),
		  (elemINDEX, mkINDEX),
		  (elemINTERFACE, mkINTERFACE),
		  (elemINTERSECT, mk INTERSECT),
		  (elemIT, mk IT),
		  (elemITEM, mk ITEM),
		  (elemITEMIZE, mk ITEMIZE),
		  (elemKEY_VIEW, mk KEY_VIEW),
		  (elemKW, mk KW),
		  (elemLL, mk LL),
		  (elemMATH, mk MATH),
		  (elemMGROUP, mk MGROUP),
		  (elemML_DOC, mk ML_DOC),
		  (elemMOD, mk MOD),
		  (elemMTEXT, mk MTEXT),
		  (elemNORM, mk NORM),
		  (elemOPAQUE, mk OPAQUE),
		  (elemOPD, mk OPD),
		  (elemOVER, mk OVER),
		  (elemPP, mk PP),
		  (elemPROD, mk PROD),
		  (elemPROTO, mk PROTO),
		  (elemPROTOTY, mk PROTOTY),
		  (elemQUESTION, mk QUESTION),
		  (elemRAISES, mk RAISES),
		  (elemRATIONALE, mk RATIONALE),
		  (elemRE, mk RE),
		  (elemREGEXP, mk REGEXP),
		  (elemSECREF, mkSECREF),
		  (elemSECTION, mkSECTION),
		  (elemSEEALSO, mk SEEALSO),
		  (elemSET, mk SET),
		  (elemSHARING, mkSHARING),
		  (elemSIGBODY, mkSIGBODY),
		  (elemSIGINSTANCE, mkSIGINSTANCE),
		  (elemSIGNATURE, mkSIGNATURE),
		  (elemSIGREF, mkTopRef SIGREF),
		  (elemSPEC, mk SPEC),
		  (elemSPECBREAK, mkSPECBREAK),
		  (elemSTRREF, mkRef STRREF),
		  (elemSTRUCTURE, mkSTRUCTURE),
		  (elemSUB, mk SUB),
		  (elemSUBINDEX, mkSUBINDEX),
		  (elemSUBSTRUCT, mk SUBSTRUCT),
		  (elemSUM, mk SUM),
		  (elemSUP, mk SUP),
		  (elemSYSNOTE, mkSYSNOTE),
		  (elemTABLE, mkTABLE),
		  (elemTD, mkTD),
		  (elemTH, mkTH),
		  (elemTITLE, mk TITLE),
		  (elemTR, mk TR),
		  (elemTT, mk TT),
		  (elemTY, mk TY),
		  (elemTYPARAM, mk TYPARAM),
		  (elemTYPE, mk TYPE),
		  (elemTYREF, mkRef TYREF),
		  (elemUL, mk UL),
		  (elemUNION, mk UNION),
		  (elemURL, mkURL),
		  (elemVAL, mk VAL),
		  (elemVALREF, mkRef VALREF),
		  (elemVERSION, mkVERSION),
		  (elemWHERETYPE, mk WHERETYPE)
		];
	      tbl
	    end
    in

(*
    fun mkElement (elemName, attrList) = (AtomTable.lookup tbl elemName) attrList
*)
    fun mkElement env (elemName, attrList) = (
	  case (AtomTable.find tbl elemName)
	   of (SOME f) => f (env, attrList)
	    | NONE =>
		raise Fail(concat["mkElement \"", Atom.toString elemName, "\""])
	  (* end case *))

    fun preserveWS (CODE _) = true
      | preserveWS (CD _) = true
      | preserveWS _ = false

    end (* local *)

  end

