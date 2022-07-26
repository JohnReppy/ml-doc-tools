(* latex-ppspec.sml
 *
 * COPYRIGHT (c) 2022 John Reppy (https://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure LaTeXDev =
  struct

    datatype style = Plain | Keyword | TyVar | Italic | IdIndex

    datatype device = DEV of {
	outStrm : TextIO.outstream,
	lineWid : int option ref,
	styleStk : style list ref
      }

    local
      structure F = Format
      fun fmtStyle Plain = F.STR "texttt"
	| fmtStyle Keyword = F.STR "mldKW"
	| fmtStyle TyVar = F.STR "mldTyvar"
	| fmtStyle Italic = F.STR "textit"
	| fmtStyle IdIndex = F.STR "mldArg"	(* ID-family index *)
      fun pr (outS, s) = TextIO.output(outS, s)
      fun prc (outS, c) = TextIO.output1(outS, c)
      fun prf (outS, fmt, items) = pr(outS, F.format fmt items)
    in

  (***** Style operations *****)

    fun sameStyle (s1 : style, s2) = (s1 = s2)

    fun pushStyle (DEV{styleStk, outStrm, ...}, sty) = let
	  fun openSty () = prf (outStrm, "\\%s{", [fmtStyle sty])
	  in
	    case (!styleStk, sty)
	     of ([], Plain) => ()
	      | ([], _) => openSty()
	      | (curSty::_, _) => if (sameStyle(curSty, sty))
		  then ()
		  else openSty()
	    (* end case *);
	    styleStk := sty :: !styleStk
	  end

    fun popStyle (DEV{styleStk, outStrm, ...}) = (case !styleStk
	   of [] => ()
	    | [Plain] => styleStk := []
	    | [sty] => (pr(outStrm, "}"); styleStk := [])
	    | (sty1::sty2::r) => (
		if (sameStyle(sty1, sty2)) then () else pr(outStrm, "}");
		styleStk := sty2::r)
	  (* end case *))

    fun defaultStyle _ = Plain

  (***** Device properties *****)

  (* maximum printing depth (in terms of boxes) *)
    fun maxDepth _ = NONE
    fun setMaxDepth _ = ()

  (* since the maximum depth is unlimited, we do not need to define ellipses *)
    fun ellipses _ = raise Fail "impossible: LaTeXDev.ellipses"
    fun setEllipses _ = ()
    fun setEllipsesWithSz _ = ()

  (* the width of the device *)
    fun lineWidth (DEV{lineWid, ...}) = !lineWid
    fun setLineWidth (DEV{lineWid, ...}, n) = (lineWid := n)

  (* the suggested maximum width of indentation; `NONE` is interpreted as no limit. *)
    fun maxIndent _ = NONE
    fun setMaxIndent _ = ()

  (* the suggested maximum width of text on a line *)
    fun textWidth _ = NONE
    fun setTextWidth _ = ()

  (***** Output operations *****)

  (* output some number of spaces to the device *)
    fun space (DEV{outStrm, ...}, n) = pr (outStrm, StringCvt.padLeft #" " n "")
    val indent = space
    fun newline (DEV{outStrm, ...}) = prc (outStrm, #"\n")

    fun string (DEV{outStrm, ...}, s) = pr (outStrm, s)

    fun char (DEV{outStrm, ...}, c) = prc (outStrm, c)

    fun flush (DEV{outStrm, ...}) = TextIO.flushOut outStrm

    fun specBreak nl (DEV{outStrm, ...}) =
	  if nl
	    then pr(outStrm, "\\mbox{}\\pagebreak[2]")
	    else pr(outStrm, "\\pagebreak[2]")

    fun openDev {dst, wid} = DEV{
	    outStrm = dst,
	    lineWid = ref(SOME wid),
	    styleStk = ref[]
	  }

    end (* local *)
  end

structure LaTeXStyle =
  struct

    local
      structure M = MLDocMarkup
      structure E = MLDocElem
      structure Idx = LaTeXIndex
    in

  (* NOTE: the following type is not used in LaTeX generation *)
    datatype xref_kind = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

    type style = LaTeXDev.style
    datatype token = TOK of {
	sty : style,
	index : string option,
	id : string
      }

    datatype location = Synop | Spec | Desc
    type context = {loc : location, ctx : LaTeXContext.context}

    val kwStyle = LaTeXDev.Keyword
    val punctStyle = LaTeXDev.Plain
    val tyvarStyle = LaTeXDev.TyVar
    val idStyle = LaTeXDev.Plain
    val idIdxStyle = LaTeXDev.IdIndex
    val itStyle = LaTeXDev.Italic

    fun extendContext ({loc, ctx}, id) =
	  {loc = loc, ctx = LaTeXContext.withSubstr (ctx, id)}

  (* identifiers that are possible cross references *)
    fun idToToken {loc, ctx} = let
	  fun toToken (MLSpec.ID id) = TOK{index = NONE, id = id, sty=idStyle}
	    | toToken (MLSpec.TAG(M.ELEM{elem, body=[M.DATA id], ...})) = let
(* FIXME: do we have to worry about ID families here? *)
		val index = (case elem
		       of (elem as E.SIGREF{noindex=false, ...}) =>
			    SOME(Idx.indexXRef(ctx, elem, id))
			| (elem as E.TYREF{noindex=false, ...}) =>
			    SOME(Idx.indexXRef(ctx, elem, id))
			| (E.SIGREF _) => NONE
			| (E.TYREF _) => NONE
			| _ => Error.bogusElem("SIGREF or TYREF", elem)
		      (* end case *))
		in
		  TOK{index = index, id = id, sty=idStyle}
		end
	  in
	    toToken
	  end

  (* identifiers that are forward links to descriptions; this function is used
   * to wrap identifiers in specifications.
   *)
    fun descRef ({loc, ctx}, xrefKind, id) = let
	  val indexCmd = (case loc
		 of Synop => (fn _ => NONE)
		  | Spec => SOME o Idx.indexSpec
		  | Desc => SOME o Idx.indexDesc
		(* end case *))
	  val idx = (case xrefKind
		 of SigRef => raise Fail "descRef: SigRef unexpected"
		  | StrRef => indexCmd (ctx, Idx.StrRef, id)
		  | FctRef => raise Fail "descRef: FctRef unexpected"
		  | ShareRef => NONE
		  | TyRef => indexCmd (ctx, Idx.TyRef, id)
		  | ExnRef => indexCmd (ctx, Idx.ExnRef, id)
		  | ConRef => indexCmd (ctx, Idx.ConRef, id)
		  | ValRef => indexCmd (ctx, Idx.ValRef, id)
		(* end case *))
	  in
	    TOK{index = idx, id = id, sty=idStyle}
	  end

  (* make a token with the given style (to support character escapes) *)
    fun mkToken (sty, s) = TOK{index = NONE, id = s, sty=sty}

    end (* local *)
  end;

structure LaTeXToken : PP_TOKEN =
  struct
    structure Sp = MLSpec
    structure E = MLDocElem
    structure M = MLDocMarkup

    type style = LaTeXStyle.style
    type token = LaTeXStyle.token

    fun string (LaTeXStyle.TOK{id, index = SOME cmd, ...}) =
	  cmd ^ LaTeXTranslate.transCodeData id
      | string (LaTeXStyle.TOK{id, ...}) = LaTeXTranslate.transCodeData id

    fun style (LaTeXStyle.TOK{sty, ...}) = sty

    fun size (LaTeXStyle.TOK{id, ...}) = String.size id

  end;

structure LaTeXPPStrm = PPStreamFn (
  structure Token = LaTeXToken
  structure Device = LaTeXDev);

structure LaTeXPPSpec = PPSpecFn(
    structure PPStrm = LaTeXPPStrm
    structure Style = LaTeXStyle
    val specBreak = LaTeXDev.specBreak
  )
