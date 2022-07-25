(* html-ppspec.sml
 *
 * COPYRIGHT (c) 2022 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * The PPSpecFn specialized to generate HTML trees.
 *)

structure HTMLDev = HTML3Dev

structure Style =
  struct

    local
      structure M = MLDocMarkup
      structure E = MLDocElem
    in

    type style = HTMLDev.style
    datatype token = TOK of {
	sty : style,
	id : string
      }

    type context = {inSpec : bool, ctx : HTMLContext.context}

    datatype xref_kind = datatype HRefs.xref_kind

    val kwStyle = HTMLDev.styleB
    val punctStyle = HTMLDev.styleNONE
    val tyvarStyle = HTMLDev.styleI
    val idStyle = HTMLDev.styleNONE
    val idIdxStyle = HTMLDev.styleI
    val itStyle = HTMLDev.styleEM

    fun extendContext ({inSpec, ctx}, id) =
	  {inSpec = inSpec, ctx = HTMLContext.withSubstr (ctx, id)}

  (* identifiers that are possible cross references *)
    fun idToToken {inSpec, ctx} = let
	  val mkURL = HRefs.xrefURL ctx
	  fun toToken (MLSpec.ID id) = TOK{id = id, sty = idStyle}
	    | toToken (MLSpec.TAG(M.ELEM{elem, body=[M.DATA id], ...})) = TOK{
(* need to translate text here? *)
		  sty = case mkURL{xref = elem, id = id}
		       of (SOME tag) => HTMLDev.link tag
			| NONE => idStyle
		      (* end case *),
		  id = id
		}
	  in
	    toToken
	  end

(* NOTE: we also need to put a specification anchor here! *)
  (* identifiers that are forward links to descriptions *)
    fun descRef ({inSpec = true, ctx}, kind, id) = TOK{
	    sty = HTMLDev.linkAnchor{
		href = HRefs.descURL ctx {isRef = true, kind = kind, id = id},
		name = HRefs.specURL ctx {isRef = false, kind = kind, id = id}
	      },
	    id = id
	  }
      | descRef (_, _, id) = TOK{sty=idStyle, id=id}

    fun mkToken (sty, s) = TOK{id = s, sty = sty}

    end (* local *)
  end;

structure Token : PP_TOKEN =
  struct
    structure Sp = MLSpec
    structure E = MLDocElem
    structure M = MLDocParser.Markup

    type style = Style.style
    type token = Style.token

    fun string (Style.TOK{id, ...}) = id

    fun style (Style.TOK{sty, ...}) = sty

(* if the text has been translated, then this will be wrong! *)
    fun size (Style.TOK{id, ...}) = String.size id

  end;

structure HTMLDev' =
  struct
    open HTMLDev
    fun string (dev, s) = HTMLDev.string(dev, TextToHTML.transData s)
  end;

structure HTMLPPStrm = PPStreamFn (
  structure Token = Token
  structure Device = HTMLDev');

structure HTMLPPSpec = PPSpecFn(
  structure PPStrm = HTMLPPStrm
  structure Style = Style
  fun specBreak _ _ = ());

