(* ml-style-sig.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

signature ML_STYLE =
  sig
    type token
    type style
    type context

    datatype xref_kind
      = SigRef | StrRef | FctRef | ShareRef | TyRef | ExnRef | ConRef | ValRef

    val kwStyle : style
    val punctStyle : style
    val tyvarStyle : style
    val idIdxStyle : style	(* ID-family index *)
    val idStyle : style
    val itStyle : style		(* italicized text style (not TT) *)

  (* extend the context for the body of a nested substructure *)
    val extendContext : (context * string) -> context

  (* identifiers that are possible cross references *)
    val idToToken : context -> MLSpec.tagged_id -> token

  (* identifiers that are forward links to descriptions; this function is used
   * to wrap identifiers in specifications.
   *)
    val descRef : (context * xref_kind * string) -> token

  (* make a token with the given style (to support character escapes) *)
    val mkToken : (style * string) -> token

  end;

