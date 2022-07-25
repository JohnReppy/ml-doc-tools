(* text-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * An abstract two-level representation of text.  Text is organized into
 * blocks and text.
 *)

signature TEXT =
  sig
    type context
    type block
    type text

(* do we need types for list items, table cells, etc? *)

    val chapter    : context -> {title : text, body : block} -> block
    val section    : context -> {title : text, body : block} -> block
    val subsection : context -> {title : text, body : block} -> block
    val paragraph  : context -> text -> block

    val blockList : context -> block list -> block

    val it : context -> text -> text
    val bf : context -> text -> text
    val tt : context -> text -> text
    val textList : context -> text list -> text

  end;
