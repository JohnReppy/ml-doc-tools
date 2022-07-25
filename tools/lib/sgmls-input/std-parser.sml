(* std-parser.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure StdMarkup = MarkupFn(StdElem);
structure StdParser = ParseSGMLS(StdMarkup);

