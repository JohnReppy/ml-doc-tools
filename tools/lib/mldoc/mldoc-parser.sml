(* mldoc-parser.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure MLDocMarkup = MarkupFn(MLDocElem);
structure MLDocParser = ParseSGMLS(MLDocMarkup);

