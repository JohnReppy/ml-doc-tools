(* markup-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * A simple tree representation of document markup
 *)

signature MARKUP =
  sig

    structure Elem : ELEM

    datatype markup
      = ELEM of {
	  elem : Elem.element,
	  body : markup list,
	  pos : (string * int * int)
	}
      | DATA of string

    type iter
    val iterate     : markup -> iter
    val iterateList : markup list -> iter

    val next : iter -> (markup * iter) option
	(* return the next markup element in the pre-order traversal of the
	 * markup tree, or NONE at the end of the traversal.
	 *)
    val next' : iter -> (markup * iter)
	(* return the next markup element in the pre-order traversal of the
	 * markup tree.  Raises Fail, if the end of the traversal is
	 * encountered.
	 *)
    val right : iter -> iter
	(* advance the iterator to the next markup element to the right.  *)
    val cut : iter -> iter
	(* return an iterator that will terminate with the current node. *)

    val transData : {
	    escape : char -> string,
	    sdata : Substring.substring -> string,
	    special : char -> string option
	  } -> string -> string
	(* given a collection of translation functions, translate the contents
	 * of a DATA element.  The translation functions are:
	 *   escape	-- used to map escape sequences (e.g., "\n", "\011", "\\")
	 *		   to strings.  The argument is character specified by
	 *		   the escape sequence.
	 *   sdata	-- used to map SDATA entities to strings.
	 *   special	-- used to map other characters that might be special
	 *		   in the target language (e.g., #"{" ==> SOME "\{" for
	 *		   LaTeX).
	 *)

    val stripData : {
	    escape : char -> string,
	    sdata : Substring.substring -> string,
	    special : char -> string option
	  } -> string -> string
	(* like transData, but it also strips leading and trailing white space
	 * from the result.
	 *)

  end;

