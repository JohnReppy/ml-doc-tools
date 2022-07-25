(* regexp-sig.sml
 *
 * COPYRIGHT (c) 1991 Cornell University
 *
 * AUTHOR: William Aitken
 *         Cornell University
 *         Ithaca, NY 14853
 *)

signature REGEXP =
  sig

    datatype reSyntax = 
	Backslash | RBracket | LBracket | RParen | Star | Plus | Quest
    datatype reError = 
        RERange of int | RESyntax of reSyntax * int | REEmptyLoop 
      | REBug of int | REMatch
    exception REError of reError 

    type matchdesc
    type re

    val compile : string -> re
    val match : re -> string -> matchdesc
    val getlimits : matchdesc -> int option -> int * int

  end
