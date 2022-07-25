(* trim-ws.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * Utility code to trim leading and trailing white space from markup lists.
 *)

structure TrimWS : sig

    val leading  : MLDocMarkup.markup list -> MLDocMarkup.markup list
    val trailing : MLDocMarkup.markup list -> MLDocMarkup.markup list
    val trim     : MLDocMarkup.markup list -> MLDocMarkup.markup list

  end = struct

    structure M = MLDocMarkup
    structure SS = Substring

    val triml = SS.dropl Char.isSpace
    val trimr = SS.dropr Char.isSpace

    fun leading [] = []
      | leading (M.DATA s :: r) = M.DATA(SS.string(triml(SS.full s))) :: r
      | leading (M.ELEM{elem, body, pos} :: r) =
	  M.ELEM{elem=elem, body=leading body, pos=pos} :: r

    fun trailing [] = []
      | trailing [M.DATA s] = [M.DATA(SS.string(trimr(SS.full s)))]
      | trailing (elem::rest) = elem :: trailing rest

    fun trim [] = []
      | trim [M.DATA s] = [M.DATA(SS.string(triml(trimr(SS.full s))))]
      | trim l = trailing(leading l)

  end
