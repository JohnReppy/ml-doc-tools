(* token-scanner-sig.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

signature TOKEN_SCANNER =
  sig

    type state

    val initial : state

    val addString : (state * string) -> state
    val getc : state -> (char * state) option
    val scan : state -> (MLDocElem.element option * substring * state) option

  end
