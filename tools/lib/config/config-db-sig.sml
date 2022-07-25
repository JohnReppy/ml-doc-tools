(* config-db-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * This module provides support for configuration databases, including
 * support for parsing a configuration file.
 *
 * TODO:
 *  - support for \ at end of line
 *)

signature CONFIG_DB =
  sig

    type config_db

    val mkConfigDB     : unit -> config_db
    val copyConfigDB   : config_db -> config_db
    val mergeConfigDBs : (config_db * config_db) -> config_db

    val dump : (TextIO.outstream * config_db) -> unit

  (* open and parse a configuration file, returning a list of name
   * value pairs.
   *)
    val parse : string -> config_db

    datatype value
      = ATOM of Atom.atom
      | STR of string
      | NUM of int
      | BOOL of bool
      | LIST of value list
      | TBL of config_db

    val find  : (config_db * Atom.atom list) -> value option
    val find' : (config_db * string list) -> value option
    val add   : (config_db * Atom.atom list * value * bool) -> unit
    val add'  : (config_db * string list * value * bool) -> unit

  end;
