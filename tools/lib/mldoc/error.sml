(* error.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Error : sig

    val error  : string -> 'a
    val error' : string list -> 'a

    val uncaughtExn : exn -> unit

    val bogusMarkupList : (string * MLDocMarkup.markup list) -> 'a
    val bogusMarkup : (string * MLDocMarkup.markup) -> 'a
    val bogusElem : (string * MLDocElem.element) -> 'a

    val warning : (string * Format.fmt_item list) -> unit

  end = struct

    structure F = Format

    fun prError msg = TextIO.output(TextIO.stdErr, msg)

    fun error msg = raise Fail msg
    fun error' msgs = raise Fail(concat msgs)

    fun uncaughtExn exn = let
	  val trace = rev(SMLofNJ.exnHistory exn)
	  in
	    case exn
	     of Fail msg =>
		  prError(F.format "Uncaught exception Fail \"%s\"\n" [F.STR msg])
	      | IO.Io{function, name, ...} =>
		  prError(F.format "Uncaught exception Io{%s, %s}\n"
		    [F.STR name, F.STR function])
	      | OS.SysErr(msg, _) =>
		  prError(F.format "Uncaught exception SysErr \"%s\"\n" [F.STR msg])
	      | exn =>
		  prError(F.format "Uncaught exception %s\n"
		    [F.STR(exnName exn)])
	    (* end case *);
	    List.app (fn msg => prError(concat["  ** ", msg, "\n"])) trace;
	    TextIO.flushOut TextIO.stdErr
	  end

    fun bogusMarkup (expected, elem) = let
	  val msg = F.format "bogus %s, found %s" [
		  F.STR expected, F.STR(PrintMLDoc.toString elem)
		]
	  in
	    error msg
	  end

    fun bogusMarkupList (expected, []) =
	  error (F.format "bogus %s, found nothing" [F.STR expected])
      | bogusMarkupList (expected, markup::_) = bogusMarkup(expected, markup)

    fun bogusElem (expected, elem) = let
	  val msg = F.format "bogus %s, found %s" [
		  F.STR expected, F.STR(MLDocElem.elemName elem)
		]
	  in
	    error msg
	  end

    fun warning (fmt, items) = (
	  prError "Warning: ";
	  prError(F.format fmt items);
	  prError "\n")

  end;

