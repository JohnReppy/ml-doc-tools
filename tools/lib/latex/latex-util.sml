(* latex-util.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Utility routines for generating LaTeX output.
 *)

structure LatexUtil : sig

    type strm

    val mkStrm : {say : string -> unit, flush : unit -> unit} -> strm

    val say : (strm * string) -> unit
    val sayf : (strm * string * Format.fmt_item list) -> unit

    val sayCmd0 : strm -> string -> unit
	(* output "\cmd{}" *)
    val sayCmd1 : strm -> (string * string) -> unit
	(* output "\cmd{arg1}" *)
    val sayCmd2 : strm -> (string * string) -> unit
	(* output "\cmd{arg1}{arg2}" *)

    val sayBegin : strm -> string -> unit
    val sayEnd   : strm -> string -> unit
	(* generate \begin{} and \end{} markers *)

    val withEnv : strm -> string -> (strm -> unit) -> unit
	(* "withEnv s e f" is equivalent to "sayBegin s e; f s; sayEnd s e" *)

    val withCmd : strm -> string -> (strm -> unit) -> unit
	(* output "\cmd{...}", where the "..." is produced by the third
	 * argument.
	 *)

    val text : strm -> string -> unit
	(* emit the given text; add escapes for "\", "{", "}", "_", "$"  *)

    val emText : strm -> string -> unit
    val ttText : strm -> string -> unit
    val bfText : strm -> string -> unit
    val itText : strm -> string -> unit
	(* emit the given text in the given font; this also deals with character
	 * escapes.
	 *)

  end = struct

    structure F = Format

    datatype strm = S of {say : string -> unit, flush : unit -> unit}

    val mkStrm = S

    fun say (S{say, ...}, s) = say s
    fun sayf (S{say, ...}, fmt, items) = say(F.format fmt items)

    fun sayCmd0 strm cmd = sayf (strm, "\\%s{}", [F.STR cmd])
    fun sayCmd1 strm (cmd, arg1) =
	  sayf (strm, "\\%s{%s}", [F.STR cmd, F.STR arg1])
    fun sayCmd2 strm (cmd, arg1, arg2) =
	  sayf (strm, "\\%s{%s}{%s}", [F.STR cmd, F.STR arg1, F.STR arg2])

    fun sayBegin strm env =  sayf (strm, "\\begin{%s}\n", [F.STR env])
    fun sayEnd strm env = sayf (strm, "\\end{%s}\n", [F.STR env])

    fun withEnv strm env f = (
	  sayBegin strm env;
	  f strm;
	  sayEnd strm env)

    local
      fun transC #"{" = "\\{"
	| transC #"}" = "\\}"
	| transC #"_" = "\\char`\\_"
	| transC #"\\" = "\\\\"
	| transC #"$" = "\\$"
	| transC #"\t" = " "
	| transC c = String.str c
      val translate = String.translate transC
    in
    fun text (S{say, ...}) s = say (translate s)
    end

    local
      fun highlight style (strm as S{say, ...}) s = (
	    say (F.format "\\%s{}" [F.STR style]);
	    text strm s;
	    say "}")
    in
    val emText = highlight "em"
    val ttText = highlight "tt"
    val bfText = highlight "bf"
    val itText = highlight "it"
    end

  end;

