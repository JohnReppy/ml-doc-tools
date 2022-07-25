(* latex-translate.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Translating PCDATA to LaTeX strings
 *)

structure LaTeXTranslate =
  struct

    local
      fun special #"{" = SOME "\\{"
	| special #"}" = SOME "\\}"
	| special #"_" = SOME "\\char`\\_"
	| special #"#" = SOME "\\#"
	| special #"^" = SOME "\\texttt{\\mldHat{}}"
	| special #"%" = SOME "\\%" 
	| special #"~" = SOME "\\mldTilde{}"
	| special #"$" = SOME "\\$"
	| special #"<" = SOME "\\ensuremath{<}"
	| special #">" = SOME "\\ensuremath{>}"
	| special _ = NONE
      fun escape #"\t" = " "
	| escape #"\n" = " "
	| escape #"\\" = "\\ensuremath{\\backslash}"
	| escape c = String.str c
    in
    val transData = MLDocMarkup.transData {
	    escape = escape,
	    sdata = EntitiesToLaTeX.transEntity,
	    special = special
	  }
    end (* local *)

  (* character translation for \mldCD{} and mldCODE environments *)
    local
      fun special #"{" = SOME "\\{"
	| special #"}" = SOME "\\}"
	| special #"#" = SOME "\\#"
	| special #"%" = SOME "\\%" 
	| special _ = NONE
      fun escape #"\\" = "\\\\"
	| escape c = String.str c
    in
    val transCodeData = MLDocMarkup.transData {
	    escape = escape,
	    sdata = EntitiesToCodeLaTeX.transEntity,
	    special = special
	  }
    end (* local *)

  (* character translation for \mldPROTO{} ant \texttt{} *)
    local
      fun special #"{" = SOME "\\char`\\{"
	| special #"}" = SOME "\\char`\\}"
	| special #"_" = SOME "\\mldUS{}"
	| special #"#" = SOME "\\#"
	| special #"%" = SOME "\\%" 
	| special #"~" = SOME "\\mldTilde{}"
	| special #"$" = SOME "\\$"
	| special #"^" = SOME "\\mldHat{}"
	| special _ = NONE
      fun escape #"\\" = "\\mldBS{}"
	| escape c = String.str c
    in
    val transProtoData = MLDocMarkup.transData {
	    escape = escape,
	    sdata = EntitiesToCodeLaTeX.transEntity,
	    special = special
	  }
    end (* local *)

  (* translate index strings for the rhs of the "@" (the literal presentation) *)
    val transIndexView = MLDocMarkup.transData {
	    escape = fn #"\\" => "\\\\" | c => String.str c,
	    sdata = EntitiesToLaTeX.transEntity,
	    special =
		fn #"!" => SOME "\"!"
		 | #"@" => SOME "\"@"
		 | #"\"" => SOME "\"\""
		 | #"|" => SOME "\"|"
		 | #"%" => SOME "\\%"
		 | #"$" => SOME "\\$"
		 | #"_" => SOME "\\mldUS{}"
		 | #"^" => SOME "\\mldHat{}"
		 | #"~" => SOME "\\mldTilde{}"
		 | _ => NONE
	  }

    fun transLabelData id = String.map
	  (fn #"_" => #"-"
	    | #"{" => #"<"
	    | #"}" => #">"
	    | c => c
	  ) id

  end
