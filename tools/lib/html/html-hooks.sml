(* html-hooks.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *	%{filename}	document filename (w/o extension)
 *	%{title}	document title
 *	%{version}	document version
 *	%{date}		document date in "month day, year" format
 *	%{year}		document year (4 digit format)
 *	%{day}		document day
 *	%{month}	document month (as a string)
 *	%{monthnum}	document month (as a number from 1-12)
 *	%{url:parent}	URL of parent document
 *	%{url:root}	URL of document root
 *	%{url:index}	URL of document index
 *	%{url:top}	local URL of the top of the document
 *	%%		"%" sign
 *
 * For example, we might use the following footer:
 *
 *	"<HR>\n\
 *	\[ <A HREF=%{url:top}>Top</A> | <A HREF=%{url:top}>Index</A> ]<BR>\n\
 *	\<HR>\n"
 *)

structure HTMLHooks : sig

    val fmtHook : (MLDocMarkup.markup list * HTMLContext.context) -> string -> string

  end = struct

    structure I = MLDocIndex
    structure C = HTMLContext
    structure SS = Substring
    structure F = Format

    val splitAtPercent = SS.splitl (fn #"%" => false | _ => true)
    val splitAtRCB = SS.splitl (fn #"}" => false | _ => true)

    fun expandNext expandFn ss = (case SS.getc(SS.triml 1 ss)
	   of SOME(#"{", rest) => let
		val (a, b) = splitAtRCB rest
		in
		  if (SS.isEmpty b)
		    then raise Fail "bad hook string"
		    else (SS.all(expandFn(SS.string a)), SS.triml 1 b)
		end
	    | SOME(#"%", rest) => (SS.all "%", rest)
	    | _ => (SS.all "%", SS.triml 1 ss)
	  (* end case *))

    fun translateURL s = let
	  fun trans #" "  = "%20"
	    | trans #"\"" = "%22"
	    | trans #"%"  = "%25"
	    | trans #"&"  = "%26"
	    | trans #"<"  = "%3C"
	    | trans #">"  = "%3E"
	    | trans #"["  = "%5B"
	    | trans #"\\" = "%5C"
	    | trans #"]"  = "%5D"
	    | trans #"^"  = "%5E"
	    | trans #"`"  = "%60"
	    | trans #"{"  = "%7B"
	    | trans #"|"  = "%7C"
	    | trans #"}"  = "%7D"
	    | trans #"~"  = "%7E"
	    | trans c = if (Char.isPrint c)
		then str c
		else F.format "%%%02x" [F.INT(Char.ord c)]
	  in
	    String.translate trans s
	  end

    fun fmtHook (doc, ctx) = let
	  fun transStr s = (TextToHTML.transData s)
	  val {hdr = {title, author, version, copyrights}, ...} =
		MLDocContent.getHeader doc
	  val months = #[
		  "January", "February", "March", "April", 
		  "May", "June", "July", "August",
		  "September", "October", "November", "December"
		]
	  fun fmtVerOpt fmt sel = (case version
		 of (SOME v) => (case (sel v)
		       of (SOME x) => (fmt x)
			| NONE => "??"
		      (* end case *))
		  | NONE => "??"
		(* end case *))
	  fun fmtFileOpt NONE = ""
	    | fmtFileOpt (SOME f) = Atom.toString(I.File.name f) ^ ".html"
	  fun expand "filename" = HTMLContext.srcFile ctx
	    | expand "title" = translateURL(transStr title)
	    | expand "version" = fmtVerOpt transStr #verid
	    | expand "date" = (case version
		 of NONE => "??"
		  | (SOME{year, day=SOME d, month=SOME m, ...}) =>
		      F.format "%s %d, %d" [
			  F.STR(Vector.sub(months, m - 1)), F.INT d, F.INT year
			]
		  | (SOME{year, day=NONE, month=SOME m, ...}) =>
		      F.format "%s %d" [
			  F.STR(Vector.sub(months, m - 1)), F.INT year
			]
		  | (SOME{year, ...}) => Int.toString year
		(* end case *))
	    | expand "year" = (case version
		 of NONE => "??"
		  | (SOME v) => Int.toString(#year v)
		(* end case *))
	    | expand "month" =
		fmtVerOpt (fn m => Vector.sub(months, m - 1)) #month
	    | expand "monthnum" =
		fmtVerOpt Int.toString #month
	    | expand "url:parent" = fmtFileOpt(C.parentFile ctx)
	    | expand "url:root" = fmtFileOpt(C.rootFile ctx)
	    | expand "url:index" = "index.html"
	    | expand "url:top" = F.format "%s.html" [F.STR(HTMLContext.srcFile ctx)]
	    | expand s =
		raise Fail(String.concat["bad hook macro \"", s, "\""])
	  fun fmt (rest, frags) = let
		val (prefix, rest) = splitAtPercent rest
		val frags = prefix :: frags
		in
		  if (SS.isEmpty rest)
		    then SS.concat(List.rev frags)
		    else let val (frag, rest) = expandNext expand rest
		      in
			fmt (rest, frag :: frags)
		      end
		end
	  in
	    fn s => fmt (SS.all s, [])
	  end

  end;
