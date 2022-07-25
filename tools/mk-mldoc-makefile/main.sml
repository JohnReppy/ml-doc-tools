(* main.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    structure F = Format
    structure P = OS.Path

    val usageMsg = "\
	\usage: mk-mldoc-makefile [options] file\n\
	\\t-help           print this message\n\
	\\t-bin dir        specify bin directory for ML-Doc tools\n\
	\\t-html dir       specify output directory for HTML files\n\
	\\t-info dir       specify output directory for info files\n\
	\\t-latex dir      specify output directory for LaTeX files\n\
	\\t-proof dir      specify output directory for proof LaTeX files\n\
	\\t-root file      specify root for LaTeX document\n\
	\"

    val binDir = BinDir.binDir
    val srcDir = ref "ML-Doc"
    val htmlDir = ref "HTML"
    val infoDir = ref "Info"
    val latexDir = ref "Hardcopy"
    val proofDir = ref "Proof"
    val makefile = ref "Makefile"
    val sigsDir = ref "Sigs"
    val htmlTemplate = ref ""
    val rootLatexFile = ref(NONE : string option)
    val indexKinds = ref ["all"]

    fun doOpts ("-help" :: r) = (
	  TextIO.output(TextIO.stdErr, usageMsg);
	  OS.Process.exit OS.Process.success)
      | doOpts ("-bin" :: dir :: r) = (binDir := dir; doOpts r)
      | doOpts ("-html" :: dir :: r) = (htmlDir := dir; doOpts r)
      | doOpts ("-info" :: dir :: r) = (infoDir := dir; doOpts r)
      | doOpts ("-latex" :: dir :: r) = (latexDir := dir; doOpts r)
      | doOpts ("-proof" :: dir :: r) = (proofDir := dir; doOpts r)
      | doOpts ("-sigs" :: dir :: r) = (sigsDir := dir; doOpts r)
      | doOpts ("-template" :: file :: r) = (htmlTemplate := file; doOpts r)
      | doOpts ("-root" :: file :: r) = (rootLatexFile := SOME file; doOpts r)
      | doOpts [] = NONE
      | doOpts [file] = SOME file
      | doOpts _ = (
	  TextIO.output(TextIO.stdErr, "mk-mldoc-makefile: bad options\n");
	  TextIO.output(TextIO.stdErr, usageMsg);
	  OS.Process.exit OS.Process.failure)

  (* get the list of input files.  Input files are specified one to a line
   * and may have an optional "!html" or "!tex" to mark them as not part
   * of the HTML (resp. LaTeX version).
   *)
    fun getInputs fileName = let
	  fun error msg = (
		TextIO.output(TextIO.stdErr,
		  Format.format "mk-mldoc-makefile: %s\n" [F.STR msg]);
		OS.Process.exit OS.Process.failure)
	  fun getInputList inStrm = let
		fun stripFile f = let
		      val {dir, file} = P.splitDirFile f
		      val dir = (case P.fromString dir
			     of {isAbs=true, ...} =>
				  error ("unexpected absolute path")
			      | {arcs=[srcDir], ...} => ""
			      | {arcs=srcDir::rest, ...} =>
				  P.toString{isAbs=false, arcs=rest, vol=""} ^ "/"
			    (* end case *))
		      in
			case P.splitBaseExt file
			 of {base="", ...} => error "bad filename"
			  | {ext=NONE, ...} => error "bad file extension"
			  | {base, ext=SOME "mldoc"} => {path = dir, file = base}
			(* end case *)
		      end
		fun get (htmlFiles, texFiles, files) = (
		      case TextIO.inputLine inStrm
		       of NONE => (rev htmlFiles, rev texFiles, rev files)
			| SOME f => if (String.sub(f, 0) = #"#")
			    then get(htmlFiles, texFiles, files)
			    else let
			      fun next (f, isHTML, isTeX) = let
				    val f = stripFile f
				    in
				      get (
					if isHTML then f::htmlFiles else htmlFiles,
					if isTeX then f::texFiles else texFiles,
					f::files)
				    end
			      in
				case (String.tokens Char.isSpace f)
				 of [] => get(htmlFiles, texFiles, files)
				  | [f] => next (f, true, true)
				  | [f, "!html"] => next (f, false, true)
				  | [f, "!tex"] => next (f, true, false)
				  | _ => error "bogus input line"
				(* end case *)
			      end
		      (* end case *))
		in
		  get ([], [], [])
		end
	  in
	    case fileName
	     of NONE => getInputList TextIO.stdIn
	      | (SOME s) => let
		  val inStrm = TextIO.openIn s
		  in
		    getInputList inStrm before TextIO.closeIn inStrm
		  end
	    (* end case *)
	  end

    fun pr (strm, s) = TextIO.output(strm, s)
    fun prl (strm, sl) = pr(strm, concat sl)
    fun prf (strm, fmt, items) = pr(strm, F.format fmt items)

    fun emitHdr strm = (
	  pr  (strm, "# Generated makefile\n");
	  pr  (strm, "#\n");
	  pr  (strm, "\n");
	  prf (strm, "MLDOC_BIN = %s\n", [F.STR(!binDir)]);
	  pr  (strm, "\n");
	  pr  (strm, "EXTRACT_INFO = $(MLDOC_BIN)/extract-info\n");
	  pr  (strm, "HTML_GEN = $(MLDOC_BIN)/html-gen\n");
	  pr  (strm, "HTML_INDEX = $(MLDOC_BIN)/html-index\n");
	  pr  (strm, "HTML_TOC = $(MLDOC_BIN)/html-toc\n");
	  pr  (strm, "LATEX_GEN = $(MLDOC_BIN)/latex-gen\n");
	  pr  (strm, "MERGE_INFO = $(MLDOC_BIN)/merge-info\n");
	  pr  (strm, "PROOF_LATEX = $(MLDOC_BIN)/proof-latex\n");
	  pr  (strm, "RUN_LATEX = $(MLDOC_BIN)/run-latex\n");
	  pr  (strm, "DVIPS = dvips\n");
	  pr  (strm, "\n");
	  prf (strm, "SRC_DIR = %s\n", [F.STR(!srcDir)]);
	  prf (strm, "INFO_DIR = %s\n", [F.STR(!infoDir)]);
	  prf (strm, "HTML_DIR = %s\n", [F.STR(!htmlDir)]);
	  prf (strm, "LATEX_DIR = %s\n", [F.STR(!latexDir)]);
	  prf (strm, "PROOF_DIR = %s\n", [F.STR(!proofDir)]);
	  pr  (strm, "\n");
	  pr  (strm, "CONFIG = Config.cfg CATALOG\n");
	  pr  (strm, "HTML_INFO = $(CONFIG) $(INFO_DIR)/HTML.info\n");
	  pr  (strm, "LATEX_INFO = $(CONFIG) $(INFO_DIR)/LaTeX.info\n");
	  pr  (strm, "\n"))

    fun emitFileList {strm, name, dir, files, ext} = let
	  fun filename {path, file} = OS.Path.joinDirFile{
		  dir = OS.Path.concat(dir, path),
		  file = OS.Path.joinBaseExt{base=file, ext=SOME ext}
		}
	  fun emit [] =  pr(strm, "\n")
	    | emit [f] = prf(strm, "\t%s\n", [F.STR(filename f)])
	    | emit (f::r) = (
		prf(strm, "\t%s \\\n", [F.STR(filename f)]);
		emit r)
	  in
	    prf (strm, "%s =", [F.STR name]);
	    emit files
	  end

    fun emitInfoRule strm {path, file} = (
	  prf (strm, "$(INFO_DIR)/%s%s.info:\t$(SRC_DIR)/%s%s.mldoc $(CONFIG)\n",
	    [F.STR path, F.STR file, F.STR path, F.STR file]);
	  prf (strm, "\t$(EXTRACT_INFO) $(SRC_DIR)/%s%s.mldoc\n",
	    [F.STR path, F.STR file]);
	  pr(strm, "\n"))

    fun emitHTMLRule strm {path, file} = (
	  prf (strm,
	    "$(HTML_DIR)/%s%s.html:\t$(SRC_DIR)/%s%s.mldoc $(HTML_INFO) \
            \page.template\n",
	    [F.STR path, F.STR file, F.STR path, F.STR file]);
	  prf (strm, "\t$(HTML_GEN) $(SRC_DIR)/%s%s.mldoc\n",
	    [F.STR path, F.STR file]);
	  pr(strm, "\n"))

    fun emitHTMLIndexRule strm kind = (
	  prf (strm,
	    "$(HTML_DIR)/index-%s.html:\t$(HTML_INFO) index.template\n",
	    [F.STR kind]);
	  prf (strm, "\t$(HTML_INDEX) -%s\n", [F.STR kind]);
	  pr (strm, "\n"))

    fun emitHTMLTOCRule strm = (
	  pr (strm, "$(HTML_DIR)/toc.html:\t$(HTML_INFO) toc.template\n");
	  pr (strm, "\t$(HTML_TOC)\n");
	  pr (strm, "\n"))

    fun emitLaTeXRule strm {path, file} = (
	  prf (strm,
	    "$(LATEX_DIR)/%s%s.tex:\t$(SRC_DIR)/%s%s.mldoc $(LATEX_INFO)\n",
	    [F.STR path, F.STR file, F.STR path, F.STR file]);
	  prf (strm, "\t$(LATEX_GEN) $(SRC_DIR)/%s%s.mldoc\n",
	    [F.STR path, F.STR file]);
	  pr(strm, "\n"))

    fun emitProofRule strm {path, file} = (
	  prf (strm, "$(PROOF_DIR)/%s%s.tex:\t$(SRC_DIR)/%s%s.mldoc $(LATEX_INFO)\n",
	    [F.STR path, F.STR file, F.STR path, F.STR file]);
	  prf (strm, "\t$(PROOF_LATEX) $(SRC_DIR)/%s%s.mldoc\n",
	    [F.STR path, F.STR file]);
	  pr(strm, "\n"))

    fun emitCleanTeXRule (outStrm, target, files, dir) = (
	  prf (outStrm, "%s:\n", [F.STR target]);
	  prf (outStrm, "\trm -f $(%s)\n", [F.STR files]);
	  prf (outStrm, "\trm -f $(%s)/*.aux\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.bbl\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.blg\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.dvi\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.idx\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.ilg\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.ind\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.rnd\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.nnd\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.log\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.ps\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.pdf\n", [F.STR dir]);
	  prf (outStrm, "\trm -f $(%s)/*.toc\n", [F.STR dir]);
	  pr  (outStrm, "\n"))

    fun main (_, args) = let
	  val (htmlInputs, texInputs, allInputs) = getInputs(doOpts args)
	  val htmlIndexFiles =
		map (fn k => {path="", file="index-"^k}) (!indexKinds)
	  val outStrm = TextIO.openOut(!makefile)
	  in
	    emitHdr outStrm;

	    emitFileList {
		strm = outStrm, name = "HTML_INFO_FILES", dir = "$(INFO_DIR)",
		files = htmlInputs, ext = "info"
	      };
	    pr (outStrm, "\n");

	    emitFileList {
		strm = outStrm, name = "LATEX_INFO_FILES", dir = "$(INFO_DIR)",
		files = texInputs, ext = "info"
	      };
	    pr (outStrm, "\n");

	    emitFileList {
		strm = outStrm, name = "HTML_FILES", dir = "$(HTML_DIR)",
		files = htmlIndexFiles @ [{path="", file="toc"}] @ htmlInputs,
		ext = "html"
	      };
	    pr (outStrm, "\n");

	    emitFileList {
		strm = outStrm, name = "LATEX_FILES", dir = "$(LATEX_DIR)",
		files = texInputs, ext = "tex"
	      };
	    pr (outStrm, "\n");

	    emitFileList {
		strm = outStrm, name = "PROOF_FILES", dir = "$(PROOF_DIR)",
		files = allInputs, ext = "tex"
	      };
	    pr (outStrm, "\n");

	    pr (outStrm, "HTML:\t$(HTML_FILES)\n");
	    pr (outStrm, "\n");
	    emitHTMLTOCRule outStrm;
	    List.app (emitHTMLIndexRule outStrm) (!indexKinds);
	    List.app (emitHTMLRule outStrm) htmlInputs;

	    pr (outStrm, "$(INFO_DIR)/HTML.info:\t$(HTML_INFO_FILES)\n");
	    pr (outStrm, "\t$(MERGE_INFO) -o $(INFO_DIR)/HTML.info $(HTML_INFO_FILES)\n");
	    pr (outStrm, "\n");
	    pr (outStrm, "$(INFO_DIR)/LaTeX.info:\t$(LATEX_INFO_FILES)\n");
	    pr (outStrm, "\t$(MERGE_INFO) -o $(INFO_DIR)/LaTeX.info $(LATEX_INFO_FILES)\n");
	    pr (outStrm, "\n");
	    List.app (emitInfoRule outStrm) allInputs;

	    case !rootLatexFile
	     of (SOME f) => (
		  prf (outStrm, "Hardcopy:\t$(LATEX_DIR)/%s.ps\n\n", [F.STR f]);
		  prf (outStrm, "$(LATEX_DIR)/%s.ps:\t$(LATEX_FILES)\n", [F.STR f]);
		  prf (outStrm,
		    "\t(cd $(LATEX_DIR); $(RUN_LATEX) %s; $(RUN_LATEX) %s; \
		      \$(DVIPS) %s)\n",
		    [F.STR f, F.STR f, F.STR f]))
	      | _ => pr (outStrm, "Hardcopy:\t$(LATEX_FILES)\n")
	    (* end case *);
	    pr (outStrm, "\n");
	    List.app (emitLaTeXRule outStrm) texInputs;

	    pr (outStrm, "Proof:\t$(PROOF_FILES)\n");
	    pr (outStrm, "\n");
	    List.app (emitProofRule outStrm) allInputs;

	    pr (outStrm, "clean-info:\n");
	    pr (outStrm, "\trm -f $(INFO_DIR)/*.info\n");
	    pr (outStrm, "\n");
	    emitCleanTeXRule (outStrm, "clean-latex", "LATEX_FILES", "LATEX_DIR");
	    emitCleanTeXRule (outStrm, "clean-proof", "PROOF_FILES", "PROOF_DIR");
	    pr (outStrm, "clean-html:\n");
	    pr (outStrm, "\trm -f $(HTML_FILES)\n");
	    pr (outStrm, "\n");
	    pr (outStrm, "clean:\tclean-html clean-info clean-latex clean-proof\n");
	    pr (outStrm, "\n");

	    TextIO.closeOut outStrm;
	    OS.Process.success
	  end

  end;

