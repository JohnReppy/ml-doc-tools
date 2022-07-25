(* print-index.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure PrintIndex : sig

    val printIndex : {
	    say: string -> unit, flush: unit -> unit
	  } -> IndexRep.index -> unit

  end = struct

    structure IR = IndexRep
    structure F = Format

    fun printIndex {say, flush} index = let
	  fun sayIndent indent = let
		val indentStr = StringCvt.padLeft #" " indent ""
		in
		  fn () => say indentStr
		end
	  val IR.IDX{fileTbl, anchorTbl, labelTbl, sigTbl, strTbl, fctTbl, ...} =
		index
	  fun sayf (fmt, args) = say(F.format fmt args)
	  fun fmtFile (IR.FILEentry{name, ...}) = F.ATOM name
	  fun fmtFileOfEnv (IR.ENV{file, ...}) = fmtFile file
	  fun fmtSigId (IR.SIGentry{id, ...}) = F.ATOM id
	  fun tblApp f tbl = AtomTable.app f tbl
	  fun sayOptList (tbl, header, sayFn) =
		if (AtomTable.numItems tbl = 0)
		  then ()
		  else (sayf("%s\n", [F.STR header]); tblApp sayFn tbl)
	  fun sayFile (IR.FILEentry{name, isEmpty, title, content}) = let
		fun sayItem indent item = (
		      sayIndent indent ();
		      case item
		       of (IR.SECTION{title, label=SOME lab, content}) => (
			    sayf("@SECTION \"%s\" \"%s\" [",
			      [F.STR title, F.ATOM lab]);
			    sayContents (indent, content))
			| (IR.SECTION{title, content, ...}) => (
			    sayf("@SECTION \"%s\" [", [F.STR title]);
			    sayContents (indent, content))
			| (IR.INCLFILE f) => sayf (" @FILE \"%s\"\n", [F.ATOM f])
		      (* end case *))
		and sayContents (_, []) = say "]\n"
		  | sayContents (indent, content) = (
		      say "\n";
		      List.app (sayItem (indent+2)) content;
		      sayIndent indent (); say "]\n")
		in
		  sayf ("  [%s\"%s\"; \"%s\";", [
		      if isEmpty then F.STR "@EMPTY " else F.STR "",
		      F.ATOM name, F.STR title
		    ]);
		  sayContents(2, !content)
		end
	  fun sayAnchor (IR.ANCHORentry{tag, file}) =
		sayf ("  [\"%s\"; \"%s\"]\n", [F.ATOM tag, fmtFile file])
	  fun sayLabel (IR.LABELentry{name, kind, file}) = let
		val kind = (case kind
		       of IR.Section => "@SECTION"
			| IR.Table => "@TABLE"
			| IR.Figure => "@FIGURE"
			| IR.Code => "@CODE"
			| IR.Interface => "@INTERFACE"
		      (* end case *))
		in
		  sayf ("  [\"%s\"; %s; \"%s\"]\n",
		    [F.ATOM name, F.STR kind, fmtFile file])
		end
	  fun sayEnv (indent, IR.ENV{
		strTbl, exnTbl, tyTbl, valTbl, includes, ...
	      }) = let
		val indentStr = StringCvt.padLeft #" " indent ""
		fun sayIndent () = say indentStr
		fun sayIncl sigEntry = (
		      sayIndent(); sayf("@INCLUDE %s\n", [fmtSigId sigEntry]))
		fun sayStr (IR.STRentry{id, bodySig=IR.NamedSig sigEntry, ...}) = (
		      sayIndent();
		      sayf("@STRUCTURE %s %s\n", [F.ATOM id, fmtSigId sigEntry]))
		  | sayStr (IR.STRentry{id, bodySig=IR.AnonSig env, ...}) = (
		      sayIndent();
		      sayf("@STRUCTURE %s [\n", [F.ATOM id]);
		      sayEnv (indent+2, env);
		      sayIndent(); say "]\n")
		 | sayStr (IR.STRentry{id, bodySig=IR.ExternSig, ...}) = (
		      sayIndent();
		      sayf("@STRUCTURE %s @EXTERNAL\n", [F.ATOM id]))
		fun sayTy (IR.TYentry{id, kind=IR.EQTYPE, ...}) = (
		      sayIndent(); sayf ("  @EQTYPE %s\n", [F.ATOM id]))
		  | sayTy (IR.TYentry{id, kind=IR.TYPE, ...}) = (
		      sayIndent(); sayf ("  @TYPE %s\n", [F.ATOM id]))
		  | sayTy (IR.TYentry{id, kind=IR.DATATYPE cl, ...}) = (
		      sayIndent(); sayf ("  @DATATYPE %s [", [F.ATOM id]);
		      List.app (fn s => sayf(" %s", [F.ATOM s])) cl;
		      say "]\n")
		  | sayTy (IR.TYentry{id, kind=IR.DATATYPEDEF, ...}) = (
		      sayIndent(); sayf ("  @DATATYPE %s\n", [F.ATOM id]))
		fun sayExn (IR.EXNentry{id, ...}) = (
		      sayIndent(); sayf ("  %s\n", [F.ATOM id]))
		fun sayVal (IR.VALentry{id, ...}) = (
		      sayIndent(); sayf ("  %s\n", [F.ATOM id]))
		in
		  List.app sayIncl (!includes);
		  tblApp sayStr strTbl;
		  sayOptList (tyTbl, indentStr ^ "@TYPES ", sayTy);
		  sayOptList (exnTbl, indentStr ^ "@EXCEPTIONS ", sayExn);
		  sayOptList (valTbl, indentStr ^ "@VALS ", sayVal)
		end
	  fun saySig (IR.SIGentry{id, body=ref NONE, instances}) =
		sayf ("@SIGNATURE %s\n", [F.ATOM id])
	    | saySig (IR.SIGentry{id, body=ref(SOME env), instances}) = (
		sayf ("@SIGNATURE %s \"%s\" [\n", [F.ATOM id, fmtFileOfEnv env]);
		sayEnv (2, env);
		say ("]\n"))
	  fun sayEnvOrSig (IR.NamedSig sigEntry) = sayf("%s\n", [fmtSigId sigEntry])
	    | sayEnvOrSig (IR.AnonSig env) = (say "[\n"; sayEnv (2, env); say "]\n")
	    | sayEnvOrSig (IR.ExternSig) = say "@EXTERNAL\n"
	  fun sayStr (IR.STRentry{id, file, bodySig, ...}) = (
		sayf("@STRUCTURE %s \"%s\" ", [F.ATOM id, fmtFile file]);
		sayEnvOrSig bodySig)
	  fun sayFct (IR.FCTentry{id, file, bodySig, argSig}) = (
		sayf("@FUNCTOR %s \"%s\" ", [F.ATOM id, fmtFile file]);
		sayEnvOrSig argSig;
		sayEnvOrSig bodySig)
	  in
	    sayOptList (fileTbl, "@FILES", sayFile);
	    sayOptList (anchorTbl, "@ANCHORS", sayAnchor);
	    sayOptList (labelTbl, "@LABELS", sayLabel);
	    tblApp saySig sigTbl;
	    tblApp sayStr strTbl;
	    tblApp sayFct fctTbl
	  end

  end (* PrintIndex *)
