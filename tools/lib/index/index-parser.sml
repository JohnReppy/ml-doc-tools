(* index-parser.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure IndexParser : sig

    val parseIndexFile : string -> IndexRep.index

  end = struct

    structure IP = IndexParseTree
    structure IR = IndexRep

    structure IndexLrVals = IndexLrValsFun(structure Token = LrParser.Token)
    structure Lex = IndexLexFun(structure Tokens = IndexLrVals.Tokens)
    structure Parser = Join(
        structure Lex= Lex
        structure LrParser = LrParser
        structure ParserData = IndexLrVals.ParserData)

    fun parseFile fname = let
	  fun errorFn (msg, lnum, _) =
		TextIO.output (TextIO.stdErr, Format.format
		  "[%s:%d] %s\n"
		  [Format.STR fname, Format.INT lnum, Format.STR msg])
	  val inStrm = TextIO.openIn fname
	  val lexer = Parser.makeLexer (fn n => TextIO.inputN(inStrm, n))
	  val (result, _) = Parser.parse (
		15,     (* lookahead *)
		lexer,
		errorFn,
		())
	  in
	    TextIO.closeIn inStrm;
	    result
	  end

    fun parseIndexFile fname = let
	  val IP.INDEX{files, anchors, labels, sigs, modules} = parseFile fname
	  val (index as IR.IDX{
		fileTbl, anchorTbl, labelTbl, strTbl, fctTbl, ...
	      }) = IR.mkIndex()
	  fun insFile (IP.FILE{name, isEmpty, title, content}) = let
		fun cvtContents (IP.SECTION{content, label, title} :: r) =
		      IR.SECTION{
			  title = title,
			  label = label,
			  content = cvtContents content
			} :: cvtContents r
		  | cvtContents (IP.INCLFILE f :: r) =
		      IR.INCLFILE f :: cvtContents r
		  | cvtContents (_ :: r) = cvtContents r
		  | cvtContents [] = []
		in
		  AtomTable.insert fileTbl (name, IR.FILEentry{
		      name = name, isEmpty = isEmpty, title = title,
		      content = ref(cvtContents content)
		    })
		end
	  fun insAnchor (IP.ANCHOR{tag, file}) =
		AtomTable.insert anchorTbl (tag, IR.ANCHORentry{
		    file = IR.findFile (index, file), tag = tag
		  })
	  fun insLabel (IP.LABEL{name, kind, file}) =
		AtomTable.insert labelTbl (name, IR.LABELentry{
		    name = name, kind = kind, file = IR.findFile (index, file)
		  })
	  fun findSig id = IR.findSig(index, id)
	  fun mkSigRef (file, IP.SIGREF sigId, ctx) = let
		val sg = IR.NamedSig(findSig sigId)
		in
		  IR.addSigInstance (sg, ctx);
		  sg
		end
	    | mkSigRef (file, IP.SIGENV env, ctx) =
		IR.AnonSig(mkEnv (file, env, ctx))
	    | mkSigRef (file, IP.SIGEXTERN, _) = IR.ExternSig
	  and mkEnv (file, IP.ENV{incl, strs, tys, exns, vals}, ctx) = let
		val env = IR.mkEnv {index=index, file=file, context=ctx}
		val IR.ENV{strTbl, exnTbl, tyTbl, conTbl, valTbl, includes, ...} = env
		fun insStr (IP.STR{id, bodySig}) =
		      AtomTable.insert strTbl (id, IR.STRentry{
			  id = id, file = file,
			  bodySig = mkSigRef(file, bodySig, IR.extendContext(ctx, id)),
			  binding = SOME env
			})
		fun insTy (IP.DTY{id, cons}) = let
		      val entry = IR.TYentry{
			      id = id, kind=IR.DATATYPE cons, binding=env
			    }
		      in
			List.app (fn id => AtomTable.insert conTbl (id, entry)) cons;
			AtomTable.insert tyTbl (id, entry)
		      end
		  | insTy (IP.DTYDEF{id}) = AtomTable.insert tyTbl
		      (id, IR.TYentry{id = id, kind=IR.DATATYPEDEF,  binding=env})
		  | insTy (IP.TY{id, eq=true}) = AtomTable.insert tyTbl
		      (id, IR.TYentry{id = id, kind=IR.EQTYPE, binding=env})
		  | insTy (IP.TY{id, eq=false}) = AtomTable.insert tyTbl
		      (id, IR.TYentry{id = id, kind=IR.TYPE, binding=env})
		fun insExn id =
		      AtomTable.insert exnTbl (id, IR.EXNentry{id=id, binding=env})
		fun insVal id =
		      AtomTable.insert valTbl (id, IR.VALentry{id=id, binding=env})
		in
		  includes := List.map findSig incl;
		  List.app insStr strs;
		  List.app insTy tys;
		  List.app insExn exns;
		  List.app insVal vals;
		  env
		end
	  fun insSig (IP.SIGNATURE{id, file, env}) =
		ignore (
		  IR.defineSig (index, id,
		    mkEnv(IR.findFile (index, file), env, IR.SIGbound[id])))
	    | insSig (IP.UNDEF_SIG id) =
		ignore (IR.findSig (index, id))
	  fun insModule (IP.STRUCTURE{file, str=IP.STR{id, bodySig}}) = let
		val fileEntry = IR.findFile (index, file)
		in
		  AtomTable.insert strTbl (id, IR.STRentry{
		      id = id, file = fileEntry,
		      bodySig = mkSigRef(fileEntry, bodySig, IR.STRbound[id]),
		      binding = NONE
		    })
		end
	    | insModule (IP.FUNCTOR{id, file, argSig, bodySig}) = let
		val fileEntry = IR.findFile (index, file)
		in
		  AtomTable.insert fctTbl (id, IR.FCTentry{
		      id = id, file = fileEntry,
		      argSig = mkSigRef(fileEntry, argSig, IR.ARGbound[id]),
		      bodySig = mkSigRef(fileEntry, bodySig, IR.FCTbound[id])
		    })
		end
	  in
	    List.app insFile files;
	    List.app insAnchor anchors;
	    List.app insLabel labels;
	    List.app insSig sigs;
	    List.app insModule modules;
	    index
	  end

  end
