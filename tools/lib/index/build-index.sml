(* build-index.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research
 *
 * This is the interface for building an index.
 *)

structure BuildIndex : BUILD_INDEX =
  struct
    structure Tbl = AtomTable
    structure I = MLDocIndex
    structure IR = IndexRep

    type name = I.name
    type index = I.index
    type env = I.env

    datatype sig_ref = datatype IR.sig_ref

    val index = IR.mkIndex

    val findSig = IR.findSig

    fun newEnv (index, file, context) =
	  IR.mkEnv {index=index, file=file, context=context}

    fun topEnv (index as IR.IDX{topEnv, ...}, {file}) = let
	  val env = IR.mkEnv {index=index, file=file, context=IR.TOPbound}
	  in
	    topEnv := env :: !topEnv;
	    env
	  end

    fun copyEnv (index, IR.ENV{file, context, ...}) =
	  IR.mkEnv {index=index, file=file, context=context}

    fun newSig (index, {file, sigId}) =
	  IR.defineSig (index, sigId,
	    IR.mkEnv {index=index, file=file, context=IR.SIGbound[sigId]})

    fun newStr (IR.IDX{strTbl, ...}, {strId, bodySig, file}) = (
	  case (Tbl.find strTbl strId)
	   of NONE => (
		IR.addSigInstance (bodySig, IR.STRbound[strId]);
		Tbl.insert strTbl (strId, IR.STRentry{
		    id = strId, bodySig = bodySig, file = file, binding = NONE
		  }))
	    | (SOME _) => raise Fail "multiple definition of structure"
	  (* end case *))

    fun newSubstr (env as IR.ENV{file, strTbl, context, ...}, {strId : name, bodySig}) = (
	  case (Tbl.find strTbl strId)
	   of NONE => (
		IR.addSigInstance (bodySig,
		  IR.extendContext(context, strId)); 
		Tbl.insert strTbl (strId, IR.STRentry{
		    id = strId, bodySig = bodySig, file = file, binding = SOME env
		  }))
	    | (SOME(IR.STRentry{bodySig=IR.NamedSig(IR.SIGentry _), ...})) => (
		case bodySig
		 of (IR.NamedSig _) => ()
		  | _ => raise Fail "inconsistent signatures for sub-structure"
		(* end case *))
	    | (SOME _) => raise Fail "multiple definition of sub-structure"
	  (* end case *))

    fun newFct (IR.IDX{fctTbl, ...}, {fctId, argSig, bodySig, file}) = (
	  case (Tbl.find fctTbl fctId)
	   of NONE => let
		val entry = IR.FCTentry{
			id = fctId, argSig = argSig, bodySig = bodySig, file = file
		      }
		in
		  IR.addSigInstance (argSig, IR.FCTbound[fctId]);
		  IR.addSigInstance (bodySig, IR.ARGbound[fctId]);
		  Tbl.insert fctTbl (fctId, entry)
		end
	    | (SOME _) => raise Fail "multiple definition of functor"
	  (* end case *))

    fun includeSig (IR.ENV{index, includes, ...}, sigId) =
	  includes := !includes @ [IR.findSig (index, sigId)]

    fun insType (env as IR.ENV{tyTbl, ...}, {eq, id}) =
	  Tbl.insert tyTbl (id, IR.TYentry{
	      id = id,
	      kind = if eq then IR.EQTYPE else IR.TYPE,
	      binding = env
	    })
    fun insDatatypeDef (env as IR.ENV{tyTbl, conTbl, ...}, {id}) =
	  Tbl.insert tyTbl (id, IR.TYentry{
	      id = id, kind = IR.DATATYPEDEF, binding=env
	    })
    fun insDatatype (env as IR.ENV{tyTbl, conTbl, ...}, {id, cons}) = let
	  val entry = IR.TYentry{id = id, kind = IR.DATATYPE cons, binding=env}
	  in
	    List.app (fn id => Tbl.insert conTbl (id, entry)) cons;
	    Tbl.insert tyTbl (id, entry)
	  end
    fun insExn (env as IR.ENV{exnTbl, ...}, id) =
	  Tbl.insert exnTbl (id, IR.EXNentry{id=id, binding=env})
    fun insVal (env as IR.ENV{valTbl, ...}, id) =
	  Tbl.insert valTbl (id, IR.VALentry{id=id, binding=env})

  (* Support for other global objects *)
    fun insFile (IR.IDX{fileTbl, ...}, {name, isEmpty, title}) = (
	  case Tbl.find fileTbl name
	   of NONE => let
		val entry = IR.FILEentry{
			name = name, isEmpty = isEmpty, title = title,
			content = ref []
		      }
		in
		  Tbl.insert fileTbl (name, entry); entry
		end
	    | (SOME _) => raise Fail "multiple definition of file"
	  (* end case *))

    fun setFileContent (IR.FILEentry{content, ...}, c) = content := c

    fun insAnchor (IR.IDX{anchorTbl, ...}, {tag, file}) = (
	  case Tbl.find anchorTbl tag
	   of NONE => Tbl.insert anchorTbl (tag, IR.ANCHORentry{
		  tag=tag, file=file
		})
	    | (SOME _) => raise Fail "multiple definition of anchor"
	  (* end case *))

    fun insLabel kind (IR.IDX{labelTbl, ...}, {name, file}) = (
	  case Tbl.find labelTbl name
	   of NONE => Tbl.insert labelTbl (name, IR.LABELentry{
		  name=name, kind=kind, file=file
		})
	    | (SOME _) => raise Fail(concat[
		  "multiple definition of label \"", Atom.toString name, "\""
		])
	  (* end case *))
    val insSecLabel = insLabel IR.Section
    val insTblLabel = insLabel IR.Table
    val insFigLabel = insLabel IR.Figure
    val insCodeLabel = insLabel IR.Code
    val insIfaceLabel = insLabel IR.Interface

  (* merge the first index into the second index *)
    fun merge (idx1, idx2) = let
	  fun dupErr (kind, name) = raise Fail(concat[
		  "duplicate definitions of ", kind, " \"",
		  Atom.toString name, "\""
		])
	  val IR.IDX{
		  fileTbl=fileTbl1, anchorTbl=anchorTbl1, labelTbl=labelTbl1,
		  topEnv=topEnv1, sigTbl=sigTbl1, strTbl=strTbl1, fctTbl=fctTbl1
		} = idx1
	  val IR.IDX{
		  fileTbl=fileTbl2, anchorTbl=anchorTbl2, labelTbl=labelTbl2,
		  topEnv=topEnv2, sigTbl=sigTbl2, strTbl=strTbl2, fctTbl=fctTbl2
		} = idx2
	(* merge two sorted lists of signature instances *)
	  fun mergeInstances ([], []) = []
	    | mergeInstances ([], l2) = l2
	    | mergeInstances (l1, []) = l1
	    | mergeInstances (i1::l1, i2::l2) = (case I.compareContext(i1, i2)
		 of LESS => i1::mergeInstances(l1, i2::l2)
		  | EQUAL => i1::mergeInstances(l1, l2)
		  | GREATER => i2::mergeInstances (i1::l1, l2)
		(* end case *))
	(* Note: we are working under the assumption that the first index
	 * will not be modified in the future.
	 *)
	  fun insItem (tbl, tblName, getName) entry = let
		val name = getName entry
		in
		  case (Tbl.find tbl name)
		   of NONE => Tbl.insert tbl (name, entry)
		    | (SOME _) => dupErr(tblName, name)
		  (* end case *)
		end
	  val insFile =
		insItem (fileTbl2, "file", fn (IR.FILEentry{name, ...}) => name)
	  val insAnchor =
		insItem (anchorTbl2, "anchor", fn (IR.ANCHORentry{tag, ...}) => tag)
	  val insLabel =
		insItem (labelTbl2, "label", fn (IR.LABELentry{name, ...}) => name)
	  fun insSig (entry as IR.SIGentry{id, body=body1, instances=i1}) = (
		case (Tbl.find sigTbl2 id)
		 of NONE => Tbl.insert sigTbl2 (id, entry)
		  | SOME(IR.SIGentry{body=body2, instances=i2, ...}) => (
		      i2 := mergeInstances (!i1, !i2);
		      case (!body1, !body2)
		       of (NONE, _) => ()
			| (SOME env, NONE) => body2 := SOME env
			| (SOME _, SOME _) => dupErr("signature", id)
		      (* end case *))
		(* end case *))
	  val insStr =
		insItem (strTbl2, "structure", fn (IR.STRentry{id, ...}) => id)
	  val insFct =
		insItem (fctTbl2, "functor", fn (IR.FCTentry{id, ...}) => id)
	  in
	  (* merge the file tables *)
	    Tbl.app insFile fileTbl1;
	  (* merge the anchor tables *)
	    Tbl.app insAnchor anchorTbl1;
	  (* merge the label tables *)
	    Tbl.app insLabel labelTbl1;
	  (* merge the top-level environments. *)
	    topEnv2 := !topEnv1 @ !topEnv2;
	  (* merge the signature tables *)
	    Tbl.app insSig sigTbl1;
	  (* merge the top-level structure tables *)
	    Tbl.app insStr strTbl1;
	  (* merge the functor tables *)
	    Tbl.app insFct fctTbl1
	  end

  end
