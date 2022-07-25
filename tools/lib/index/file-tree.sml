(* file-tree.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Code for creating the inclusion forest of files from an index.
 *)

structure FileTree : sig

    datatype file_nd = FILE of {
	file : MLDocIndex.File.entry,
	level : int,
	content : file_content list
      }

    and file_content
      = SECTION of {
	    title : string,
	    label : Atom.atom option,
	    content : file_content list
	  }
      | INCLFILE of file_nd

    val mkForest : MLDocIndex.index -> {
	    roots : file_nd list,
	    nodeMap : MLDocIndex.File.entry -> file_nd,
	    parentMap : MLDocIndex.File.entry -> file_nd option
	  }

  end = struct

    structure IR = IndexRep

    datatype file_nd = FILE of {
	file : MLDocIndex.File.entry,
	level : int,
	content : file_content list
      }

    and file_content
      = SECTION of {
	    title : string,
	    label : Atom.atom option,
	    content : file_content list
	  }
      | INCLFILE of file_nd

    structure Set = BinarySetFn(
	type ord_key = IR.file_entry
	fun compare (IR.FILEentry{name=a, ...}, IR.FILEentry{name=b, ...}) =
	      Atom.compare(a, b)
      );

  (* allocate a table for mapping file names to a pair of their file
   * entry and their parent reference.  We construct this table in two passes:
   * the first inserts a node for eachfile, and the second sets the parent
   * links.
   *)
    fun gatherRoots fileTbl = let
	  val roots = let
		val allFiles = Set.addList(Set.empty, AtomTable.listItems fileTbl)
		fun f (IR.FILEentry{content, ...}, s) = let
		      fun walk (IR.SECTION{content, ...}, s) =
			    List.foldl walk s content
			| walk (IR.INCLFILE f, s) = (
			    case AtomTable.find fileTbl f
			     of (SOME entry) => (Set.delete(s, entry) handle _ => s)
			      | NONE => s
			    (* end case *))
		      in
			List.foldl walk s (! content)
		      end
		in
		  AtomTable.fold f allFiles fileTbl
		end
	  val nodeMap =
		AtomTable.mkTable(AtomTable.numItems fileTbl, Fail "nodeMap")
	  fun cvt entry = let
		fun walkEntry (lvl, entry as IR.FILEentry{name, content, ...}) = let
		      val nd = FILE{
			      file = entry,
			      level = lvl,
			      content = List.foldr (walk lvl) [] (!content)
			    }
		      in
			AtomTable.insert nodeMap (name, nd);
			nd
		      end
		and walk lvl (IR.SECTION{title, label, content}, l) = let
		      val content' = List.foldr (walk (lvl+1)) [] content
		      in
			SECTION{title = title, label = label, content = content'}
			  :: l
		      end
		  | walk lvl (IR.INCLFILE name, l) = (
		      case AtomTable.find fileTbl name
		       of (SOME entry) => INCLFILE(walkEntry (lvl, entry)) :: l
			| NONE => l
		      (* end case *))
		in
		  walkEntry (0, entry)
		end
	  val roots' = Set.foldl (fn (item, l) => (cvt item)::l) [] roots
	  val parentMap =
		AtomTable.mkTable(AtomTable.numItems fileTbl, Fail "parentMap")
	  fun insParents root = let
		fun walkEntry (optParent, IR.FILEentry{name, content, ...}) = let
		      val nd = AtomTable.lookup nodeMap name
		      fun walk (IR.SECTION{content, ...}) = List.app walk content
			| walk (IR.INCLFILE childName) = (
			    case AtomTable.find fileTbl childName
			     of (SOME entry) => walkEntry (SOME nd, entry)
			      | NONE => ()
			    (* end case *))
		      in
			AtomTable.insert parentMap (name, optParent);
			List.app walk (!content)
		      end
		in
		  walkEntry (NONE, root)
		end
	  in
	    Set.app insParents roots;
	    (roots', nodeMap, parentMap)
	  end

    fun mkForest (IR.IDX{fileTbl, ...}) = let
	  val (roots, nodeTbl, parentTbl) = gatherRoots fileTbl
	  fun mkMap tbl (IR.FILEentry{name, ...}) = AtomTable.lookup tbl name
	  in
	    { roots = roots,
	      nodeMap = mkMap nodeTbl,
	      parentMap = mkMap parentTbl
	    }
	  end

  end;
