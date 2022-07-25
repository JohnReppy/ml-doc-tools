(* config-db.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * This module provides support for configuration databases, including
 * support for parsing a configuration file.
 *
 * TODO:
 *  - support for \ at end of line
 *)

structure ConfigDB :> CONFIG_DB =
  struct

    structure A = Atom
    structure Tbl = AtomTable

  (**** Configuration files ****)

    datatype config_db = CF of value Tbl.hash_table

    and value
      = ATOM of Atom.atom
      | STR of string
      | NUM of int
      | BOOL of bool
      | LIST of value list
      | TBL of config_db

  (* do a deep copy of a config DB table *)
    fun copyConfigDB (CF cf) = let
	  val newCF = Tbl.mkTable(Tbl.numItems cf, Fail "ConfigDB")
	  fun copy (key, item) = Tbl.insert newCF (key, copyItem item)
	  in
	    Tbl.appi copy cf;
	    CF newCF
	  end

    and copyItem (TBL tbl) = TBL(copyConfigDB tbl)
      | copyItem item = item

  (* merge the contents of the second config DB table into the first. *)
    fun mergeDB (CF cf1, CF cf2) = let
	  fun merge (key, item2) = (case Tbl.find cf1 key
		 of NONE => Tbl.insert cf1 (key, copyItem item2)
		  | (SOME item1) => (case (item1, item2)
		       of (TBL tbl1, TBL tbl2) => mergeDB (tbl1, tbl2)
			| _ => Tbl.insert cf1 (key, copyItem item2)
		      (* end case *))
		(* end case *))
	  in
	    Tbl.appi merge cf2
	  end

    fun mergeConfigDBs (cf1, cf2) = let
	  val newCF = copyConfigDB cf1
	  in
	    mergeDB (newCF, cf2);
	    newCF
	  end

    fun dump (outStrm, cf) = let
	  fun pr s = TextIO.output(outStrm, s)
	  fun prIndent n = pr(StringCvt.padLeft #" " n "")
	  fun prNameVal indent (name, v) = (
		prIndent indent;
		pr(Atom.toString name);
		pr " = ";
		prValue (indent, v);
		pr "\n")
	  and prValue (_, ATOM a) = pr(Atom.toString a)
	    | prValue (_, STR s) = pr(concat["\"", String.toString s, "\""])
	    | prValue (_, NUM n) = pr(Int.toString n)
	    | prValue (_, BOOL b) = pr(Bool.toString b)
	    | prValue (indent, LIST l) = pr "<list>"
	    | prValue (indent, TBL tbl) = prTbl(indent, tbl)
	  and prTbl (indent, CF tbl) = (
		pr "{\n";
		Tbl.appi (prNameVal (indent+2)) tbl;
		prIndent indent;
		pr "}")
	  in
	    prTbl(0, cf); pr "\n"
	  end

    fun mkConfigDB () = CF(Tbl.mkTable(16, Fail "ConfigDB"))

    fun parse fname = let
	  val specs = ConfigParse.parse{name = fname, errMsg = TextIO.print}
	  val tbl = Tbl.mkTable(List.length specs, Fail "ConfigDB")
	  fun cvt (ConfigParse.NAME a) = ATOM a
	    | cvt (ConfigParse.SVAL s) = STR s
	    | cvt (ConfigParse.NVAL n) = NUM n
	    | cvt (ConfigParse.BVAL n) = BOOL n
	    | cvt (ConfigParse.LIST vl) = LIST(List.map cvt vl)
	    | cvt (ConfigParse.OBJ l) = TBL(cvtTbl l)
	  and cvtTbl l = let
		val tbl = Tbl.mkTable(List.length specs, Fail "ConfigDB")
		fun ins (key, ConfigParse.OBJ l) = (case Tbl.find tbl key
		       of (SOME(TBL cf)) => mergeDB(cf, cvtTbl l)
			| _ => Tbl.insert tbl (key, TBL(cvtTbl l))
		      (* end case *))
		  | ins (key, item) = Tbl.insert tbl (key, cvt item)
		in
		  List.app ins l; CF tbl
		end
	  in
	    cvtTbl specs
	  end


  (**** Searching ****)

    fun find (CF tbl, path) = let
	  fun look (_, []) = NONE
	    | look (tbl, [x]) = Tbl.find tbl x
	    | look (tbl, x::r) = (case Tbl.find tbl x
		 of (SOME(TBL(CF tbl'))) => look(tbl', r)
		  | _ => NONE
		(* end case *))
	  in
	    look (tbl, path)
	  end
    fun find' (db, path) = find (db, List.map Atom.atom path)

    fun add (CF tbl, path, v, override) = let
	  val v = copyItem v
	  fun look (_, []) = ()
	    | look (tbl, [x]) =
		if (override orelse not(isSome(Tbl.find tbl x)))
		  then Tbl.insert tbl (x, v)
		  else ()
	    | look (tbl, x::r) = (case (override, Tbl.find tbl x)
		 of (_, SOME(TBL(CF tbl'))) => look(tbl', r)
		  | (false, SOME _) => ()
		  | _ => let
		      fun newTbl (x, v) = let
			    val tbl = Tbl.mkTable (4, Fail "ConfigDB")
			    in
			      Tbl.insert tbl (x, v);
			      TBL(CF tbl)
			    end
		      fun mkTbl [x] = newTbl(x, v)
			| mkTbl (x::r) = newTbl(x, mkTbl r)
		      in
			Tbl.insert tbl (x, mkTbl r)
		      end
		(* end case *))
	  in
	    look (tbl, path)
	  end
    fun add' (db, path, v, override) =
	  add (db, List.map Atom.atom path, v, override)

  end;
