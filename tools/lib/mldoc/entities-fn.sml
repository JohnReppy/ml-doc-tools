(* entities-fn.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * Generic code for translating the standard ML-Doc entities.
 *)

functor EntitiesFn (val entityMap : (string * string) list) :
  sig

    val transEntity : substring -> string

  end = struct

    structure SS = Substring
    structure HT = HashTableFn (
      struct
	type hash_key = substring
	fun hashVal ss = HashString.hashString(SS.string ss)
	fun sameKey (ss1, ss2) = (SS.compare(ss1, ss2) = EQUAL)
      end)

    val tbl = let
	  val tbl = HT.mkTable (List.length entityMap, Fail "Undefined entity")
	  fun ins (a, b) = HT.insert tbl (SS.full a, b)
	  in
	    List.app ins entityMap;
	    tbl
	  end

    fun transEntity ss = (case SS.getc ss
	   of SOME(#"@", rest) => (case HT.find tbl rest
		 of (SOME s) => s
		  | NONE => (
		      Error.warning("Undefined entity \"%s\"",
			[Format.STR(SS.string ss)]);
		      SS.string rest)
		(* end case *))
	    | _ => SS.string ss
	  (* end case *))

  end;

