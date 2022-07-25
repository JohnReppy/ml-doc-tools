(* index-group.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Routines to structure a list of index elements into a list of
 * element groups.
 *)

structure IndexGroup : sig

    datatype group
      = Single of IndexEntry.entry
      | Multi of {
	  id : string,
	  linkId : string,
	  kind : HRefs.xref_kind,
	  entries : {
	      context : MLDocIndex.binding_context
	    } list
	}

    val groupEntries : IndexEntry.entry list -> group list

    val alphaGroups : group list -> (string * group list) list
	(* split the groups into alphabetical chunks *)

    val mkColumns : (group list * int) -> group list list

  end = struct

    structure E = IndexEntry

    datatype group
      = Single of E.entry
      | Multi of {
	  id : string,
	  linkId : string,
	  kind : HRefs.xref_kind,
	  entries : {
	      context : MLDocIndex.binding_context
	    } list
	}

    local
    (* case insensitive character comparison, with symbolic characters
     * always before letters.
     *)
      fun caseCmpChar (a, b) = (case (Char.isAlpha a, Char.isAlpha b)
	     of (true, true) => Char.compare (Char.toUpper a, Char.toUpper b)
	      | (false, false) => Char.compare (a, b)
	      | (false, true) => LESS
	      | (true, false) => GREATER
	    (* end case *))
      val caseCmp = String.collate caseCmpChar
      fun cmpKind (a, b) = let
	    fun toInt HRefs.SigRef = 0
	      | toInt HRefs.StrRef = 1
	      | toInt HRefs.FctRef = 2
	      | toInt HRefs.ExnRef = 3
	      | toInt HRefs.TyRef  = 4
	      | toInt HRefs.ConRef = 5
	      | toInt HRefs.ValRef = 6
	    in
	      Int.compare(toInt a, toInt b)
	    end
      fun cvtContext (MLDocIndex.TOPbound) = []
	| cvtContext (MLDocIndex.SIGbound path) = path
	| cvtContext (MLDocIndex.STRbound path) = path
	| cvtContext (MLDocIndex.FCTbound path) = path
	| cvtContext (MLDocIndex.ARGbound path) = path
      fun cmpContext (ctx1, ctx2) = let
	    fun cmp ([], []) = EQUAL
	      | cmp ([], _) = LESS
	      | cmp (_, []) = GREATER
	      | cmp (a::r1, b::r2) = (
		  case String.compare(Atom.toString a, Atom.toString b)
		   of EQUAL => cmp (r1, r2)
		    | order => order
		  (* end case *))
	    in
	      cmp (cvtContext ctx1, cvtContext ctx2)
	    end
      fun caseCmpEntry (
	    E.E{id, kind, context, ...}, E.E{id=id', kind=kind', context=context', ...}
	  ) = (case caseCmp(id, id')
	     of EQUAL => (case cmpKind(kind, kind')
		   of EQUAL => cmpContext(context, context')
		    | order => order
		  (* end case *))
	      | order => order
	    (* end case *))
      fun cmpEntry (
	    E.E{id, kind, context, ...}, E.E{id=id', kind=kind', context=context', ...}
	  ) = (case String.compare(id, id')
	     of EQUAL => (case cmpKind(kind, kind')
		   of EQUAL => cmpContext(context, context')
		    | order => order
		  (* end case *))
	      | order => order
	    (* end case *))
    in
    val sortEntries = ListMergeSort.sort (fn (a, b) => (caseCmpEntry(a, b) = GREATER))
    val sortGroup = ListMergeSort.sort (fn (a, b) => (cmpEntry(a, b) = GREATER))
    fun caseEqId (E.E{id, ...}, E.E{id=id', ...}) = (caseCmp(id, id') = EQUAL)
    fun eqId (E.E{id, kind, ...}, E.E{id=id', kind=kind', ...}) =
	  (kind = kind') andalso (id = id')
    end (* local *)

  (* convert the list of elements to a list of lists of elements, where each
   * of the sublists has a single element name.
   *)
    fun mkGroups elems = let
	  fun group eq [] = []
	    | group eq (item::r) = let
		fun f (_, [], grp, grps) = List.rev((List.rev grp) :: grps)
		  | f (item, item'::r, grp, grps) =
			if eq(item, item')
			  then f (item, r, item'::grp, grps)
			  else f (item', r, [item'], (List.rev grp)::grps)
		in
		  f (item, r, [item], [])
		end
	  fun subdivideGroup (grp as [_], gl) = grp :: gl
	    | subdivideGroup (grp, gl) = (group eqId (sortGroup grp)) @ gl
	  in
	    List.foldr subdivideGroup [] (group caseEqId elems)
	  end

    fun classifyGroup [] = raise Fail "empty group"
      | classifyGroup [elem] = Single elem
      | classifyGroup (el as (E.E{id, linkId, kind, ...} :: r)) = Multi{
	    id = id, linkId = linkId, kind = kind,
	    entries = List.map
	      (fn (E.E{context, ...}) => {context=context}) el
	  }

    fun groupEntries el = List.map classifyGroup (mkGroups (sortEntries el))

    fun alphaGroups [] = []
      | alphaGroups (g::rest) = let
	  fun firstChar (Single(E.E{id, ...})) = String.sub(id, 0)
	    | firstChar (Multi{id, ...}) = String.sub(id, 0)
	  fun isFirstChar c = if (Char.isAlpha c)
		then let val c' = Char.toUpper c
		  in
		    (fn g => (c' = Char.toUpper(firstChar g)))
		  end
		else (fn g => not(Char.isAlpha(firstChar g)))
	  fun tagGroups ([], gll) = gll
	    | tagGroups ((gl as (g::_))::rest, gll) = let
		val c = firstChar g
		in
		  if (Char.isAlpha c)
		    then tagGroups (rest, (String.str(Char.toUpper c), gl)::gll)
		    else tagGroups (rest, ("OTHER", gl)::gll)
		end
	  fun group ([], _, [], gll) = tagGroups (gll, [])
	    | group ([], _, gl, gll) = tagGroups ((rev gl)::gll, [])
	    | group (g::r, pred, gl, gll) =
		if (pred g)
		  then group (r, pred, g::gl, gll)
		  else group (r, isFirstChar(firstChar g), [g], (rev gl)::gll)
	  in
	    group (rest, isFirstChar(firstChar g), [g], [])
	  end

    fun groupLen (Single _) = 1
      | groupLen (Multi{entries, ...}) = 1 + length entries

    fun mkColumns (groups, ncols) = let
	  fun numLines (n, []) = n
	    | numLines (n, grp::r) = numLines (n + groupLen grp, r)
	  val desiredLen = (numLines (0, groups) + (ncols-1)) div ncols
	  fun chop (_, [], [], cols) = rev cols
	    | chop (_, [], curCol, cols) = rev((rev curCol)::cols)
	    | chop (n, grp::r, curCol, cols) = let
		val k = groupLen grp
		in
		  if (k >= n)
		    then chop (desiredLen, r, [], (rev(grp::curCol))::cols)
		    else chop (n-k, r, grp::curCol, cols)
		end
	  in
	    chop (desiredLen, groups, [], [])
	  end

  end;

