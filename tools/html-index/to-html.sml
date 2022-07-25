(* to-html.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure ToHTML : sig

    val mkIndexTable :
	  HTMLContext.context -> IndexGroup.group list list -> HTML.block

  end = struct

    structure E = IndexEntry
    structure G = IndexGroup
    structure I = MLDocIndex
    structure Mk = MakeHTML

    val emptyCell = Mk.mkTD(HTML.BlockList[])
    val twoEmpty = [emptyCell, emptyCell]
    fun oneCell contents = [Mk.mkTD_COLSPAN{colspan=2, content=contents}]
    fun twoCell contents = [emptyCell, Mk.mkTD contents]

  (* convert a binding context to a list of HTML text items *)
    fun contextToHTML ctx = let
	  fun toHTML [] = []
	    | toHTML (id::r) = HTML.PCDATA "." ::
		HTML.PCDATA(Atom.toString id) :: toHTML r
	  fun doPath (id, r) = HTML.PCDATA(Atom.toString id) :: (toHTML r)
	  in
	    case ctx
	     of (I.TOPbound) => [HTML.I(HTML.PCDATA "&lt;top level&gt;")]
	      | (I.SIGbound(id::ids)) => doPath (id, ids)
	      | (I.STRbound(id::ids)) => doPath (id, ids)
	      | (I.FCTbound(id::ids)) => doPath (id, ids)
	      | (I.ARGbound(id::ids)) => doPath (id, ids)
	    (* end case *)
	  end

    fun wrapText ctx {bindCtx, id, kind} text =
	  HTML.TextBlock(HTML.TextList[
	      HRefs.mkHRef (
		HRefs.remoteURL (HTMLContext.index ctx) {
		    base=NONE, bindCtx=bindCtx, id=id, kind=kind
		  })
	        (HTML.CODE(Mk.textList text))
	    ])

  (* make a list of HTML table rows out of a list of columns, each of which
   * is a list of groups.
   *)
    fun mkTableRows ctx cols = let
	  val wrapText = wrapText ctx
	  fun bf id = HTML.B(HTML.PCDATA id)
	(* convert a group to a list of list of cells *)
	  fun grpToHTML (G.Single(E.E{
		  id, linkId, kind, context as I.TOPbound
		})) = [
		  oneCell(wrapText {bindCtx=context, id=linkId, kind=kind} [bf id])
		]
	    | grpToHTML (G.Single(E.E{id, linkId, kind, context})) = [
		  oneCell(wrapText {bindCtx=context, id=linkId, kind=kind}
		    (bf id :: HTML.PCDATA ", " :: contextToHTML context))
		]
	    | grpToHTML (G.Multi{id, linkId, kind, entries}) = let
		fun f {context} = twoCell (
		      wrapText {bindCtx=context, id=linkId, kind=kind}
			(contextToHTML context))
		in
		  oneCell(HTML.TextBlock(bf id)) :: (List.map f entries)
		end
	  val htmlCols = List.map
		(fn col => List.foldr (fn (g, rl) => grpToHTML g @ rl) [] col)
		  cols
	(* split a list of column lists into a list of the first row and a list
	 * of the rest.
	 *)
	  fun split ([], hl, tl) = (rev hl, rev tl)
	    | split ([]::r, hl, tl) = split (r, twoEmpty::hl, []::tl)
	    | split ((x::r)::rr, hl, tl) = split (rr, x::hl, r::tl)
	  fun allEmpty [] = true
	    | allEmpty ([]::r) = allEmpty r
	    | allEmpty _ = false
	  fun mkRows (cols, rows) = if (allEmpty cols)
		then rev rows
		else let
		  val (row, cols) = split (cols, [], [])
		  val row = Mk.mkTR(List.concat row)
		  in
		    mkRows (cols, row::rows)
		  end
	  in
	    mkRows (htmlCols, [])
	  end

    fun mkIndexTable ctx cols = let
	  val numIndexCols = length cols
	  val rows = mkTableRows ctx cols
	  in
	    HTML.TABLE{
		align = SOME HTML.HAlign.center,
		width = SOME("80%"),
		border = NONE,
		cellspacing = NONE,
		cellpadding = NONE,
		caption = NONE,
		content = rows
	      }
	  end

  end;

