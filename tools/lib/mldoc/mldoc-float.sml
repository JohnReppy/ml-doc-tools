(* mldoc-float.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Support for analysing <FLOAT> and <TABLE> elements.
 *)

structure MLDocFloat : sig

    structure M : MARKUP

    datatype cap_align = TOP | BOTTOM
    datatype col_align = LEFT | RIGHT | CENTER | DEFAULT | PARBOX of string

    datatype table_cell
      = TH of {colspan : int option, align : col_align, contents : M.markup list}
      | TD of {colspan : int option, align : col_align, contents : M.markup list}

    datatype float
      = FIGURE of {
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    file : string
	  }
      | TABLE of {
	    long : bool,
	    small : bool,
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    cols : col_align list,
	    rows : table_cell list list
	  }
      | CODE of {
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    body : M.markup		(* the <CODE> element *)
	  }

    val getFloatContents : M.markup -> float

    val getTableContents : M.markup -> {
	    long  : bool,
	    small : bool,
	    cols  : col_align list,
	    rows  : table_cell list list
	  }

  end = struct

    structure E = MLDocElem
    structure M = MLDocMarkup

    datatype cap_align = TOP | BOTTOM
    datatype col_align = LEFT | RIGHT | CENTER | DEFAULT | PARBOX of string

    datatype table_cell
      = TH of {colspan : int option, align : col_align, contents : M.markup list}
      | TD of {colspan : int option, align : col_align, contents : M.markup list}

    datatype float
      = FIGURE of {
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    file : string
	  }
      | TABLE of {
	    long : bool,
	    small : bool,
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    cols : col_align list,
	    rows : table_cell list list
	  }
      | CODE of {
	    label : string,
	    caption : M.markup list,	(* [] for no caption *)
	    capAlign : cap_align,
	    body : M.markup		(* the <CODE> element *)
	  }

    fun transCapAlign (true, NONE) = TOP	(* table *)
      | transCapAlign (false, NONE) = BOTTOM	(* figure *)
      | transCapAlign (_, SOME E.TOP) = TOP
      | transCapAlign (_, SOME E.BOTTOM) = BOTTOM

    fun transAlign NONE = DEFAULT
      | transAlign (SOME E.ALIGN_CENTER) = CENTER
      | transAlign (SOME E.ALIGN_LEFT) = LEFT
      | transAlign (SOME E.ALIGN_RIGHT) = RIGHT

    fun getTableContents (M.ELEM{elem=E.TABLE{long, small}, body, ...}) = let
	  fun getCol (M.ELEM{elem=E.COL{parbox = SOME s, ...}, ...}::r, cols) =
		getCol (r, PARBOX s :: cols)
	    | getCol (M.ELEM{elem=E.COL{align, ...}, ...}::r, cols) =
		getCol (r, transAlign align :: cols)
	    | getCol (r, cols) = (rev cols, r)
	  val (cols, rest) = getCol (body, [])
	  val nCols = length cols
	  fun getRows ([], rows) = rev rows
	    | getRows (M.ELEM{elem=E.TR, body, ...}::r, rows) =
		getRows (r, getRowContents (body, nCols, []) :: rows)
	  and getRowContents ([], 0, cells) = rev cells
	    | getRowContents ([], _, cells) =
		Error.error("insufficient cells in a row")
	    | getRowContents (_, 0, cells) =
		Error.error("excess cells in a row")
	    | getRowContents (M.ELEM{elem, body, ...}::r, n, cells) = let
		val (cell, colspan) = (case elem
		       of E.TH{colspan, align} => (TH{
			      colspan = colspan,
			      align = transAlign align,
			      contents = body
		            }, colspan)
			| E.TD{colspan, align} => (TD{
			      colspan = colspan, align = transAlign align,
			      contents = body
		            }, colspan)
			| _ => Error.bogusElem("table cell", elem)
		      (* end case *))
		val colspan = (case colspan
		       of NONE => 1
			| (SOME n) => n
		      (* end case *))
		in
		  getRowContents (r, n-colspan, cell::cells)
		end
	  in
	    { long = long, small = small, cols = cols, rows = getRows (rest, []) }
	  end

    fun getFloatContents (M.ELEM{elem=E.FLOAT{label, capalign}, body, ...}) = let
	  val (caption, rest) = (case body
		 of (M.ELEM{elem=E.CAPTION, body, ...}::r) => (body, r)
		  | _ => ([], body)
		(* end case *))
	  in
	    case rest
	     of [M.ELEM{elem=E.FIGURE{file}, ...}] => FIGURE{
		    label = label,
		    caption = caption,
		    capAlign = transCapAlign(false, capalign),
		    file = file
		  }
	      | [cd as M.ELEM{elem=E.CODE _, ...}] => CODE{
		    label = label,
		    caption = caption,
		    capAlign = transCapAlign(false, capalign),
		    body = cd
		  }
	      | [tblElem] => let
		  val {long, small, cols, rows} = getTableContents tblElem
		  in
		    TABLE{
			long = long,
			small = small,
			label = label,
			caption = caption,
			capAlign = transCapAlign(true, capalign),
			cols = cols,
			rows = rows
		      }
		  end
	    (* end case *)
	  end

  end;

