(* mldoc-kind.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

structure MLDocKind :> sig

    datatype elem_kind = LIST | BLOCK | FLOAT | OTHER

    val kind : MLDocMarkup.markup -> elem_kind

  end = struct

    structure M = MLDocMarkup
    structure E = MLDocElem

    datatype elem_kind = LIST | BLOCK | FLOAT | OTHER

    fun kind (M.ELEM{elem=E.ITEMIZE, body, ...}) = LIST
      | kind (M.ELEM{elem=E.ENUM, body, ...}) = LIST
      | kind (M.ELEM{elem=E.DESCRIP, body, ...}) = LIST
      | kind (M.ELEM{elem=E.DISPLAYMATH, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.EQNARRAY, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.FLOAT _, body, ...}) = FLOAT
      | kind (M.ELEM{elem=E.TABLE _, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.EXAMPLE, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.QUESTION, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.IMPLNOTE, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.SYSNOTE _, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.RATIONALE, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.CODE _, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.GRAMMAR, body, ...}) = BLOCK
      | kind (M.ELEM{elem=E.REGEXP, body, ...}) = BLOCK
      | kind elem = OTHER

  end
