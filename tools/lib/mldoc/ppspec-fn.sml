(* ppspec-fn.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Pretty-print SML specifications and types to a PP stream.
 *
 * FIXME: need to add a hook for dealing with IdFamilies of structure names (e.g., Int{N}).
 *)

functor PPSpecFn (
    structure PPStrm : PP_STREAM
    structure Style : ML_STYLE
    sharing type PPStrm.style = Style.style
    sharing type PPStrm.token = Style.token
    val specBreak : bool -> PPStrm.device -> unit
  ) : PP_SPEC = struct

    structure PPStrm = PPStrm
    structure Style = Style

    structure PP = PPStrm
    structure Ty = MLDocType
    structure Sp = MLSpec
    structure M = MLDocMarkup

    type device = PP.device
    type stream = PP.stream
    type token = PP.token
    type context = Style.context

    type ml_type = MLSpec.ml_type

  (* print a list with separators *)
    fun ppList (ppItem, ppSep) ppStrm = let
	  fun pp [] = ()
	    | pp [x] = ppItem x
	    | pp (x::r) = (ppItem x; ppSep ppStrm; pp r)
	  in
	    pp
	  end

  (* add page-break candidate to the PP stream *)
    fun ppPageBreak (ppStrm, newline) = PP.control ppStrm (specBreak newline)

    fun ppKW kw ppStrm = PP.token ppStrm (Style.mkToken(Style.kwStyle, kw))
    fun ppPunct kw ppStrm = PP.token ppStrm (Style.mkToken(Style.punctStyle, kw))

    val pp_datatype = ppKW "datatype"
    val pp_end = ppKW "end"
    val pp_eqtype = ppKW "eqtype"
    val pp_exception = ppKW "exception"
    val pp_functor = ppKW "functor"
    val pp_include = ppKW "include"
    val pp_of = ppKW "of"
    val pp_sharing = ppKW "sharing"
    val pp_sig = ppKW "sig"
    val pp_signature = ppKW "signature"
    val pp_structure = ppKW "structure"
    val pp_type = ppKW "type"
    val pp_val = ppKW "val"
    val pp_where = ppKW "where"
    val pp_ARROW = ppKW "->"
    val pp_STAR = ppKW "*"
    val pp_BAR = ppPunct "|"
    val pp_COMMA = ppPunct ","
    val pp_COLON = ppKW ":"
    val pp_OPAQUE = ppKW ":>"
    val pp_EQ = ppPunct "="
    val pp_LP = ppPunct "("
    val pp_RP = ppPunct ")"
    val pp_LCB = ppPunct "{"
    val pp_RCB = ppPunct "}"
    val pp_UNIT = ppPunct "()"
    fun pp_ELLIPSE ppStrm = PP.token ppStrm (Style.mkToken(Style.itStyle, "..."))

    fun ppId (ctx, ppStrm) = (PP.token ppStrm) o (Style.idToToken ctx)
    fun ppId' (ppStrm, s) = PP.token ppStrm (Style.mkToken(Style.idStyle, s))
    fun ppStrId (ppStrm, id) = let
	  fun f (IdFamily.DATA s) = Style.mkToken(Style.idStyle, s)
	    | f (IdFamily.INDEX s) = Style.mkToken(Style.idIdxStyle, s)
	  in
	    List.app (PP.token ppStrm) (List.map f (IdFamily.parse id))
	  end

    fun ppTyVar ppStrm = let
	  val string = PP.string ppStrm
	  in
	    fn tyvar => (
		PP.pushStyle(ppStrm, Style.tyvarStyle);
		string tyvar;
		PP.popStyle ppStrm)
	  end

    fun ppTyParams (ppStrm, NONE) = ()
      | ppTyParams (ppStrm, SOME params) = let
	  val ppTyVar = ppTyVar ppStrm
	  val space = PP.space ppStrm
	  in
	    case Ty.scanTyParams params
	     of [] => ()
	      | [tv] => (ppTyVar tv; space 1)
	      | (tv :: r) => let
		  fun pp [] = (pp_RP ppStrm; space 1)
		    | pp (tv::r) = (pp_COMMA ppStrm; ppTyVar tv; pp r)
		  in
		    PP.openHBox ppStrm;
		      pp_LP ppStrm;
		      ppTyVar tv;
		      pp r;
		    PP.closeBox ppStrm
		  end
	    (* end case *)
	  end

  (* type precedences *)
    val atomPrec = 4
    val rArrowPrec = 3
    val lArrowPrec = 2
    val tuplePrec = 1
    val argPrec = 0

    fun ppType (ctx, ppStrm) = let
	  val ppId = ppId (ctx, ppStrm)
	  val string = PP.string ppStrm
	  val ppTyVar = ppTyVar ppStrm
	  fun space () = PP.space ppStrm 1
	  fun pp (_, Ty.VarTy tyVar) = ppTyVar tyVar
	    | pp (_, Ty.BaseTy([], id)) = ppId id
	    | pp (_, Ty.BaseTy([arg], id)) = (
		PP.openHBox ppStrm;
		  pp (argPrec, arg);
		  space ();
		  ppId id;
		PP.closeBox ppStrm)
	    | pp (_, Ty.BaseTy(arg1::args, id)) = let
		fun ppArgs [] = ()
		  | ppArgs (ty::tys) = (
		      pp_COMMA ppStrm;
		      space ();
		      pp (atomPrec, ty);
		      ppArgs tys)
		in
		  PP.openHVBox ppStrm (PP.Abs 2);
		    PP.openHBox ppStrm;
		      pp_LP ppStrm;
		      pp (atomPrec, arg1);
		      ppArgs args;
		      pp_RP ppStrm;
		    PP.closeBox ppStrm;
		    space ();
		    ppId id;
		  PP.closeBox ppStrm
		end
	    | pp (p, ty as Ty.FnTy(ty1, ty2)) =
		if (p < rArrowPrec)
		  then ppParen ty
		  else (
		    PP.openBox ppStrm (PP.Abs 2);
		      PP.openHBox ppStrm;
		        pp (lArrowPrec, ty1);
		      PP.closeBox ppStrm;
		      space ();
		      PP.openHBox ppStrm;
		        pp_ARROW ppStrm;
		        PP.nbSpace ppStrm 1;
		        pp (rArrowPrec, ty2);
		      PP.closeBox ppStrm;
		    PP.closeBox ppStrm)
	    | pp (_, Ty.TupleTy[]) = pp_UNIT ppStrm
	    | pp (p, Ty.TupleTy [ty]) = pp (p, ty)
	    | pp (p, ty as Ty.TupleTy(ty1::tys)) =
		if (p <= tuplePrec)
		  then ppParen ty
		  else let
		    fun ppTys [] = ()
		      | ppTys (ty::r) = (
			  space ();
			  PP.openHBox ppStrm;
		            pp_STAR ppStrm;
			    PP.nbSpace ppStrm 1;
			    pp (tuplePrec, ty);
			  PP.closeBox ppStrm;
			  ppTys r)
		    in
		      PP.openHVBox ppStrm (PP.Abs 0);
			pp (tuplePrec, ty1);
			ppTys tys;
		      PP.closeBox ppStrm
		    end
	    | pp (p, Ty.RecordTy ltys) = let
		fun ppLTy (lab, ty) = (
		      PP.openHBox ppStrm;
			string lab;
			PP.nbSpace ppStrm 1;
			pp_COLON ppStrm;
			PP.nbSpace ppStrm 1;
			pp (atomPrec, ty);
		      PP.closeBox ppStrm)
		fun ppLTys [] = ()
		  | ppLTys ([lty]) = ppLTy lty
		  | ppLTys (lty::r) = (
		      ppLTy lty;
		      pp_COMMA ppStrm;
		      space ();
		      ppLTys r)
		in
		  PP.openHVBox ppStrm (PP.Abs 0);
		    PP.openHVBox ppStrm (PP.Abs 2);
		      pp_LCB ppStrm;
		      PP.cut ppStrm;
		      ppLTys ltys;
		    PP.closeBox ppStrm;
		    PP.cut ppStrm;
		    pp_RCB ppStrm;
		  PP.closeBox ppStrm
		end
	    | pp (_, Ty.ParenTy ty) = ppParen ty
	  and ppParen ty = (
		pp_LP ppStrm;
		pp (atomPrec, ty);
		pp_RP ppStrm)
	  in
	    fn (ty : ml_type) => (
		PP.openHBox ppStrm;
		  pp (atomPrec, ty);
		PP.closeBox ppStrm)
	  end

  (* pretty-print a list of where type specification.  Note that this
   * function emits a newline prior to the first specification.
   *)
    fun ppWhereTys (ctx, ppStrm) = let
	  val space = PP.space ppStrm
	  val ppType = ppType (ctx, ppStrm)
	  fun pp (Sp.WHEREty{params, id, def}) = (
		PP.newline ppStrm;
		PP.openHBox ppStrm;
		  pp_where ppStrm;
		  space 1;
		  pp_type ppStrm;
		  space 1;
		  ppTyParams (ppStrm, params);
		  ppId' (ppStrm, id);
		  space 1;
		  pp_EQ ppStrm;
		  space 1;
		  PP.openHOVBox ppStrm (PP.Rel 0);
		    ppType def;
		  PP.closeBox ppStrm;
		PP.closeBox ppStrm)
	  in
	    List.app pp
	  end

    fun ppInclSpec (ctx, ppStrm, doWhereTys) = let
	  val ppSigId = ppId (ctx, ppStrm)
	  val space = PP.space ppStrm
	  val ppWhereTys = ppWhereTys (ctx, ppStrm)
	  fun ppIncl id = (
		PP.openHBox ppStrm;
		  pp_include ppStrm;
		  space 1;
		  ppSigId id;
		PP.closeBox ppStrm)
	  fun simplePP (id, _) = ppIncl id;
	  fun pp (id, []) = simplePP (id, [])
	    | pp (id, whereTys) = (
		PP.openVBox ppStrm  (PP.Abs 2);
		  ppIncl id;
		  ppWhereTys whereTys;
		PP.closeBox ppStrm)
	  in
	    if doWhereTys then app pp else app simplePP
	  end

    fun ppStrSpec (ctx, ppStrm, doWhereTys) = let
	  val ppSigId = ppId (ctx, ppStrm)
	  val space = PP.space ppStrm
	  val ppWhereTys = ppWhereTys (ctx, ppStrm)
	  fun ppStr (id, sigId) = (
		PP.openHBox ppStrm;
		  pp_structure ppStrm;
		  space 1;
		  ppStrId (ppStrm, id);
		  space 1;
		  pp_COLON ppStrm;
		  space 1;
		  ppSigId sigId;
		PP.closeBox ppStrm)
	  fun pp (id, sigId, whereTys) =
		if (not doWhereTys orelse List.null whereTys)
		  then ppStr (id, sigId)
		  else (
		    PP.openVBox ppStrm  (PP.Abs 2);
		      ppStr (id, sigId);
		      ppWhereTys whereTys;
		    PP.closeBox ppStrm)
	  in
	    pp
	  end

    fun ppSharingSpec (ctx, ppStrm) = let
	  val space = PP.space ppStrm
	  fun ppTys (id, r) = (
		PP.openBox ppStrm (PP.Abs 2);
		  ppId' (ppStrm, id);
		  app
		    (fn id => (
		      space 1;
		      PP.openHBox ppStrm;
		        pp_EQ ppStrm; space 1; ppId' (ppStrm, id);
		      PP.closeBox ppStrm))
		    r;
		PP.closeBox ppStrm)
	  fun pp spec = (
		PP.openHBox ppStrm;
		  pp_sharing ppStrm;
		  space 1;
		  case spec
		   of (Sp.STRshare(id::r)) => ppTys(id, r)
		    | (Sp.TYshare(id::r)) => (
			pp_type ppStrm;
			space 1;
			ppTys(id, r))
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    fn specs => (
		PP.openVBox ppStrm (PP.Abs 0);
		  ppList (pp, PP.cut) ppStrm specs;
		PP.closeBox ppStrm)
	  end

    fun ppTySpec (ctx, ppStrm) = let
	  val space = PP.space ppStrm
	  fun pp {eq, params, id, def} = (
		PP.openHBox ppStrm;
		  if eq then pp_eqtype ppStrm else pp_type ppStrm;
		  space 1;
		  ppTyParams (ppStrm, params);
		  PP.token ppStrm (Style.descRef(ctx, Style.TyRef, id));
		  case def
		   of NONE => ()
		    | (SOME ty) => (
			space 1;
			pp_EQ ppStrm;
			space 1;
			PP.openHOVBox ppStrm (PP.Rel 0);
			  ppType (ctx, ppStrm) ty;
			PP.closeBox ppStrm)
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    fn specs => (
		PP.openVBox ppStrm (PP.Abs 0);
		  ppList (pp, PP.cut) ppStrm specs;
		PP.closeBox ppStrm)
	  end

    fun ppDTLHS (ctx, ppStrm) (params, id) = (
	  PP.openHBox ppStrm;
	    pp_datatype ppStrm;
	    PP.space ppStrm 1;
	    ppTyParams (ppStrm, params);
	    PP.token ppStrm (Style.descRef(ctx, Style.TyRef, id));
	  PP.closeBox ppStrm)

    fun ppConsSpec (ctx, ppStrm) = let
	  val space = PP.space ppStrm
	  fun pp {isFirst, id, ty} = (
		PP.openHBox ppStrm;
		  if isFirst then pp_EQ ppStrm else pp_BAR ppStrm;
		  space 1;
		  PP.token ppStrm (Style.descRef(ctx, Style.ConRef, id));
		  case ty
		   of NONE => ()
		    | (SOME ty) => (
		        space 1;
		        pp_of ppStrm;
		        space 1;
		        ppType (ctx, ppStrm) ty)
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    pp
	  end

    fun ppDTSpec (ctx, ppStrm) specs = let
	  val space = PP.space ppStrm
	  val ppLHS = ppDTLHS (ctx, ppStrm)
	  fun pp {compact = false, params, id, cons as _::_::_} = let
		val ppCons = ppConsSpec (ctx, ppStrm)
		fun ppCons' ((id, optTy, comment), isFirst) = (
		      if not isFirst then space 1 else ();
		      ppCons {isFirst = isFirst, id = id, ty = optTy};
		      false)
		in
		  PP.openVBox ppStrm (PP.Abs 2);
		    ppLHS (params, id);
		    PP.newline ppStrm;
		    ignore (foldl ppCons' true cons);
		  PP.closeBox ppStrm
		end
	    | pp {params, id, cons, ...} = let
		fun ppCons' ((id, optTy, _), isFirst) = (
		      space 1;
		      PP.openHBox ppStrm;
		        if isFirst then pp_EQ ppStrm else pp_BAR ppStrm;
			space 1;
			PP.token ppStrm (Style.descRef(ctx, Style.ConRef, id));
			case optTy
			 of NONE => ()
			  | (SOME ty) => (
			      space 1;
			      pp_of ppStrm;
			      space 1;
			      ppType (ctx, ppStrm) ty)
			(* end case *);
		      PP.closeBox ppStrm;
		      false)
		in
		  PP.openHBox ppStrm;
		    ppLHS (params, id);
		    PP.openHVBox ppStrm (PP.Abs 2);
		      ignore (foldl ppCons' true cons);
		    PP.closeBox ppStrm;
		  PP.closeBox ppStrm
		end
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      ppList (pp, PP.cut) ppStrm specs;
	    PP.closeBox ppStrm
	  end

    fun ppDTDefSpec (ctx, ppStrm) specs = let
	  val space = PP.space ppStrm
	  val ppId = ppId (ctx, ppStrm)
	  fun pp {id, def} = (
		PP.openHBox ppStrm;
		  pp_datatype ppStrm;
		  space 1;
		  PP.token ppStrm (Style.descRef(ctx, Style.TyRef, id));
		  space 1;
		  pp_EQ ppStrm;
		  space 1;
		  pp_datatype ppStrm;
		  space 1;
		  ppId def;
		PP.closeBox ppStrm)
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      ppList (pp, PP.cut) ppStrm specs;
	    PP.closeBox ppStrm
	  end

    fun ppExnSpec (ctx, ppStrm) specs = let
	  val space = PP.space ppStrm
	  fun pp (exnId, optTy) = (
		PP.openHBox ppStrm;
		  pp_exception ppStrm;
		  space 1;
		  PP.token ppStrm (Style.descRef(ctx, Style.ExnRef, exnId));
		  case optTy
		   of NONE => ()
		    | (SOME ty) => (
			space 1;
			pp_of ppStrm;
			space 1;
			ppType (ctx, ppStrm) ty)
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      ppList (pp, PP.cut) ppStrm specs;
	    PP.closeBox ppStrm
	  end

    fun ppValSpec (ctx, ppStrm, align) = let
	  val ppTy = ppType (ctx, ppStrm)
	  val nbSpace = PP.nbSpace ppStrm
	  val space = PP.space ppStrm
	  val token = PP.token ppStrm
	  fun pp (l : (string * ml_type * M.markup list) list) = let
		val ppId = if align
		      then let
			val maxWid =
			      1 +
			      List.foldl
				(fn ((s, _, _), w) => Int.max(w, size s)) 0 l
			in
			  fn id => (
			      token (Style.descRef(ctx, Style.ValRef, id));
			      nbSpace (maxWid - size id))
			end
		      else (fn id => (token (Style.descRef(ctx, Style.ValRef, id)); nbSpace 1))
		fun ppSpec (id, ty, _) = (
		      PP.openHBox ppStrm;
		        pp_val ppStrm;
			space 1;
			ppId id;
			pp_COLON ppStrm;
			space 1;
			PP.openHOVBox ppStrm (PP.Rel 0);
			  ppTy ty;
			PP.closeBox ppStrm;
		      PP.closeBox ppStrm)
		in
		  PP.openVBox ppStrm (PP.Abs 0);
		    ppList (ppSpec, PP.cut) ppStrm l;
		  PP.closeBox ppStrm
		end
	  in
	    pp
	  end

    fun ppStrSigSpec (ctx, ppStrm) = let
	  val space = PP.space ppStrm
	  val nbSpace = PP.nbSpace ppStrm
	  val ppWhereTys = ppWhereTys (ctx, ppStrm)
	  fun ppStr (id, sigSpecs) = let
		val ctx = Style.extendContext (ctx, id)
		val ppSpecList = ppSpecList (ctx, ppStrm, true)
		in
		  PP.openVBox ppStrm (PP.Abs 2);
		    pp_structure ppStrm;
		    nbSpace 1;
		    PP.token ppStrm (Style.descRef(ctx, Style.StrRef, id));
		    nbSpace 1;
		    pp_COLON ppStrm;
		    nbSpace 1;
		    pp_sig ppStrm;
		    PP.openVBox ppStrm (PP.Abs 2);
		      PP.cut ppStrm;
		      ppSpecList sigSpecs;
		    PP.closeBox ppStrm;
		    PP.cut ppStrm;
		    pp_end ppStrm;
		  PP.closeBox ppStrm
		end
	  in
	    ppStr
	  end

    and ppSpec' (ctx, ppStrm, align) (spec, _) = (case spec
	   of (Sp.BRspec newline) => ppPageBreak (ppStrm, newline)
	    | (Sp.INCLspec specs) => ppInclSpec (ctx, ppStrm, true) specs
	    | (Sp.STRspec specs) => ppStrSpec (ctx, ppStrm, true) specs
	    | (Sp.STRSIGspec specs) => ppStrSigSpec (ctx, ppStrm) specs
	    | (Sp.SHARINGspec specs) => ppSharingSpec (ctx, ppStrm) specs
	    | (Sp.EXNspec specs) => ppExnSpec (ctx, ppStrm) specs
	    | (Sp.TYspec specs) => ppTySpec (ctx, ppStrm) specs
	    | (Sp.DTspec specs) => ppDTSpec (ctx, ppStrm) specs
	    | (Sp.DTDEFspec specs) => ppDTDefSpec (ctx, ppStrm) specs
	    | (Sp.VALspec specs) => ppValSpec (ctx, ppStrm, align) specs
	  (* end case *))

  (* pretty print a list of specs with cuts between them; we are careful to avoid
   * excess cuts around BRspecs.
   *)
    and ppSpecList (ctx, ppStrm, align) = let
	  val ppSpec = ppSpec' (ctx, ppStrm, align)
	  fun pp [] = ()
	    | pp [spc] = ppSpec spc
	    | pp (spc :: (r as (Sp.BRspec newline, _) :: _)) = (
		ppSpec spc;
		if newline then PP.cut ppStrm else ();
		pp r)
	    | pp (spc::r) = (ppSpec spc; PP.cut ppStrm; pp r)
	  in
	    pp
	  end

    fun ppSpecs (ctx, ppStrm, align) specs = let
	  val ppSpecList = ppSpecList (ctx, ppStrm, align)
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      ppSpecList specs;
	    PP.closeBox ppStrm
	  end

  (* the export version of ppSpec *)
    fun ppSpec (ctx, ppStrm, align) spec = ppSpec' (ctx, ppStrm, align) (spec, [])

    fun ppSignature (ppStrm, sigId) = (
	  PP.openHBox ppStrm;
	    pp_signature ppStrm;
	    PP.space ppStrm 1;
	    ppId' (ppStrm, sigId);
	  PP.closeBox ppStrm)

    fun ppStructure (ctx, ppStrm) (strId, opaque, optSigId, whereTys) = let
	  fun pp () = (
		PP.openHBox ppStrm;
		  pp_structure ppStrm;
		  PP.space ppStrm 1;
		  ppStrId (ppStrm, strId);
		  PP.space ppStrm 1;
		  if opaque then pp_OPAQUE ppStrm else pp_COLON ppStrm;
		  PP.space ppStrm 1;
		  case optSigId
		   of NONE => (
			pp_sig ppStrm; PP.space ppStrm 1;
			pp_ELLIPSE ppStrm; PP.space ppStrm 1;
			pp_end ppStrm)
		    | (SOME id) => ppId' (ppStrm, id)
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    case whereTys
	     of [] => pp()
	      | _ => (
		PP.openVBox ppStrm (PP.Abs 2);
		  pp();
		  ppWhereTys (ctx, ppStrm) whereTys;
		PP.closeBox ppStrm)
	    (* end case *);
	    PP.cut ppStrm
	  end
(*DEBUG*)handle ex => raise ex

    fun ppFunctor (ctx, ppStrm) (fctId, optArgId, opaque, optResId, whereTys) = let
	  fun pp () = (
		PP.openHBox ppStrm;
		  pp_functor ppStrm;
		  PP.space ppStrm 1;
		  ppId' (ppStrm, fctId);
		  PP.space ppStrm 1;
		  pp_LP ppStrm;
		  case optArgId
		   of NONE => pp_ELLIPSE ppStrm
		    | (SOME id) => ppId' (ppStrm, id)
		  (* end case *);
		  pp_RP ppStrm;
		  if opaque then pp_OPAQUE ppStrm else pp_COLON ppStrm;
		  PP.space ppStrm 1;
		  case optResId
		   of NONE => (
			pp_sig ppStrm; PP.space ppStrm 1;
			pp_ELLIPSE ppStrm; PP.space ppStrm 1;
			pp_end ppStrm)
		    | (SOME id) => ppId' (ppStrm, id)
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    case whereTys
	     of [] => pp()
	      | _ => (
		PP.openVBox ppStrm (PP.Abs 2);
		  pp();
		  ppWhereTys (ctx, ppStrm) whereTys;
		PP.closeBox ppStrm)
	    (* end case *);
	    PP.cut ppStrm
	  end

    val ppKW = fn (ppStrm, kw) => ppKW kw ppStrm
    val ppPunct = fn (ppStrm, punct) => ppPunct punct ppStrm

  end;

