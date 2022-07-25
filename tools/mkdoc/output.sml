(* output.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * Queries:
 * top level declarations
 * optional
 * infix
 * open
 * where type
 *)

structure Output :
  sig

    datatype spec = None
                  | Str of (string * bool)    
                  | Ftr of (string * string * string * bool)    
                  | SigI of (string * bool) list   

    val outSGML : {
            specs : spec,
	    findComments : (int * int) -> string list,
	    infile : string option,
	    fname : string,
	    version : string,
	    copyright : string,
	    outs : TextIO.outstream,
	    dec : Ast.dec
	  } -> unit

  end = struct

    structure SS = Substring
    structure F = Format
    structure LF = ListFormat
    structure Sym = Symbol
    structure Ast = Ast
    fun empty _ = NONE
    fun prologue copyright = concat [
	    "\
            \\n\
            \<!DOCTYPE ML-DOC SYSTEM>\n\
            \\n\
            \<COPYRIGHT OWNER=\"",
	    copyright,
	    "\" YEAR=%d>\n"
	  ]

    datatype spec = None
                  | Str of (string * bool)    
                  | Ftr of (string * string * string * bool)    
                  | SigI of (string * bool) list   

    val translateFileEntity = String.translate (
	   fn #"_" => "-"
	    | c => String.str c)

    datatype module_kind = STRUCT | SIG | FUNCT
    fun doPrologue (outf, version, copyright, infile, fname, moduleName) = let
	  val date = Date.fromTimeLocal (Time.now())
          fun intOf Date.Jan = 1
            | intOf Date.Feb = 2
            | intOf Date.Mar = 3
            | intOf Date.Apr = 4
            | intOf Date.May = 5
            | intOf Date.Jun = 6
            | intOf Date.Jul = 7
            | intOf Date.Aug = 8
            | intOf Date.Sep = 9
            | intOf Date.Oct = 10
            | intOf Date.Nov = 11
            | intOf Date.Dec = 12
          val fmt = "<VERSION VERID=\"%s\" YEAR=%d MONTH=%d DAY=%d>\n"
          val (name,kind) = moduleName
	   in
	   app outf ["<!-- ", fname, " -->\n"];
(*** I'm commenting this out for now.  I think that it should
 *** be controlled by a flag, that would also specify the Entities file
 *** to append the decl to.  Furthermore, we should extract the base
 *** name of the infile.
	   (case infile of    
	       NONE => ()
	     | (SOME f) =>
		   app outf ["<!-- Entities.sgml entry \n",
			     "<!ENTITY ",translateFileEntity name,
			     " SDATA \"",  f,"\">\n",
			     " -->\n"]);
 ***)
            F.formatf (prologue copyright) outf [F.INT(Date.year date)];
            F.formatf fmt outf [F.STR version, F.INT(Date.year date), 
              F.INT(intOf(Date.month date)), F.INT(Date.day date)];
            app outf ["<TITLE>The ", name, " ", kind, "</TITLE>\n"];
            outf "\n<INTERFACE>\n";
            app outf ["<HEAD>The <CD/", name, "/ ", kind, "</HEAD>\n"];
            outf "<!-- optional SEEALSO; uncomment to use     -->\n";
            outf "<!-- <SEEALSO>    -->\n";
            outf "<!--   non-empty list of XREFS here   -->\n";
            outf "<!-- </SEEALSO>    -->\n";
            outf "\n<PP>\n";
	    outf "<!-- Some general introductory text -->\n\n"
          end
          
    val epilogue = [
	  "\n</INTERFACE>\n"
        ]

    fun doEpilogue outf = app outf epilogue

    datatype decls = DECLS of {
        sigs : Ast.sigb list,
        strs : Ast.strb list,
        functs : Ast.fctb list
      }

      (* Convert an ast into a decls : lists of signatures,
       * structures and functors. Strip away high-level marks.
       *)
    fun normalize dec = let
          open Ast
          fun stripFct (MarkFctb(fctb,_)) = stripFct fctb
            | stripFct fb = fb
          fun stripStr (MarkStrb(strb,_)) = stripStr strb
            | stripStr sb = sb
          fun stripSig (MarkSigb(sigb,_)) = stripSig sigb
            | stripSig sb = sb
          fun norml (MarkDec(d,_),l) = norml(d,l)
            | norml (StrDec bl,(sgs,strs,fns)) = 
                (sgs,List.revAppend(map stripStr bl,strs),fns)
            | norml (FctDec bl,(sgs,strs,fns)) = 
                (sgs,strs,List.revAppend(map stripFct bl,fns))
            | norml (SigDec bl,(sgs,strs,fns)) = 
                (List.revAppend(map stripSig bl,sgs),strs,fns)
            | norml (SeqDec dl,l) = List.foldl norml l dl
            | norml _ = Error.error "Non-module declaration\n"
          val (sgs,strs,fns) = norml(dec,([],[],[]))
          in
            DECLS{sigs = rev sgs, strs = rev strs, functs = rev fns}
          end
     
      (* moduleName
       * Heuristic - if a single structure/functor is declared, use
       * its name.
       * Otherwise, use the name of the first signature.
       * Complain if functors are declared, or if there are
       * multiple structures and no signatures.
       *)
    fun moduleName (DECLS{functs = [Ast.Fctb{name,...}],strs=[],...}) =
            (Sym.name name, "functor")
      | moduleName (DECLS{strs = [Ast.Strb{name,...}],functs=[],...}) =
            (Sym.name name, "structure")
      | moduleName (DECLS{sigs = (Ast.Sigb{name,...})::_,...}) =
            (Sym.name name, "signature")
      | moduleName _ = Error.error "No signatures defined\n"

    datatype sigexp_type = SIGNAME of Sym.symbol * Ast.wherespec list
                         | SIGBODY of Ast.spec list

    fun functArgs (Ast.BaseFct{params,constraint,...}) = (params, constraint)
      | functArgs (Ast.MarkFct(f,m)) = functArgs f
      | functArgs _ = Error.error "Only functor definitions supported"

    fun getSigType s = let
          fun getst (Ast.VarSig s,l) = SIGNAME (s, List.concat (rev l))
            | getst (Ast.MarkSig (se,_),l) = getst (se,l)
            | getst (Ast.BaseSig specs,l) = (
                if l = [] then ()
                else Error.warning ("where clauses on signature body: ignored",[]);
                SIGBODY specs)
            | getst (Ast.AugSig (se,wh),l) = getst (se,wh::l)
          in
            getst (s,[])
          end

    fun tyVarStr (Ast.Tyv sym) = Sym.name sym
      | tyVarStr (Ast.MarkTyv (tyvar,_)) = tyVarStr tyvar	

    val arrowTycon = Sym.tycSymbol "->"

    fun isFunction (Ast.ConTy ([sym], [_,_])) = Sym.eq(sym,arrowTycon)
      | isFunction (Ast.ConTy _) = false
      | isFunction (Ast.MarkTy (ty,_)) = isFunction ty
      | isFunction _ = false

    val longStr = LF.fmt {init="",sep=".",final="",fmt=Sym.name}

    fun tyStr (Ast.VarTy tyvar) = tyVarStr tyvar
      | tyStr (Ast.ConTy (syml, tyl)) = let
          fun tyConStr ([sym], tys as [lhs,rhs]) =
                if Sym.eq(sym,arrowTycon)
                  then if isFunction lhs
                    then concat["(", tyStr lhs, ") -> ", tyStr rhs]
                    else concat[tyStr lhs, " -> ", tyStr rhs]
                  else concat[typarmsStr tys, " ", Sym.name sym]
            | tyConStr (syms, []) = longStr syms
            | tyConStr (syms, [typ]) = 
                if isFunction typ
                  then concat["(", tyStr typ, ") ", longStr syms]
                  else concat[tyStr typ, " ", longStr syms]
            | tyConStr (syms, typs) = concat[typarmsStr typs," ",longStr syms]
          in
            tyConStr(syml,tyl)
          end
      | tyStr (Ast.RecordTy l) = let
          fun fmt (sym,ty) = concat[Sym.name sym, " : ", tyStr ty]
          in
            LF.fmt {init="{",sep=", ",final="}", fmt=fmt} l
          end
      | tyStr (Ast.TupleTy l) = let
          fun tupStr ty =
                if isFunction ty 
                  then concat["(",tyStr ty,")"]
                  else tyStr ty
          in
            LF.fmt {init="(",sep=" * ",final=")", fmt=tupStr} l
          end
      | tyStr (Ast.MarkTy (ty,_)) = tyStr ty
    and typarmsStr [] = ""
      | typarmsStr [typ] = tyStr typ
      | typarmsStr pl = LF.fmt {init="(",sep=",",final=")",fmt=tyStr} pl

    fun tyvarsStr [] = ""
      | tyvarsStr [tyv] = tyVarStr tyv
      | tyvarsStr pl = LF.fmt {init="(",sep=",",final=")",fmt=tyVarStr} pl

    fun wrapArg s = concat["<ARG>", s, "</ARG>"]

    val suf = #["2","3","4","5","6","7","8","9"]
    val tyVar = #["a","b","c"]
    val funVar = #["f","g","h"]
    val argsList = [
          ("list", #["l"]),
          ("int", #["i","j","k"]),
          ("string", #["s","t"]),
          ("char", #["c","d"]),
          ("real", #["r","t","x","y"]),
          ("file_desc", #["fd"]),
          ("offset", #["off"]),
          ("bool", #["b"]),
          ("option", #["opt"]),
          ("vector", #["vec"]),
          ("array", #["arr"])
        ]
      (* This list is used to specify that the second type uses the
       * same argument generator as the first type.
       *)
    val dupList = [
          ("int", "word"),
          ("string", "substring"),
          ("real", "float")
        ]

      (* Given a vector of parameter names, genArg returns a function
       * that on each invocation, returns the next name. When all names
       * are used up, additional calls to the function produce a name
       * which is the first entry in the vector with a suffix.
       *)
    fun genArg v = let
          val idx = ref 0
          val sidx = ref 0
          val l = Vector.length v
          val v0 = Vector.sub(v,0)
          fun g() = let
            val i = !idx
            val si = !sidx
            in
              if i < l 
                then wrapArg(Vector.sub(v,i)) before idx := i + 1
                else wrapArg(v0^Vector.sub(suf,si)) before sidx := si + 1
            end
          in g end

    fun unitArg () = wrapArg "()"

    structure ArgTable = HashTableFn (
      struct
	type hash_key = string
	val hashVal = HashString.hashString
	fun sameKey (s,s' : string) = s = s'
      end)

      (* Make a typical argument name from a type constructor name.
       * If the type constructor has an underscore in it, we use
       * the first character, and the first character after the underscore.
       * Thus, for file_desc, we get fd.
       * Otherwise, we use the first two characters.
       *)
    fun mkFun name = let
          fun is_ c = c = #"_"
          val (first,rest) = SS.splitl (not o is_) (SS.full name)
          val (_,second) = SS.splitl is_ rest
          val sym =
                case (SS.first first, SS.first second) of
                  (SOME f, SOME s) => implode[f,s]
                | _ => if size name > 1 then substring(name,0,2)
                       else substring(name,0,1)
          in
            genArg (#[sym])
          end

    fun argStr ty = let
          val tyvArg = genArg tyVar
          val funArg = genArg funVar
          val argTbl = ArgTable.mkTable (32, Fail "argTable")
          val findArg = ArgTable.find argTbl
          val lookup = ArgTable.lookup argTbl
          val insArg = ArgTable.insert argTbl

          fun argOf sym =
                case Sym.name sym of
                  "unit" => unitArg ()
                | name =>
                    case findArg name of
                      SOME f => f()
                    | NONE => let
                        val f = mkFun name
                        in
                          insArg (name, f);
                          f()
                        end

          fun args (Ast.VarTy _) = tyvArg()
            | args (Ast.ConTy (syml, tyl)) = let
                val sym = List.last syml
                fun doFunction [lhs,rhs] = let
                      val larg = if isFunction lhs then funArg() else args lhs
                      in
                        if isFunction rhs
                          then concat [larg," ",args rhs]
                          else larg
                      end
                  | doFunction _ = Error.error "internal error: doFunction\n"
                in
                  if Sym.eq(sym,arrowTycon)
                    then doFunction tyl
                    else argOf sym
                end
            | args (Ast.RecordTy l) = let
                fun fmt (sym,_) = wrapArg (Sym.name sym)
                in
                  LF.fmt {init="{",sep=", ",final="}", fmt=fmt} l
                end
            | args (Ast.TupleTy l) = let
                fun tupArgs ty = if isFunction ty then funArg () else args ty
                in
                  LF.fmt {init="(",sep=", ",final=")", fmt=tupArgs} l
                end
            | args (Ast.MarkTy (ty,_)) = args ty
          in
            app (fn (s,v) => insArg(s, genArg v)) argsList;
            app (fn (s,t) => insArg (t, lookup s)) dupList;
            args ty
          end

    val indent = 2
    fun mkIndent (0,l) = implode l
      | mkIndent (i,l) = mkIndent(i-1,#" "::l)
    val inds = mkIndent(indent,[])
    fun doInd outf 0 = ()
      | doInd outf i = (outf inds; doInd outf (i-1))

    fun outWheres (_,_,[]) = ()
      | outWheres (outf,ind,wheres) = let
          fun outw (Ast.WhType(syms,tyvars,ty)) = (
                doInd outf ind;
                outf "<WHERETYPE>";
                outf (tyvarsStr tyvars);
                outf "<ID>";
                outf (longStr syms);
                outf "</ID>";
                outf "<TY>";
                outf (tyStr ty);
                outf "</WHERETYPE>";
                outf "\n"
              )
            | outw (Ast.WhStruct(syms,lsyms)) = (
                Error.warning ("where struct clauses unsupported: ignored",[])
              )
          in
            outf "\n";
            app outw wheres
          end


    fun outSigBody {outf,find} (sigid, file, specs) = let
	   open Ast

	   fun mergeMarks (VarTy _,a) = a
	    | mergeMarks (ConTy (_,tys),a) =
	      List.foldl mergeMarks a tys
	    | mergeMarks (RecordTy vs,a) =
	      List.foldl (fn ((_,ty),a) => mergeMarks (ty,a)) a vs
	    | mergeMarks (TupleTy ty,a) =
	      List.foldl mergeMarks a ty
	    | mergeMarks (MarkTy(ty,(s,e)),SOME (s',e')) =
	      let
		  val ns = Int.min(s,s')
		  val ne = Int.max(e,e')
	      in
		  mergeMarks(ty,SOME (ns,ne))
	      end
	     | mergeMarks (MarkTy(ty,r),NONE) = mergeMarks(ty,SOME r)

	  val doInd = doInd outf

          fun doExe ind (name,NONE) =
                (doInd ind; app outf ["<EXN>",Sym.name name,"\n"])
            | doExe ind (name,SOME ty) =
                (doInd ind; app outf ["<EXN>",Sym.name name,"<TY>",tyStr ty,"\n"])
          fun doSpec ind = (doInd ind; outf "<SPEC>\n")

          fun doProto ind (name,ty) = (
                doInd ind; 
                outf "<PROTOTY>\n";
                doInd ind; 
                outf name;
                if isFunction ty
                  then (outf " "; outf (argStr ty))
                  else ();
                outf "\n";
                doInd ind; 
                outf "</PROTOTY>\n"
              )

	  val translateVal = String.translate (
		 fn #"&" => "&AMP;"
		  | #"<" => "&LT;"
		  | c => String.str c)

          fun doVal ind (sym,ty) = let
                val name = translateVal(Sym.name sym)
		val mrk = (mergeMarks(ty,NONE))
                in
                  doInd ind; 
                  app outf ["<VAL>", name, "<TY>", tyStr ty,"\n"];
                  doInd (ind+1); 
                  outf "<COMMENT>\n";
                  doProto (ind+2) (name,ty);
                  doInd (ind+2);
		  outf "<PP>\n";
                  doInd (ind+2);
		  case (Option.map find mrk) of
		     NONE =>
			  app outf ["explain the use and semantics of ",
				    name," HERE.\n"]
		   | SOME [] =>
			 app outf ["explain the use and semantics of ",
				    name," HERE.\n"]
		   | SOME s => (outf "<!-- ";
				app outf s; outf "-->\n")
		  (* end case *);
		  doInd (ind+1);
                  outf "</COMMENT>\n"
                end

          fun doTypeVars [] = ()
            | doTypeVars tvl = (outf "<TYPARAM>"; outf (tyvarsStr tvl))

          fun doCons ind (sym,tyopt) = (
                doInd ind;
                outf "<CONS>";
                outf (Sym.name sym);
                case tyopt of
                  NONE => ()
                | SOME ty => (outf "<TY>"; outf(tyStr ty));
                outf "\n"
              )

          fun doDataty ind (Db{tyc, tyvars, rhs=Constrs def, ...}) = (
                doInd ind;
                outf "<DATATYPE>";
                doTypeVars tyvars;
                outf "<ID>";
                outf (Sym.name tyc);
                outf "\n";
                app (doCons (ind+1)) def;
                doInd ind;
                outf "</DATATYPE>\n"
              )
            | doDataty ind (Db{tyc, tyvars, rhs=Repl syms, ...}) = (
                doInd ind;
                outf "<DATATYPEDEF>";
                outf "<ID>";
                outf (Sym.name tyc);
                outf "</ID>";
                outf (longStr syms);
                outf "</DATATYPEDEF>\n"
              )
            | doDataty ind (MarkDb (db,_)) = doDataty ind db

          fun doTyc (ind,eq) = let
                val kind = if eq then "<EQTYPE>" else "<TYPE>"
                in
                  fn (sym,tvl,tyabbrev) => (
                    doInd ind;
                    outf kind;
                    doTypeVars tvl;
                    outf "<ID>";
                    outf (Sym.name sym);
                    case tyabbrev of 
                      SOME ty => (outf "<TY>"; outf(tyStr ty))
                    | NONE => ();
                    outf "\n"
                  )
                end

          fun doSharing (ind, isType, sl) = (
                doInd ind;
		outf "<SPEC>\n";
                doInd (ind+1);
                if isType then outf "<SHARING TYPE>\n" else outf "<SHARING>\n";
                doInd (ind+2);
                outf (LF.fmt {init="",sep=" <EQU> ",final="",fmt=longStr} sl);
                outf "\n";
                doInd (ind+1);
                outf "</SHARING>\n"
              )

          fun doSubstr ind (sym,se,qid) = (
                if qid = NONE then ()
                else 
                  Error.warning("structure spec ID : SIG = LONGID not supported\n", []);
             
                doInd ind;
                outf "<SUBSTRUCT>";
                outf (Sym.name sym);
                case getSigType se of
                  SIGNAME (sigsym,[]) =>
                    app outf ["<ID>",Sym.name sigsym,"</SUBSTRUCT>\n"]
                | SIGNAME (sigsym,wheres) => (
                    app outf ["<ID>",Sym.name sigsym];
                    outWheres (outf,ind+1,wheres);
                    doInd ind;
                    outf "</SUBSTRUCT>\n")
                | SIGBODY specl => (
                    outf "\n";
                    doInd (ind+1);
                    outf "<SIGBODY>\n";
                    app (outSpec (ind+1)) specl;
                    doInd (ind+1);
                    outf "</SIGBODY>\n";
                    doInd ind;
                    outf "</SUBSTRUCT>\n"
                  )
              )

          and outSpec ind (StrSpec slist) = (
                doSpec ind;
                app (doSubstr(ind+1)) slist
              )
            | outSpec ind (TycSpec (tl,eq)) = (
                doSpec ind;
                app (doTyc (ind+1, eq)) tl
              )
            | outSpec ind (ValSpec vlist) = (
                doSpec ind;
                app (doVal(ind+1)) vlist
              )
            | outSpec ind (DataSpec {datatycs, withtycs}) = (
                case withtycs of 
                  [] => ()
                |  _ => Error.warning("with types not supported\n", []);
                doSpec ind;
                app (doDataty(ind+1)) datatycs
              )
            | outSpec ind (ExceSpec elist) = (
                doSpec ind;
                app (doExe(ind+1)) elist
              )
            | outSpec ind (IncludeSpec sigExp) = let
		fun getWhereSpecs (VarSig sym, wsl) = (sym, wsl)
		  | getWhereSpecs (AugSig(sigExp, ws), wsl) =
		      getWhereSpecs (sigExp, ws@wsl)
		  | getWhereSpecs (BaseSig _, _) = raise Fail "include sig ... end"
		  | getWhereSpecs (MarkSig(sigExp, _), wsl) =
		      getWhereSpecs(sigExp, wsl)
		val (sym, wsl) = getWhereSpecs(sigExp, [])
		in
		  doSpec ind;
		  doInd(ind+1);
		  app outf ["<INCLUDE><ID>", Sym.name sym, "</ID>\n"];
		  outWheres (outf, ind, wsl)
                end
            | outSpec ind (ShareStrSpec pl) = doSharing (ind,false,pl)
            | outSpec ind (ShareTycSpec pl) = doSharing (ind,true,pl)
            | outSpec ind (FctSpec _) =  Error.error "Functor in signature\n"
            | outSpec ind (MarkSpec (spec,_)) = outSpec ind spec

          in
            doInd 1;
            case sigid of
              SOME signame => let
                val sname = Sym.name signame
                val fname = case file of
                              NONE => sname
                            | SOME s => Sym.name s
                in
                  F.formatf "<SIGBODY SIGID=\"%s\" FILE=%s>\n" outf
                    [F.STR sname, F.STR(translateFileEntity fname)]
                end
            | NONE =>
                case file of
                  SOME s =>
                     F.formatf "<SIGBODY FILE=%s>\n" outf
                       [F.STR(translateFileEntity(Sym.name s))]
                | NONE =>  outf "<SIGBODY>\n"
            ; (* end case *)
            app (outSpec 2) specs;
            doInd 1;
            outf "</SIGBODY>\n"
          end

    fun outSigHd outf name =
          app outf ["<SIGNATURE SIGID=\"",Sym.name name,"\">\n"]

    fun outSiginstance outf (str,isOpaque) = (
(** eventually, we should check for ID families **)
	    doInd outf 1;
            if isOpaque then outf "<SIGINSTANCE OPAQUE>"
	    else outf "<SIGINSTANCE>";
	    outf " <ID> ";
	    outf str;
            outf "\n"
	  )

    fun outSigAndSigis (s as {outf, find},sigName,specs,sigis) = (
          outSigHd outf sigName;
          outSigBody s (SOME sigName, NONE, specs);
          app (outSiginstance outf) sigis;
          outf "</SIGNATURE>\n"
        )

    fun outSigAndStr (s as {outf, find},
		      Ast.Sigb{name as sigName, def}, strs) = (
	  case (getSigType def)
	   of SIGNAME (sigName,_) =>
		Error.error "Rebinding of signature name not implemented\n"
	    | (SIGBODY specs) => let
                fun usesSig constraint =
                      case (getSigType constraint) of 
                        SIGNAME (sigName',_) => sigName = sigName'
                      | _ => false
		fun split ([], l1, l2) = (rev l1, rev l2)
		  | split ((str as Ast.MarkStrb(strb,_))::r,l1,l2) = 
                      split(strb::r,l1,l2)
		  | split ((str as Ast.Strb{name, constraint,...})::r,l1,l2) = 
                      case constraint of
                        Ast.NoSig => split(r, l1, str::l2)
                      | Ast.Transparent cst => 
                          if usesSig cst 
                            then split (r, (Sym.name name,false)::l1, l2)
                            else split(r, l1, str::l2)
                      | Ast.Opaque cst => 
                          if usesSig cst 
                            then split (r, (Sym.name name,true)::l1, l2)
                            else split(r, l1, str::l2)
		val (strs1, strs2) = split (strs, [], [])
		in
                  outSigAndSigis (s,sigName,specs,strs1);
		  strs2
		end
	  (* end case *))
      | outSigAndStr _ = Error.error "internal error: outSigAndStr\n"

    fun outSig s sg = (outSigAndStr (s,sg, []); ())
    fun outStrHd outf name =
          app outf ["<STRUCTURE STRID=\"",Sym.name name,"\">"]

    fun outStr (s as {outf,find}) (Ast.Strb{name,constraint,...}) = let
          fun outS (isOpaque, constraint) =
                case getSigType constraint of
                  SIGNAME (sigName,wheres) => (
                    outStrHd outf name;
                    if isOpaque then outf " <OPAQUE> " else ();
                    outf (Sym.name sigName);
                    outWheres (outf,1,wheres);
                    outf "</STRUCTURE>\n"
                  )
                | SIGBODY specs => (
                    outStrHd outf name;
                    outf "\n";
                    if isOpaque then (doInd outf 1; outf "<OPAQUE>")
                    else ();
                    outSigBody s (NONE,SOME name,specs);
                    outf "</STRUCTURE>\n"
                  )
          in
            case constraint of
              Ast.NoSig => Error.error "Structure with no signature constraint\n"
            | Ast.Transparent constraint => outS (false, constraint)
            | Ast.Opaque constraint => outS (true, constraint)
          end
      | outStr s (Ast.MarkStrb(strb,_)) = outStr s strb

    fun outFctHd outf name =
          app outf ["<FUNCTOR FCTID=\"",Sym.name name,"\">"]

    fun mkArgs (pname, psig) = 
          [(SOME(Sym.strSymbol pname),Ast.VarSig(Sym.sigSymbol psig))]

    fun outFctArgs _ [] = 
          Error.error "Empty functor parameters not supported\n"
      | outFctArgs (s as {outf,find}) [parm] = (
          (case parm of
            (NONE,se) =>
              (case getSigType se of
                SIGNAME (sigName,_) => Error.error "functor with no formal parameters\n"
              | SIGBODY specs => (outf "\n";
				  outSigBody s (NONE,NONE,specs))
              )
          | (SOME id, se) =>
              (case getSigType se of
                SIGNAME (sigName,wheres) => (
                    app outf ["<ID>",Sym.name id,"</ID><ID>",Sym.name sigName,"</ID>\n"];
                    outWheres (outf, 2, wheres)
                  )
              | SIGBODY specs => (
                  outSigBody s (NONE,NONE,[Ast.StrSpec[(id,se,NONE)]])
                )
              )
          )
        )
      | outFctArgs _ _ = Error.error "Higher order functors not supported\n"

    fun outFct (s as {outf,find}) (Ast.Fctb{name,def}) = let
          val (args,constraint) = functArgs def
          fun outFct (se, isOpaque) =
                    case getSigType se of
                      SIGNAME (sigName,wheres) => (
                          outFctHd outf name;
                          outFctArgs s args;
                          outf inds;
                          if isOpaque then outf "<OPAQUE> " else ();
                          outf "<ID>";
                          outf (Sym.name sigName);
                          outWheres (outf, 2, wheres);
                          outf "\n</FUNCTOR>\n"
                        )
                    | SIGBODY specs => (
                          outFctHd outf name;
                          outFctArgs s args;
                          if isOpaque then (outf inds; outf "<OPAQUE>") else ();
                          outSigBody s (NONE,SOME name,specs);
                          outf "</FUNCTOR>\n"
                        )
          in
            case constraint of
              Ast.Transparent se => outFct (se,false)
            | Ast.Opaque se => outFct (se,true)
            | Ast.NoSig => Error.error "Functor with no signature constraint\n"
          end
      | outFct _ _ =
	  Error.error "internal error: Non-functor passed to outFct\n"

      (* outModule
       * If there is a single signature, and command line structure/functor
       * specifications, output the structure/functor with the signature.
       * If, rather, signature instances have been specified, output these
       * after the signature.
       * If there is a single structure/functor with a corresponding
       * signature declaration,
       * create a <STRUCTURE>/<FUNCT> element with the signature as the body.
       * Otherwise, output each signature, structure and functor.
       * In the case of a single signature and multiple structures,
       * output the signature followed by any matching structures as
       * siginstances. Then output the remaining structures.
       *)
    fun outAll (s, DECLS{sigs=[sg], strs, functs=[]}) = let
	  val strs' = outSigAndStr (s,sg, strs)
	  in
	    app (outStr s) strs'
	  end
      | outAll (s,DECLS{sigs,strs,functs}) = (
          app (outSig s) sigs; 
          app (outStr s) strs;
          app (outFct s) functs
        )

    fun outStrDecl (s as {outf,find},name,isOpaque,sigName,specs) = (
          outStrHd outf name;
          outf "\n";
          if isOpaque then (outf inds; outf "<OPAQUE>")
          else ();
          outSigBody s (SOME sigName, NONE, specs);
          outf "</STRUCTURE>\n"
        )

    fun outFtrDecl (s as {outf,find},name,args,isOpaque,sigName,specs) = (
          outFctHd outf name;
          outFctArgs s args;
          if isOpaque then (doInd outf 1; outf "<OPAQUE>")
          else ();
          outSigBody s (SOME sigName, NONE, specs);
          outf "</FUNCTOR>\n"
        )

    fun outModule (arg as (s as {outf,find}, 
                           DECLS{sigs=[Ast.Sigb {name=sigName,def}],
                                 strs=[Ast.Strb {name,constraint,...}],
                                 functs=[]})) = let
            fun outStr (isOpaque, constraint) =
                  case getSigType constraint of
                    SIGNAME (sigName',_) =>
                      if Sym.eq(sigName,sigName')
                        then case getSigType def of
                          SIGBODY specs => 
                            outStrDecl (s,name,isOpaque,sigName,specs)
                        | _ => outAll arg
                        else outAll arg
                  | _ => outAll arg
            in
              case constraint of
                Ast.NoSig => outAll arg
              | Ast.Transparent cst => outStr (false, cst)
              | Ast.Opaque cst => outStr (true, cst)
            end
      | outModule (arg as (s as {outf,find}, 
                           DECLS{sigs=[Ast.Sigb {name=sigName,def=sigDef}],
                                 functs=[Ast.Fctb {name,def}],
                                 strs=[]})) = let
          (* val Ast.Fctb {name,def} = fct *)
          val (args,constraint) = functArgs def
          (* val Ast.Sigb {name=sigName,def} = sg *)
          fun outFct (isOpaque,constraint) = 
                case getSigType constraint of
                  SIGNAME (sigName',_) =>
                    if Sym.eq(sigName,sigName')
                      then case getSigType sigDef of
                        SIGBODY specs =>
                          outFtrDecl (s,name,args,isOpaque,sigName,specs)
                      | _ => outAll arg
                      else outAll arg
                | _ => outAll arg
          in
            case constraint of
              Ast.NoSig => outAll arg
            | Ast.Transparent cst => outFct (false, cst)
            | Ast.Opaque cst => outFct (true, cst)
          end
      | outModule arg = outAll arg

    fun outModules (s as {outf,find}, specs,
                           decls as DECLS{sigs=sigs as [Ast.Sigb {name=sigName,def}],
                                 strs=[],
                                 functs=[]}) = let
            in
              case specs of
                None => outSig s (hd sigs)
              | Str (name, isOpaque) => (
                  case getSigType def of
                    SIGBODY specs => outStrDecl (s,Sym.strSymbol name,isOpaque,sigName,specs)
                  | _ => Error.error "No signatures defined\n"
                )
              | Ftr (name, pname, psig, isOpaque) => let
                  val args = mkArgs (pname, psig)
                  in
                    case getSigType def of
                      SIGBODY specs => 
                        outFtrDecl (s,Sym.fctSymbol name,args,isOpaque,sigName,specs)
                    | _ => Error.error "No signatures defined\n"
                  end
              | SigI sigis => 
                  case getSigType def of
                    SIGBODY specs => outSigAndSigis (s,sigName,specs,rev sigis)
                  | _ => Error.error "No signatures defined\n"
            end
      | outModules (s, specs, decls) = (
          case specs of
            None => ()
          | _  => Error.warning ("Did not find a single signature: command line specifications ignored", []);
          outModule (s, decls)
        )

    fun outSGML {specs, findComments, infile, fname, version, copyright, outs, dec} = let
	  val decls = normalize dec
	  fun outf s = TextIO.output (outs, s)
	  in
            doPrologue (outf, version, copyright, infile, fname, moduleName decls);
            outModules ({outf=outf,find=findComments}, specs, decls);
            doEpilogue outf
	  end

  end
