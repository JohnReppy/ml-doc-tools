(* main.sml
 *
 * COPYRIGHT (c) 1997 AT&T Laboratories.
 *
 * A tool for validating module implementations against ML-Doc specifications.
 *
 *)
structure Main :
  sig
    val main : string * string list -> OS.Process.status

  end = struct

    structure IO = TextIO
    structure E = MLDocElem
    structure P = MLDocParser
    structure Pr = PrintMLDoc
    structure M = P.Markup
    structure Run = RunMLDocSGMLS
    structure C = Common
    structure V = Validate
    structure SS = Substring

    datatype module_kind = STR_K of (string * bool) | SIG_K of string

    val sdata = EntitiesToSML.transEntity

    val transData = M.transData {
        escape = String.str,
        sdata = sdata,
        special = fn _ => NONE
      }

    val stripr = SS.dropr (Char.contains " \t\n")
    val stripl = SS.dropl Char.isSpace
    fun fmtData s =
          SS.string(SS.dropl Char.isSpace (
                    SS.dropr Char.isSpace (SS.full (transData s))))

    fun skipHdr iter = let
      fun skip iter = case (M.next iter) of
            NONE => iter
          | SOME(M.ELEM{elem=E.SECTION _, ...}, _) => iter
          | SOME(_, iter') => skip iter'
      in
        skip iter
      end

    fun statusOf (SOME E.REQUIRED) = true
      | statusOf _ = false

    fun findModule iter =
          case M.next iter of
            NONE => NONE
          | SOME(M.ELEM{elem=E.SIGNATURE{sigid, ...}, body,...},iter') =>
              SOME(SIG_K sigid, body, M.right iter')
          | SOME(m as M.ELEM{elem=E.STRUCTURE{strid,status}, body,...},iter') =>
              (SOME(STR_K (strid, statusOf status), body, M.right iter'))
          | SOME(_,iter') => findModule iter'

    fun getData (M.DATA str::_) = fmtData str
      | getData [] = raise Fail "getData with empty list"
      | getData _ = raise Fail "getData with ELEM"
    fun getOptVars (M.ELEM{elem=E.TYPARAM,body,...}::rest) = (getData body,rest)
      | getOptVars body = ("", body)
    fun getID (M.ELEM{elem=E.ID,body,...}::rest) = (getData body,rest)
      | getID (m::_) = raise Fail ("getID: "^(Pr.toString m))
      | getID [] = raise Fail "getID: empty"
    fun getTy (M.ELEM{elem=E.TY,body,...}::rest) = (getData body,rest)
      | getTy _ = raise Fail "getTy"

    fun nameOf (M.ELEM{elem=E.ID,body,...}) = C.ID (getData body)
      | nameOf (M.ELEM{elem=E.IDFAMILY,body,...}) = C.IDFAMILY (getData body)
      | nameOf _ = raise Fail "nameOf : expected ID | IDFAMILY"

    fun genWhere body = let
          val (vars,body) = getOptVars body
          val (id, body) = getID body
          val (ty, body) = getTy body
          in
            case vars of
              "" => concat [id, " = ", ty]
            | vs => concat [vs, " ", id, " = ", ty]
            
          end
    fun wheresOf (M.ELEM{elem=E.WHERETYPE,body,...}::rest,l) =
          wheresOf (rest,(genWhere body)::l)
      | wheresOf (_,l) = rev l

    fun doFile env mldoc = let
          val iter = M.iterateList (Run.parseFile {
		  mldocHome = NONE, sgmlsPath = NONE, includeEntities = []
		} mldoc)
          fun genInstance(status,opaque,body) = let
                val name = nameOf (hd body)
                val wheres = wheresOf (tl body,[])
                in
                 C.STR {name = name,
                      isOpaque = opaque,
                      isReq = statusOf status,
                      wheres = wheres }
                end

          fun genSig (name,M.ELEM{elem=E.SIGBODY{file,...},...}::rest) = let
                fun instances (M.ELEM{elem=E.SIGINSTANCE{status,opaque},
                      body,...}::rest,l) = 
                        instances (rest, genInstance (status,opaque,body)::l)
                  | instances (_,l) = rev l
                val strs = instances (rest,[])
                in
                  C.SGR{name=name,file=fmtData(valOf file),strs=strs}
                end
            | genSig _ = raise Fail "Ill-formed Signature: no SIGBODY"

          fun genStr (name,sts,body) = let
                val (opaque, sd) = case body of
                                     M.ELEM{elem=E.OPAQUE,...}::rest =>
                                       (true,rest) 
                                   | _ => (false,body) 
                val (sigid, file, wheres) = case sd of
                      M.ELEM{elem=E.SIGBODY{sigid=SOME sigid,file},...}::_ =>
                        (sigid, getOpt (file,""), [])
                    | (M.ELEM{elem=E.ID,body,...})::rest =>
                        (#1(getID sd), "", wheresOf (rest,[]))
                    | _ => raise Fail "Ill-formed Structure"
                val str = C.STR {name = C.ID name,
                               isOpaque = opaque,
                               isReq = sts,
                               wheres = wheres }
                in
                  C.SGR{name=sigid,file=fmtData file,strs=[str]}
                end

          fun readRecs (iter, l) =
                case findModule iter of
                  NONE => rev l 
                | SOME (SIG_K name, body, iter) =>
                    readRecs (iter, (genSig (name, body))::l)
                | SOME (STR_K (name,sts), body, iter) =>
                    readRecs (iter, (genStr (name, sts, body))::l)

            (* Merge adjacent records with the same sig name,
             * and a unique file name
             *)
          fun mergeRecs [] = []
            | mergeRecs (r::recs) = let
                fun merge (r, [], l) = r::l
                  | merge (r, r'::recs, l) =
                      case C.blend (r,r') of
                        NONE => merge (r',recs,r::l)
                      | SOME r'' => merge (r'',recs,l)
                in
                  rev (merge (r,recs,[]))
                end
          in
            app (V.doRec (env,mldoc)) (mergeRecs (readRecs (skipHdr iter,[])))
          end

    val useStr = "Usage: valid [-?] [files]\n"
    fun usage ok = let
          val exval = if ok then OS.Process.success else OS.Process.failure
          in
            C.pr useStr;
            OS.Process.exit exval
          end

    fun doOpts args = let
          fun loop ([],l) = ([],rev l)
            | loop ("-?"::_,_) = usage true
            | loop (""::rest,l) = loop (rest, l)
            | loop (arg::rest,l) =
                if String.sub(arg,0) = #"-"
                  then (C.eprf "Unknown flag: %s\n" [Format.STR arg]; 
                        usage false)
                  else loop (rest,arg::l)
          in
            loop (args,[])
          end
    
    fun main (cmdName, cmdLine) = let
         val (opts, files) = doOpts cmdLine
         val env = NJCompiler.mkEnv ()
         in
           app (doFile env) files;
           OS.Process.success
           end handle e => C.abort e

  end (* Main *)
