(* valid.sml
 *
 * COPYRIGHT (c) 1997 AT&T Laboratories.
 *
 * Validate a signature and a collection of related structures.
 *
 *)
structure Validate :
  sig
    val doRec : (string AtomTable.hash_table * string) 
                  -> Common.sig_record -> unit
  end = 
  struct
    structure C = Common
    structure IO = TextIO
    structure F = Format
    structure SS = Substring
    structure RE = Regexp

    fun genCode (f,name,isOpaque,signame,wheres) = let
          fun out s = IO.output(f,s)
          fun outw w = (out "  where type "; out w; out "\n")
          in
            out "structure S ";
            if isOpaque then out ":> " else out ": ";
            out signame; out "\n";
            app outw wheres;
            out " = "; out name; out "\n"
          end

      (* Transform name template abc{N}def into matching function
       * recognizing abc[0-9]+def. The function returns true if it
       * finds a match.
       *)
    fun transform n = let
          val num = "[0-9]+"
          val gen = RE.match o RE.compile
          val (pref,rest) = SS.splitl (fn c => c <> #"{") (SS.full n)
          val (exp,suff) = SS.splitr (fn c => c <> #"}") rest
          val matchf = if SS.size exp = 3 andalso SS.sub(exp,1) <> #"N"
                         then gen (concat [SS.string pref,num,SS.string suff])
                         else gen n
          in
            fn s => matchf s <> []
          end

    fun doRec (env,docfile) (C.SGR{name,file="",strs}) = ()
      | doRec (env,docfile) (C.SGR{name=signame,file,strs}) = let
            (* Converts IDs to matching structure names.
             * Punts on long names.
             *) 
          fun lookupStr (C.ID n) =
                if Char.contains n #"."  then [n]
                else if AtomTable.find env (Atom.atom n) = NONE then [] else [n]
            | lookupStr (C.IDFAMILY n) = let
                fun look match (n,l) = if match n then n::l else l
                in
                  AtomTable.fold (look (transform n)) [] env
                end
          val tmpf = OS.FileSys.tmpName ()
          fun chkStr (C.STR{name,isReq,isOpaque,wheres}) = let
                fun chk n = let
                      val outf = IO.openOut tmpf
                      in
                        genCode (outf,n,isOpaque,signame,wheres);
                        IO.closeOut outf;
                        NJCompiler.useF (tmpf,n);
                        OS.FileSys.remove tmpf
                      end
                in
                  case lookupStr name of
                    [] => if not isReq then ()
                            else C.eprf "Required structure %s not found\n"
                              [F.STR (C.nameOf name)]
                  | strs => app chk strs
                end
            (* Generate signature from mldoc *)
          fun genSigFile () = OS.Process.system (C.codeCmd^docfile)
          in
            if genSigFile () = OS.Process.success then (
              if NJCompiler.useF (file,signame) then app chkStr strs else ();
              OS.FileSys.remove file)
            else ()
          end

  end (* Validate *)
