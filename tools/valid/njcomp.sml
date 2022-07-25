(* njcomp.sml
 *
 * COPYRIGHT (c) 1997 AT&T Laboratories.
 *
 * SML/NJ compiler specific routines
 *
 *)
structure NJCompiler :
  sig
    val mkEnv : unit -> string AtomTable.hash_table
    val useF : string * string -> bool
  end =
  struct
    structure C = Common
    structure Env = Compiler.Environment
    structure Sym = Compiler.Symbol

      (* Create an environment (hash table) containing the 
       * structures in the pervasive environment.
       *)
    fun mkEnv () = let
          exception Hash
          val env = AtomTable.mkTable (100, Hash)
          val statenv = 
            Env.catalogEnv (Env.staticPart ((#get Compiler.EnvRef.pervasive)()))
          fun ins sym =
                if Sym.nameSpace sym <> Sym.STRspace then ()
                else let
                  val s = Sym.name sym
                  in
                    AtomTable.insert env (Atom.atom s, s)
                  end
          in
            app ins statenv;
            env
          end

      (* Internal version of use function.
       * Turns off all output unless an error occurs.
       *)
    fun useF (f,name) = let
          fun null _ = ()
          val outs : string list ref = ref []
          fun say s = outs := s::(!outs)
          val saveout = !Compiler.Control.Print.out
          val _ =  Compiler.Control.Print.out := {say = say, flush = null};
          val ins = TextIO.openIn f
          val error = (Compiler.Interact.useStream ins; 
                       TextIO.closeIn ins; 
                       false) handle e => (TextIO.closeIn ins; true)
          in
            Compiler.Control.Print.out := saveout;
            if error then (app C.epr ["Error in ",name,":\n"];
                           app C.epr (rev (!outs)); 
                           false)
            else true
          end

  end
