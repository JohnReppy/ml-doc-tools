(* main.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure SS = Substring
    structure O = Output
    structure PWC = ParseWithComments

    val version = "1.0"

    val defaultCopyright = "The Fellowship of SML/NJ"

    fun errOut s = TextIO.output (TextIO.stdErr, s)

    val cmd = "mkdoc"

    fun usage cmd = (
          errOut "Usage: ";
          errOut cmd;
          errOut " [-ba?] [-o outfile] [-c copyright] ";
          errOut "[(+|-)s str] [(+|-)f fct pid psig] [(+|-)i sig] [file]\n";
	  errOut "  Options:\n\
		 \    -?               -- print this message\n\
		 \    -a,-b            -- copy comments from source into ML-Doc file;\n\
		 \                        -a (after)  binds comments to the proceeding specification\n\
		 \                        -b (before) binds comments to the following specification\n\
		 \    -o outfile       -- specify the output file; otherwise stdout is used\n\
		 \    -c copyright     -- insert the given copyright string into the output\n\
		 \    -s str           -- add the structure str to the generated specs\n\
		 \    +s str           -- like -s option, but with opaque signature matching\n\
		 \    -f fct pid psig  -- add the functor fct to the generated specs\n\
		 \    +f fct pid psig  -- like -f option, but with opaque signature matching\n\
		 \    -i sig           -- add the signature sig to the generated specs\n\
		 \    +i sig           -- like -i option, but with opaque signature matching\n\
		 \"
        )

    fun parseOptions (_, args) = let
          val inf = ref NONE
          val outf = ref NONE
          val mode = ref PWC.Disable
          val specs = ref Output.None
          val copyright = ref defaultCopyright
          fun optError msg c = (app errOut [msg,c,"\n"];
                                usage cmd;
                                raise Fail "bad option")
          val unknown = optError "Unknown option: "
          val missing = optError "Missing parameter(s) for flag "

          fun getOpt (opt, rest) =
                if SS.size opt = 1 then  (* only flag is present *)
                  case rest of
                    ""::rest => NONE
                  | opt::rest => SOME(opt,rest)
                  | _ => NONE
                else SOME(SS.string(SS.triml 1 opt), rest)
          fun doArgs [] = ()
            | doArgs (arg::rest) = let
                val ss = SS.full arg
                in
                  case SS.first ss of
                    NONE => doArgs rest
                  | SOME #"-" => doOpt (SS.triml 1 ss, rest, false)
                  | SOME #"+" => doOpt (SS.triml 1 ss, rest, true)
                  | SOME _ => (inf := SOME arg; doArgs rest)
                end
          and doOpt (opt, rest, opaque) =
                case SS.first opt of
                  NONE => unknown (#1(SS.base opt))
                | SOME #"a" => (mode := PWC.After; doArgs rest)
                | SOME #"b" => (mode := PWC.Before; doArgs rest)
                | SOME #"c" =>
                    (case getOpt (opt, rest) of
                      NONE => missing "c"
                    | SOME (opt,rest) => (copyright := opt; doArgs rest))
                | SOME #"f" =>
                    (case getOpt (opt, rest) of
                      NONE => missing "f"
                    | SOME (opt,rest) => ( 
                       case rest of
                         pid::psig::t => (specs := O.Ftr(opt,pid,psig,opaque);
                                          doArgs t)
                       | _ => missing "f")
                    )
                | SOME #"i" =>
                    (case getOpt (opt, rest) of
                      NONE => missing "i"
                    | SOME (opt,rest) => (
                       case !specs of
                         O.SigI l => specs := O.SigI((opt,opaque)::l)
                       | _ => specs := O.SigI[(opt,opaque)];
                       doArgs rest)
                    )
                | SOME #"o" =>
                    (case getOpt (opt, rest) of
                      NONE => missing "o"
                    | SOME (opt,rest) => (outf := SOME opt; doArgs rest))
                | SOME #"s" =>
                    (case getOpt (opt, rest) of
                      NONE => missing "s"
                    | SOME (opt,rest) => (specs := O.Str(opt,opaque); doArgs rest))
                | SOME #"?" => (usage cmd; OS.Process.exit OS.Process.success)
                | SOME #"h" => (usage cmd; OS.Process.exit OS.Process.success)
                | SOME c => unknown (#1(SS.base opt))
          in
            doArgs args;
            { cmdname = cmd,
              infile = !inf,
              outfile = !outf,
              copyright = !copyright,
              specs = !specs,
              mode = !mode
            }
          end

    fun main args = let
          val options = parseOptions args
          val (inname, ins, closeIn) = (case (#infile options) of
                    NONE => ("<stdIn>", TextIO.stdIn, fn _ => () )
                  | SOME fname => (fname,TextIO.openIn fname, TextIO.closeIn) 
                          handle (IO.Io {cause,...}) => 
                            Error.error'[cmd, ": could not open \"", fname,
                                        "\" for input - ", exnMessage cause]
                  (* end case *))
          val (ast,cmap) = PWC.parse (#mode options) (inname, ins)
          val (outname, outs, closeOut) = (case #outfile options of
                    NONE => ("",TextIO.stdOut, fn _ => () )
                  | SOME fname => (fname,TextIO.openOut fname, TextIO.closeOut)
                          handle (IO.Io{cause,...}) =>
                            Error.error'[cmd, ": could not open \"", fname,
                                        "\" for output - ", exnMessage cause]
                  (* end case *))
          in
            closeIn ins;
            Output.outSGML {
              findComments = cmap,
              infile = #infile options,
              fname = outname,
              version = version,
              copyright = #copyright options,
              specs = #specs options,
              outs = outs,
              dec = ast
            };
            closeOut outs;
            OS.Process.success
          end handle ex => (Error.uncaughtExn ex; OS.Process.failure)
  end;
