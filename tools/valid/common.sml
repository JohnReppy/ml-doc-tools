(* common.sml
 *
 * COPYRIGHT (c) 1997 AT&T Laboratories.
 *
 * Common types and functions for validate program.
 *
 *)
structure Common =
  struct
      (* Path name to code extraction tool *)
    val codeCmd = "/usr/local/sml/Tools/ML-Doc/bin/code-extract "

    fun pr s = TextIO.output(TextIO.stdOut, s)
    fun epr s = TextIO.output(TextIO.stdErr, s)
    fun prf fmt args =  TextIO.output(TextIO.stdOut, Format.format fmt args)
    fun eprf fmt args = epr (Format.format fmt args)

    fun abort e = (epr "Abort: "; 
                   epr (exnMessage e);
                   epr "\n"; 
                   OS.Process.failure)

    datatype str_name = ID of string | IDFAMILY of string

    fun nameOf (ID s) = s
      | nameOf (IDFAMILY s) = s

    datatype str_record = 
               STR of {name : str_name, 
                       isOpaque: bool, 
                       isReq : bool, 
                       wheres : string list }

    datatype sig_record = 
               SGR of {name : string, 
                       file : string, 
                       strs : str_record list }

      (* Blend two records with the same sig name
       * and compatible file names.
       *)
    fun blend (SGR{name,file,strs},SGR{name=name',file=file',strs=strs'}) = let
          fun blendFile ("",n') = SOME n'
            | blendFile (n,"") = SOME n
            | blendFile (n,n') = if n = n' then SOME n else NONE
          in
            if name <> name' then NONE
            else case blendFile (file,file') of
              NONE => NONE
            | SOME file => SOME (SGR {name=name,file=file,strs=strs'@strs})
          end

  end
