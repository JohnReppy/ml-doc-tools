(* parse.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure ParseML : sig

    val parse : string * TextIO.instream -> Ast.dec

  end = struct

    val outs = TextIO.stdErr
    fun ppflush () = TextIO.flushOut outs
    fun ppout s = TextIO.output (outs, s)

    fun parse (fname, ins) = let
          val ppcons = {consumer = ppout, linewidth=fn () => 0, flush = ppflush}
          val src = Source.newSource(fname,1,ins,false,ppcons)
          in
            SmlFile.parse src
          end

  end
