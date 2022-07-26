(* parse.sml
 *
 * COPYRIGHT (c) 2022 John Reppy (https://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ParseML : sig

    val parse : string * TextIO.instream -> Ast.dec

  end = struct

    val outs = TextIO.stdErr
    fun ppflush () = TextIO.flushOut outs
    fun ppout s = TextIO.output (outs, s)

    fun parse (fname, ins) = let
          val ppcons = {consumer = ppout, linewidth=fn () => 0, flush = ppflush}
          val src = Source.newSource(fname, ins, false, ppcons)
          in
            SmlFile.parse src
          end

  end
