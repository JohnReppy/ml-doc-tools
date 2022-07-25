(* parse-with-comments.sml
 *
 * Contributed by Dan Wang (danwang@cs.princeton.edu)
 * Revised by E. Gansner (erg@research.att.com)
 *)

structure ParseWithComments :> sig

    datatype mode =  Before  | After  | Disable

    val parse : mode -> (string * TextIO.instream) -> (Ast.dec * ((int * int) -> string list))

  end = struct
        
    fun next s = raise (Fail "")
    datatype state
      = Q
      | QP
      | QPS
      | QPSS
      | QPSSP
    type cmap = (string * (int * int)) list
    type state = {buff:char list,beg:int,comms:cmap}

    fun make_comm s = let
          val isNL = (fn #"\n" => true  | _ => false)
          val isJunk = (fn #"*" => true  | x => Char.isSpace x)
          val ss = Substring.full(String.implode (List.rev s))
          val lins = Substring.tokens isNL ss
          val lins = List.map (Substring.dropl isJunk) lins
          in
           (Substring.concat lins)
          end
        
    fun add_char #"\n" {buff,beg,comms} =
        {buff=(#"\n")::(#" "::buff),beg=beg,comms=comms}
      | add_char c {buff,beg,comms} =
        {buff=c::buff,beg=beg,comms=comms}
    fun end_comm e {buff,beg,comms} = 
        {buff=[],beg=0,
         comms=(make_comm buff,(beg,e))::comms}

    fun beg_comm b {buff,beg,comms} = 
        {buff=[],beg=b,comms=comms}
        
    val init = {buff=[],beg=0,comms=[]}
    fun delta (#"(",i,(Q,b)) = (QP,beg_comm i b)
      | delta (_,i,(Q,b)) = (Q,b)

      | delta (#"*",i,(QP,b)) = (QPS,b)
      | delta (_,i,(QP,b)) = (Q,b)

      | delta (#"*",i,(QPS,b)) = (QPSS,b)
      | delta (c,i,(QPS,b)) = (QPS,add_char c b)

      | delta (#")",i,(QPSS,b)) = (QPSSP,end_comm i b)
      | delta (c,i,(QPSS,b)) = (QPS,add_char c (add_char #"*" b))

      | delta (#"(",i,(QPSSP,b)) = (QP,b)
      | delta (_,i,(QPSSP,b)) = (Q,b)

    structure S = TextIO.StreamIO
    fun read s = let
         fun loop (NONE,pos,state) = state
           | loop (SOME (c,rest),pos,state) =
                loop(S.input1 rest,pos+1,delta(c,pos,state))
          val ins = (TextIO.getInstream s)
         val (_,{comms,buff,beg}) = loop(S.input1 ins,0,(Q,init))
         in
           List.rev comms
         end
    datatype mode =  Before  | After  | Disable
    (* nasty O(n) algorithm *)
    fun after ((s1,e1),(s2,e2)) = s1 > e2 orelse s1 = e2
    fun bef ((s1,e1),(s2,e2)) = e1 < s2 orelse e1 = s2
    fun between (x,y,z) =
        after(y,x) andalso bef(y,z)

(*
    fun find ([]) _ _ = []
      | find _ Disable  _ = []
      | find ([(c,x)]) After y =
        if after (x,y) then [c]
        else []
      | find ([(c,x)]) Before y =
        if bef (y,x) then [c]
        else []
      | find ((b,x)::(rest as (a,z)::_)) m y =
            if between (x,y,z) then
                (case m of
                    Before => [b]
                  | After =>  [a]
                  | _ => [])
            else find rest m y
*)

    fun findBefore [] _ = []
      | findBefore ([(c,x)]) y =
        if bef (x,y) then [c] else []
      | findBefore ((b,x)::(rest as (a,z)::_)) y =
        if between (x,y,z) then [b]
        else findBefore rest y

    fun findAfter [] _ = []
      | findAfter ([(c,x)]) y =
        if after (x,y) then [c] else []
      | findAfter ((b,x)::(rest as (a,z)::_)) y =
        if between (x,y,z) then [a]
        else findAfter rest y

    fun parse mode (n,ins) = let
          val cfn = case mode of
                      Disable => (fn _ => [])
                    | Before => findBefore (read ins)
                    | After => findAfter (read ins)
          val ast = ParseML.parse (n,ins)
          in
           (ast,cfn)
          end
end
