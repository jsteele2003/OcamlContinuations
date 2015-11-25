(* ------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                  *)
(* COMP 302 Programming Languages - FALL 2015                                *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* ------------------------------------------------------------------------- *)
(*
  STUDENT NAME(S):Joe Steele, Stanley Su
  STUDENT ID(S)  :260516386, 260632313
Both my partner and I discussed all problems in person. I (Joe Steele) coded and commented the solutions to part 1, and attempted problems 2.2 and 2.3.
Stanley wrote 2.1,

 

  Fill out the template below.

*)
module type STREAM = 
  sig
    type 'a susp = Susp of (unit -> 'a)
    type 'a str = {hd: 'a  ; tl : ('a str) susp} 

    val force: 'a susp -> 'a
    val map  : ('a -> 'b) -> 'a str -> 'b str 
    val take : int -> 'a str -> 'a list
  end 


module Stream : STREAM = 
  struct
    (* Suspended computation *)
    type 'a susp = Susp of (unit -> 'a)

    (* force: *)
    let force (Susp f) = f ()

    type 'a str = {hd: 'a  ; tl : ('a str) susp} 

    (* map: ('a -> 'b) -> 'a str -> 'b str *)
    let rec map f s = 
      { hd = f (s.hd) ; 
	tl = Susp (fun () -> map f (force s.tl))
}

    (* Inspect a stream up to n elements *)
    let rec take n s = match n with 
      | 0 -> []
      | n -> s.hd :: take (n-1) (force s.tl)
	  
  end 



module type BIN = 
 sig
   type bit = Zero | One | End
   type bin = int list

   val bin_str : bin Stream.str 
  val send_str : bin Stream.str -> bit Stream.str
   val rcv_str : bit Stream.str -> bin Stream.str
  (*  val to_int  : bin Stream.str -> int Stream.str *)
       
 end 

module Bin : BIN = 
  
  struct
  include Stream
  type bit = Zero | One | End
  type bin = int list
  let rec toBinary (x:int) (acc: int list) = if (x/2 != 0) then toBinary (x/2) ((x mod 2)::acc) else [1]@acc
  let rec numsFrom n = {hd = toBinary (n) [] ; tl = Susp (fun () -> numsFrom (n+1))}
  let nats = numsFrom 1
(*   let rec bin_str = {hd = [1]; tl = Susp(fun() -> bin_str)} *)
  let rec bin_str = nats

 let rec send_str s = match s.hd with

                  |[] -> {hd = End; tl = Susp(fun() -> send_str(force s.tl))}
                  | 1::t -> {hd = One; tl = Susp(fun () -> send_str {hd = t; tl = s.tl})}
                  | 0::t -> {hd = Zero; tl = Susp(fun () -> send_str {hd = t; tl = s.tl})}

(* bitTake: builds an int list from a bit stream (causes stack overflow) *)
  let rec bitTake (acc: int list) (s: bit Stream.str)  = match s.hd with
                            | Zero -> bitTake (acc@[0])  (force s.tl)
                            | One  -> bitTake (acc@[1]) (force s.tl)
                            | End -> acc

  let rec rcv_str (b: bit Stream.str) = {hd = bitTake [] b; tl = Susp(fun () -> rcv_str(force b.tl))}
end 


(* Implement a module Bin that matches the signature BIN and provides 
   all the necessary functionality *)
