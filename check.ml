(* Joe Steele, 260516386 *)
module type CHECK = 
sig

  exception Error

  type 'a input = 'a list
  type 'a args = 'a list
  type 'a condition = 'a args -> bool

  val find     : 'a input list -> 'a condition list -> 'a args  
  val find_all : 'a input list -> 'a condition list -> 'a args list


end 



module Check : CHECK = 
struct

  exception Error 
  type 'a input = 'a list
  type 'a args = 'a list
  type 'a condition = 'a args -> bool


  (* Check whether all conditions in cond are satisfied *)

(* acc: an 'a list that accumaltes tuples of inputs to be tested on each condition *)
   
  let rec find_tr (l: ('a input) list) (cond : ('a condition) list) (acc : 'a args) (fc : unit -> 'a args) =
    match l with (*This first pattern match will break the matrix down, row by row*)
                 (*first, we fill the accumulator with n args and test it. Then, iterate with the next possible combination of n inputs*)
    | [] -> fc ()
    | [hl] ->  (match hl with (*Here, hl is the last row of the matrix, and therefore we are adding the last element to acc*)
                  |[ha] -> (match cond with (*last element of the matrix, so we know we can start testing acc with no other work to do*)
                      |[c1] -> if (c1 (acc@[ha])) then (acc@[ha]) (*Here, we are on the last condition, so if it checks then we have our answer*)
                                else fc () (*in this case, our branch has failed, and we use the fc to backtrack*)
                      |c1::ct -> if (c1 (acc@[ha])) then find_tr l ct acc fc
                                else fc () 
                      |[] -> fc ())
                  |ha::ta -> find_tr [[ha]] cond acc (fun () -> find_tr [ta] cond acc fc) (*Here, our two situations are to either take one element from the row as our last 
                                                                                            acc input, or ignore it and look to the rest of the row for our last input *)
                  |[] -> fc ())

    | hl::tl-> (match hl with (*hl represents one row of the matrix*)
                  |[ha] -> find_tr tl cond (acc@[ha]) fc
                  |ha::ta -> find_tr tl cond (acc@[ha]) (fun () -> find_tr (ta::tl) cond acc fc)(*Here, we have two situations: we either accept an element from our row 
                                                                                                  to the accumulator, and so we can ignore the rest of the row. When we do
                                                                                                  this though, we need to pass the work we're not doing to the continuation,
                                                                                                  and so we leave the acc to be filled by the rest of the current row *) 
                  |[] -> fc ())

  let find (l: ('a input) list) (cond : ('a condition) list) = find_tr l cond [] (fun () -> [])

  (* find all input combinations that satisfy a given list of conditions *)
  let rec find_all_tr (l: ('a input) list) (cond : ('a condition) list) (acc: 'a args) (sc: ('a args) list -> ('a args) list) (fc : unit -> ('a args) list) =
  match l with
  | [] -> fc ()
  | [hl] -> (match hl with  
              | [ha] -> (match cond with 
                        |[c1] -> if (c1 (acc@[ha])) then sc [acc@[ha]]
                                  else fc ()
                        | c1::ct -> if (c1 (acc@[ha])) then find_all_tr l ct acc sc fc  
                                  else fc ()
                        | [] -> fc ())
              |ha::ta -> find_all_tr [[ha]] cond acc (fun s' -> s'@(find_all_tr [ta] cond acc sc fc)) (fun () -> find_all_tr [ta] cond acc sc fc)
              |[] -> fc ())

  | hl::tl -> (match hl with
              | [ha] -> find_all_tr tl cond (acc@[ha]) sc fc 
              |ha::ta -> find_all_tr tl cond (acc@[ha]) (fun s' -> s'@(find_all_tr (ta::tl) cond acc sc fc)) (fun () -> find_all_tr (ta::tl) cond acc sc fc) (*explanation below*)
              | [] -> fc ())


(*Essentially, we are doing exactly the same thing as before, except when we 
move down a row, or select a final element that isn't actually the last item of the last row,
we are essentially ignoring potential successes that could be occurring in our failure continuation,
after a success in the main recursive call. 
This means to aggregate across all successful accumulators, we need to concatenate our successes to
the results from the work inside the fc. *)



  let find_all (l: ('a input) list) (cond : ('a condition) list) = 
    find_all_tr l cond [] (fun r -> r) (fun () -> [])

end
