(* 9. Pack consecutive duplicates of list elements into sublists. (medium)
Solution *)

(* Tail Recursive *)

let pack inList = 
  let rec auxPack currentPack packedList = function
  | [] -> []
  | [last] -> (last::currentPack)::packedList
  | hd::(next::_ as tl) -> if (hd = next) then
                            auxPack (hd::currentPack) packedList tl
                           else
                            auxPack [] ((hd::currentPack)::packedList) tl
  in List.rev (auxPack [] [] inList)
;;



(* Non tail Recursive *)
let nt_pack inList =
  let rec auxPack currentPack = function
  | [] -> []
  | [last] -> [(last::currentPack)]
  | hd::(nxt::tl as rest) -> if (hd = nxt) then
                              (auxPack (hd::currentPack) rest)
                             else
                              (hd::currentPack)::(auxPack [] rest)
  in auxPack [] inList
;;

(* Tests *)
(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]) = (nt_pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]);;
