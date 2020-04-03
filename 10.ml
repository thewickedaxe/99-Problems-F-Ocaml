(* 10. Run-length encoding of a list. (easy)

Here is an example:

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

(* Going forward I will be doing tail reursive or non tail recursive whichever is easier *)
let encode inList = 
  let rec auxEncode curPack packedList = function
  | [] -> []
  | [last] -> (List.length (last::curPack), last)::packedList
  | hd::(next::_ as tl) -> 
                          if (hd = next) then
                            auxEncode (hd::curPack) packedList tl
                          else
                            auxEncode [] ((List.length (hd::curPack), hd)::packedList) tl
  in List.rev (auxEncode [] [] inList)
;;

(* Tests *)
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode [];;