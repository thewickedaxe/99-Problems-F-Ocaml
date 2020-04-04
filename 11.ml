(* 11. Modified run-length encoding. (easy)
Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

# type 'a rle =
    | One of 'a
    | Many of int * 'a;;
type 'a rle = One of 'a | Many of int * 'a


# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

 type 'a rle =
  | One of 'a
  | Many of int * 'a;;
type 'a rle = One of 'a | Many of int * 'a
;;

let encode inList = 
  let rec auxEncode curPack packedList = function
  | [] -> []
  | [last] -> (List.length (last::curPack), last)::packedList
  | hd::(next::_ as tl) -> 
                          if (hd = next) then
                            auxEncode (hd::curPack) packedList tl
                          else
                            auxEncode [] ((List.length (hd::curPack), hd)::packedList) tl
  in let rec runLengthEncode encodedList = function
  | [] -> encodedList
  | hd::tl -> match hd with
              | (1, x) -> runLengthEncode (One x::encodedList) tl
              | (num, x) -> runLengthEncode (Many (num, x)::encodedList) tl
  in runLengthEncode [] (auxEncode [] [] inList)
;;

(* Tests *)
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode [];;