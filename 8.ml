(* 8. Eliminate consecutive duplicates of list elements. (medium)
Solution *)

let compress inList = 
  let rec reverse reversedList = function
  | [] -> reversedList
  | hd::tl -> reverse (hd::reversedList) tl
  in let rec auxCompress compressedList = function
  | [] -> compressedList
  | [last] -> (last::compressedList)
  | x::(y::tl) -> if (x=y) then
                    auxCompress compressedList (y::tl)
                  else
                    auxCompress (x::compressedList) (y::tl)
  in reverse [] (auxCompress [] inList)
;;

(* Tests *)
compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
compress [];;
compress ["a"; "b"; "c"];;