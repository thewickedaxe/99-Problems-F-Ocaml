(* 4. Find the number of elements of a list. (easy)
OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

Solution

# hd_rec_length [ "a" ; "b" ; "c"];;
- : int = 3
# hd_rec_length [];;
- : int = 0 *)

(* Tail Recursive *)
let length in_list =
  let rec find_len cur_length = function
  | [] -> cur_length
  | hd::tl -> find_len (cur_length + 1) tl
  in find_len 0 in_list
;;

(* Regular *)
let hd_rec_length in_list =
  let rec pop_last saved_list= function
  | [] -> saved_list
  | [last] -> saved_list
  | hd::tl -> pop_last (saved_list@[hd]) tl
  in 
    let rec hd_rec_find_len cur_length in_list =
    match in_list with  
      | [] -> cur_length
      | hd::tl -> hd_rec_find_len (cur_length+1) (pop_last [] in_list)
      in hd_rec_find_len 0 in_list
;;