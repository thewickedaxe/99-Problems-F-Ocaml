(* Question *)
(*  Find the last but one (last and penultimate) elements of a list. (easy) *)

(* Code *)

let rec last_two = function
  | [] -> None
  | [pen_ultimate ; ultimate] -> Some(pen_ultimate, ultimate)
  | head::tail -> last_two tail
;;

last_two [ "a" ; "b" ; "c" ; "d" ];;
last_two [ "a" ];;
