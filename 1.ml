
(* Find the last element of a list. *)

let rec last x =
 match x with
 | [] -> None
 | [x] -> Some x
 | _::tl -> last tl
;;
assert (last [1;2;3;4] = None) ;;
