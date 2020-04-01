
(* Find the last element of a list. *)

let rec last x =
 match x with
 | [] -> None
 | [x] -> Some x
 | _::tl -> last tl
;;

assert (last [ `a ; `b ; `c ; `d ] = Some `d) ;;
assert (last [] = None) ;;
