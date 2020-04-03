(* 6. Find out whether a list is a palindrome. (easy)
HINT: a palindrome is its own reverse.

Tests

# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true *)

let is_palindrome in_list =
  let rec reverse reversed_list = function
  | [] -> reversed_list
  | hd::tl -> reverse (hd::reversed_list) tl
  in match in_list with
  | [] -> true
  | hd::tl -> (in_list = reverse [] in_list)
;;

(* Testing
─( 12:28:49 )─< command 3 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
─( 12:29:01 )─< command 4 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # is_palindrome [ "x" ; "a" ; "m" ; "a"  ];;
- : bool = false
─( 12:29:06 )─< command 5 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # is_palindrome [ "x" ];;
- : bool = true
─( 12:29:12 )─< command 6 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # is_palindrome [  ];;
- : bool = true

*)