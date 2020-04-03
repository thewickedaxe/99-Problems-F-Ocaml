(* 7. Flatten a nested list structure. (medium)
# (* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list
Solution

# flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"] *)

type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

(* Non Tail Recursive *)

let nt_flatten nested_list = 
  let rec aux_flatten flattened_list = function
  | [] -> flattened_list
  | hd::tl -> match hd with
              | One x -> x::(aux_flatten flattened_list tl)
              | Many l -> (aux_flatten flattened_list l)@(aux_flatten flattened_list tl)
  in aux_flatten [] nested_list
;;

(* Tail Recursive *)

let flatten nested_list = 
  let rec aux_flatten flattened_list = function
  | [] -> flattened_list
  | hd::tl -> match hd with
              | One x -> aux_flatten (x::flattened_list) tl
              | Many l -> aux_flatten (aux_flatten flattened_list l) tl
  in 
  let rec reverse reversed_list = function
  | [] -> reversed_list
  | hd::tl -> reverse (hd::reversed_list) tl
  in reverse [] (aux_flatten [] nested_list)
;;

(* Tests *)

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
flatten [ One "a" ];;
flatten [];;

nt_flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
nt_flatten [ One "a" ];;
nt_flatten [];;