(* // Example in F#: 

// > reverse <| List.ofSeq ("A man, a plan, a canal, panama!")
// val it : char list =
//  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
//   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
//   'A']
// > reverse [1,2,3,4];;
// val it : int list = [4; 3; 2; 1] *)

let reverse inList =
    let rec reverseAux savedList = function
    | [] -> []
    | hd::tl -> reverseAux (hd::savedList) tl
    in
    reverseAux [] inList
;;
(* Test *)
reverse [1;2;3];;
reverse([]);;
reverse ['a';'b';'c'];;