(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let rec toklist_of_charlist chars =
  match chars with
  | [] -> []
  | 'A' :: t -> A :: toklist_of_charlist t
  | 'B' :: t -> B :: toklist_of_charlist t
  | '=' :: t -> toklist_of_charlist t
  | c :: _ -> failwith ("Unrecognized symbol: " ^ String.make 1 c)

let toklist_of_string s =
  let chars = explode s in
  toklist_of_charlist chars

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = List.for_all (fun t -> t = A || t = B) l;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let count l x = List.filter (fun y -> x = y) l |> List.length;;
let win l = if (count l A > count l B) then A else if (count l A < count l B) then B else X;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with | A -> "A" | B -> "B" | X -> "X";;
