(** returns true if the list is empty or contains only '0' or '1'*)
let validDomain l = List.for_all (fun x -> x = '0' || x = '1') l;;

(* [01]+ *)
let lang1 (l: char list) : bool = (validDomain l) && List.length l > 0;;

(* 0?1* *)
let lang2 (l: char list) : bool = if (not (validDomain l)) then false else match l with
| [] -> true
| _ :: t -> List.for_all (fun x -> x = '1') t;;

(* 0[01]*0 *)
let lang3 (l: char list) : bool = (List.length l >= 2) && (validDomain l) && (List.nth l 0 = '0') && (List.nth l ((List.length l) - 1) = '0')

(* 0*10*10* *)
let lang4 (l: char list) : bool = (validDomain l) && (List.filter (fun x -> x = '1') l |> List.length = 2)

(* (00|11)+ *)
let lang5 (l: char list) : bool =
  if not (validDomain l) || (List.length l mod 2 <> 0) || (List.length l = 0) then false
  else
    let rec check_pairs lst =
      match lst with
      | [] -> true
      | '0'::'0':: t -> check_pairs t
      | '1'::'1':: t -> check_pairs t
      | _ -> false
    in
    check_pairs l
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
(* No word belongs to all five languages because every word in lang2 can have at most only one '0' 
and the words of lang3 must have at least 2 '0'*)