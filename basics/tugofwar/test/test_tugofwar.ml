open Tugofwar

let%test _ = toklist_of_string "" = []
let%test _ = toklist_of_string "A" = [A]
let%test _ = toklist_of_string "B" = [B]
let%test _ = toklist_of_string "AAA" = [A; A; A]
let%test _ = toklist_of_string "BBB" = [B; B; B]
let%test _ = toklist_of_string "AABBA" = [A; A; B; B; A]
let%test _ = toklist_of_string "AA==BB" = [A; A; B; B]
let%test _ = toklist_of_string "=AB" = [A; B]
let%test _ = toklist_of_string "AB=" = [A; B]
let%test _ = toklist_of_string "===" = []
let%test _ = toklist_of_string "ABABAB" = [A; B; A; B; A; B]

(* Test per caratteri non validi *)
let%test _ = 
  try 
    let _ = toklist_of_string "ABC" in 
    false 
  with Failure _ -> true

let%test _ = 
  try 
    let _ = toklist_of_string "A1B" in 
    false 
  with Failure _ -> true

(* Test per valid *)
let%test _ = valid []
let%test _ = valid [A]
let%test _ = valid [B]
let%test _ = valid [A; A; A]
let%test _ = valid [B; B; B]
let%test _ = valid [A; A; B; B; A]
let%test _ = valid [A; B; A; B; A; B]
let%test _ = valid [B; B; B; B; B]

(* Test per win *)
let%test _ = win [] = X
let%test _ = win [A] = A
let%test _ = win [B] = B
let%test _ = win [A; A; B] = A
let%test _ = win [B; B; A] = B
let%test _ = win [A; B] = X
let%test _ = win [A; A; B; B] = X
let%test _ = win [A; A; A; B; B] = A
let%test _ = win [B; B; B; B; A] = B
let%test _ = win [A; A; A; A; A; A; A; A; A; A] = A
let%test _ = win [B; B; B; B; B; B; B; B; B; B] = B
let%test _ = win [A; B; A; B; A; B] = X

(* Test per string_of_winner *)
let%test _ = string_of_winner A = "A"
let%test _ = string_of_winner B = "B"
let%test _ = string_of_winner X = "X"

(* Test integrati *)
let%test _ = 
  let tokens = toklist_of_string "AAA" in
  valid tokens && win tokens = A

let%test _ = 
  let tokens = toklist_of_string "BBB" in
  valid tokens && win tokens = B

let%test _ = 
  let tokens = toklist_of_string "AB" in
  valid tokens && win tokens = X

let%test _ = 
  let tokens = toklist_of_string "AABB" in
  valid tokens && win tokens = X

let%test _ = 
  let tokens = toklist_of_string "AAAB" in
  valid tokens && win tokens = A

let%test _ = 
  let tokens = toklist_of_string "ABBB" in
  valid tokens && win tokens = B

let%test _ = 
  let tokens = toklist_of_string "AA=BB=" in
  valid tokens && win tokens = X

let%test _ = 
  let tokens = toklist_of_string "===AAA===" in
  valid tokens && win tokens = A