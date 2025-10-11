open Recognizer

let%test _ = lang1 ['0']
let%test _ = lang1 ['1'; '0'; '1']
let%test _ = not (lang1 [])
let%test _ = not (lang1 ['0'; '2'])   (* '2' non è valido *)
let%test _ = not (lang1 ['a'; 'b'])   (* lettere non binarie *)

let%test _ = lang2 []                        (* ε *)
let%test _ = lang2 ['1']
let%test _ = lang2 ['1'; '1'; '1']
let%test _ = lang2 ['0'; '1'; '1']
let%test _ = not (lang2 ['0'; '0'])
let%test _ = not (lang2 ['1'; '0'])
let%test _ = not (lang2 ['0'; '1'; '0'])
let%test _ = not (lang2 ['2'])              (* input non valido *)

let%test _ = lang3 ['0'; '0']
let%test _ = lang3 ['0'; '1'; '0']
let%test _ = lang3 ['0'; '1'; '1'; '0']
let%test _ = not (lang3 ['1'; '0'])
let%test _ = not (lang3 ['0'; '1'])           (* non termina con '0' *)
let%test _ = not (lang3 ['0'])                (* troppo corta *)
let%test _ = not (lang3 ['0'; '1'; '2'; '0']) (* input non valido *)

let%test _ = lang4 ['1'; '1']
let%test _ = lang4 ['0'; '1'; '0'; '0'; '1']
let%test _ = lang4 ['0'; '0'; '1'; '0'; '1'; '0']
let%test _ = not (lang4 ['1'])             (* solo una '1' *)
let%test _ = not (lang4 ['1'; '1'; '1'])   (* tre '1' *)
let%test _ = not (lang4 ['0'; '0'; '0'])   (* nessuna '1' *)
let%test _ = not (lang4 ['1'; '2'; '1'])   (* input non valido *)

let%test _ = lang5 ['0'; '0']
let%test _ = lang5 ['1'; '1']
let%test _ = lang5 ['0'; '0'; '1'; '1']
let%test _ = lang5 ['1'; '1'; '0'; '0'; '1'; '1']
let%test _ = not (lang5 [])                  (* vuota *)
let%test _ = not (lang5 ['0'])               (* dispari *)
let%test _ = not (lang5 ['0'; '1'])          (* '01' non ammesso *)
let%test _ = not (lang5 ['0'; '0'; '1'])     (* dispari *)
let%test _ = not (lang5 ['0'; '0'; '1'; '0'])(* '10' non ammesso *)
