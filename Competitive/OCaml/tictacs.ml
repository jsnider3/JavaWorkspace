open Core.Std
open Printf


let haswon bd c =
  (String.nget bd 0 = String.nget bd 3 && String.nget bd 3 = String.nget bd 6 && String.nget bd 6 = c) ||
  (String.nget bd 1 = String.nget bd 4 && String.nget bd 4 = String.nget bd 7 && String.nget bd 7 = c) ||
  (String.nget bd 2 = String.nget bd 5 && String.nget bd 5 = String.nget bd 8 && String.nget bd 8 = c) ||
  (String.nget bd 0 = String.nget bd 1 && String.nget bd 1 = String.nget bd 2 && String.nget bd 2 = c) ||
  (String.nget bd 3 = String.nget bd 4 && String.nget bd 4 = String.nget bd 5 && String.nget bd 5 = c) ||
  (String.nget bd 6 = String.nget bd 7 && String.nget bd 7 = String.nget bd 8 && String.nget bd 8 = c) ||
  (String.nget bd 0 = String.nget bd 4 && String.nget bd 4 = String.nget bd 8 && String.nget bd 8 = c) ||
  (String.nget bd 2 = String.nget bd 4 && String.nget bd 4 = String.nget bd 6 && String.nget bd 6 = c);;

let gameover bd =
  None = String.index bd ' ' || haswon bd 'X' || haswon bd 'O';;

let winner bd = if haswon bd 'X' then 'X'
                else if haswon bd 'O' then 'O'
                else ' '
            

let empties bd =
  List.filter [0; 1; 2; 3; 4; 5; 6; 7; 8] (fun a -> String.nget bd a = ' ');;

let average ls =
  (List.fold ~f:(+.) ~init:0.0 ls) /. Float.of_int(List.length ls);;

let str_set bd n c =
  String.set bd n c;
  bd;;

let rec oturn bd =
  if gameover bd then
    match winner bd with
    'X' -> 1.0
    |_ -> 0.0 
  else
    let spawn_x bd n = xturn (str_set (String.copy bd) n 'O') in
    let results = List.map (empties bd) (spawn_x bd) in
      printf "\"%s\" %f\n" bd (average results);
      average results

and xturn bd =
  if gameover bd then
    match winner bd with
    'X' -> 1.0
    |_ -> 0.0 
  else
    let spawn_o bd n = oturn (str_set (String.copy bd) n 'X') in
    let results = List.map (empties bd) (spawn_o bd) in
      List.fold ~init:(List.hd_exn results) ~f:max results;;

assert (xturn "XXXXXXXXX" = 1.0);
assert (xturn "X   X   X" = 1.0);
assert (xturn "XOOOOOOOX" = 0.0);
printf "%f\n" (xturn "X   X    ");
(*printf "Odds of winning are %f.\n" (xturn Sys.argv.(1))*)
