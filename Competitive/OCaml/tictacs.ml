(**
  Given perfect play by x against a person who moves randomly,
    what are the odds that X wins a game of tic-tac-toe?
*)

open Core.Std
open Printf

(**
  Returns if the characters in st at the positions given by cs
    are all equal to c.
*)
let ident st cs c =
  List.fold cs ~init:true ~f:(fun a b -> a && String.get st b = c);;

(**
  Determine if c has three-in-a-row in the given bd.
*)
let haswon bd c =
  (ident bd [0; 3; 6] c) || (ident bd [1; 4; 7] c) ||
  (ident bd [2; 5; 8] c) || (ident bd [0; 1; 2] c) ||
  (ident bd [3; 4; 5] c) || (ident bd [6; 7; 8] c) ||
  (ident bd [0; 4; 8] c) || (ident bd [2; 4; 6] c);;

(**
  X and O either make the same number of moves or it's O's turn and
    X is ahead by one.
*)
let valid bd =
  let xs = String.count bd (fun a -> a = 'X') in
  let os = String.count bd (fun a -> a = 'O') in
  String.length bd = 9 && (xs = os || xs + 1 > os);;


let is_full bd =
  None = String.index bd '-';;

let gameover bd =
  assert(valid bd);
  haswon bd 'X' || haswon bd 'O' || is_full bd;;

(**
  Given knowledge that bd is a finished game,
    returns the winner or '-' for a draw.
*)
let winner bd = if haswon bd 'X' then 'X'
                else if haswon bd 'O' then 'O'
                else '-'
            
let average ls =
  (List.fold ~f:(+.) ~init:0.0 ls) /. Float.of_int(List.length ls);;

let empties bd =
  List.filter [0; 1; 2; 3; 4; 5; 6; 7; 8] (fun a -> String.get bd a = '-');;

let max_list ls =
  List.fold ~init:(List.hd_exn ls) ~f:max ls;;

(**
  Return a string equal to bd with bd[n] <- c without
    mutating the original.
*)
let copy_set bd c n = let newb = String.copy bd in
  String.set newb n c;
  (*print_endline ("Start with " ^ bd);
  print_endline ("End   with " ^ newb);*)
  newb;;

let children bd c = List.map (empties bd) (copy_set bd c);;

let rec oturn bd =
  assert(valid bd);
  if gameover bd then
    match winner bd with
    'X' -> 1.0
    |_ -> 0.0 
  else
    let results = List.map (children bd 'O') xturn in
      printf "O to move at \"%s\". X wins %f.\n" bd (average results);
      average results

and xturn bd =
  assert(valid bd);
  if gameover bd then
    match winner bd with
    'X' -> 1.0
    |_ -> 0.0 
  else
    let results = List.map (children bd 'X') oturn in
      printf "X to move at \"%s\". X wins %f.\n" bd (max_list results);
      max_list results;;

let test_winner () =
  assert (winner "XXX-OO---" = 'X');
  assert (winner "OOOX--X--" = 'O');
  assert (oturn "OOXXXOXOX" = 1.0);;

let test_children () =
  print_endline "Children of \"X-X---O-\"";
  List.iter (children "X--X---O-" 'O') (print_endline);
  print_endline "Children of \"OX--XOXOX\"";
  List.iter (children "OX--XOXOX" 'X')
    (fun a -> print_endline (a ^ " winner " ^ String.of_char (winner a)));;

let test_score () =
  assert (oturn "OX--XOXOX" = 0.5);
  assert (oturn "---X-OXOX" = 0.875);
  assert (oturn "--O-X-XOX" = 0.75);
  assert (xturn "X--OX-O-X" = 1.0);
  assert (xturn "-------OX" = 1.0);
  assert (xturn "OO-XXOXOX" = 1.0);;

(**
  Main - run some tests, then answer the question.
  It currently says that X has a 99.4792% of winning
  and a 0.5208% chance of drawing.
  
  Is this believable? Probably, if both X and O were playing
  randomly X would win 51.4% of the time, lose 30.5% of the time,
  and draw 18.1% of the time.
*)
let () =
  test_winner ();
  test_children ();
  test_score ();
  printf "%f\n" (xturn "X---O----");
  printf "Odds of winning from start: %f.\n" (xturn "---------")
