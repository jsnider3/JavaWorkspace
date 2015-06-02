open Core.Std
open Printf
open Scanf

(** 
  Needs work.
*)

let signum n = if n < 0 then -1 
  else if n = 0 then 0 else
  1;;

let first_exn o = match o with
  Some (i, c) -> i
  |None -> invalid_arg "You promised me this wouldn't happen.";;

let rec read_grid chan rows cnt =
  match cnt with
    0 -> List.rev rows
    |_ -> let ln = String.to_list (input_line chan) in
      read_grid chan (ln::rows) (cnt - 1);;

let rec dotimes f n = match n with
  0 -> ()
  |_ -> f (); dotimes f (n - 1);;

let matrix_of_grid ls = Array.of_list (List.map ls Array.of_list);;

let () =
  let chan = open_in Sys.argv.(1) in
  let rows = Int.of_string (input_line chan) in
  let file = matrix_of_grid (read_grid chan [] rows) in
  Array.iter file ~f:(fun a -> Array.iter a ~f:(printf "%c"); print_newline ());
  printf "%c\n" file.(1).(0)
  
