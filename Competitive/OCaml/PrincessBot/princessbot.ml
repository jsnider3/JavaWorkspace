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

let rec grid_locs rows n p r =
  match rows with
    0 -> (p, r)
    |_ -> let ln = String.to_list (read_line ()) in
      let p_search = List.findi ln ~f:(fun a b -> b = 'p') in
      let r_search = List.findi ln ~f:(fun a b -> b = 'm') in
      let new_p = if p_search = None then p else (n, first_exn p_search) in
      let new_r = if r_search = None then r else (n, first_exn r_search) in
      grid_locs (rows - 1) (n + 1) new_p new_r;;

let rec dotimes f n = match n with
  0 -> ()
  |_ -> f (); dotimes f (n - 1);;

let print_updown n = if n > 0 then dotimes (fun a -> printf "DOWN\n") n
  else if n < 0 then dotimes (fun a -> printf "UP\n") (-n);;

let print_leftright n = if n > 0 then dotimes (fun a -> printf "LEFT\n") n
  else if n < 0 then dotimes (fun a -> printf "RIGHT\n") (-n);;

let () =
  let rows = Int.of_string (read_line ()) in
  read_line();
  let ((py, px), (ry, rx)) = grid_locs rows 0 (-1, -1) (-1, -1) in
  if px < rx then printf "LEFT\n"
  else if px > rx then printf "RIGHT\n"
  else if py > ry then printf "DOWN\n"
  else if py < ry then printf "UP\n"
