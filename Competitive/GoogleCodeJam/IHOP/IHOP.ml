open Core.Std;;
open Printf;;

let rec readlines ic n = match n with
  0 -> []
  |_ -> let ln = input_line ic in 
          ln :: (readlines ic (n - 1));;

let words strn = String.split strn ~on:' ';;

let int_list_of_string strn = List.map (words strn) Int.of_string;;

let print_ints ls = List.map ls (printf "%d ");
  print_newline ();;

let rec every_other ls = match ls with 
  [] -> []
  |[x] -> [x]
  |(x::xs) -> x :: (every_other (List.tl_exn xs));;

let filter_tests t = every_other(List.tl_exn t);;

let should_split srt = 
  let rec should_helper ls run = match ls with
    [] -> raise(Invalid_argument "Should be impossible")
    |[x] -> if x > run + 1 then run else 0
    |(x::xs) -> if x > (List.hd_exn xs) + run 
                  then run
                  else should_helper xs (run + 1) 
  in
    should_helper srt 1;;

let div2 n = [n/2; n - n/2];;

let splitify ls = List.concat (List.map ls div2);;

let rec take ls n = match n with
  0 -> []
  |_ -> List.hd_exn ls :: take (List.tl_exn ls) (n - 1);;

let rec drop ls n = match n with
  0 -> ls
  |_ -> drop (List.tl_exn ls) (n - 1);;

let rec solve ls = let srt = List.sort ~cmp:(fun a b -> - compare a b) ls in
  (*print_ints srt;*)
  let split = should_split srt in
  (*printf "Should split %d.\n" split;*)
  if split > 0
    then split + solve (List.concat[splitify (take srt split);drop srt split])
    else List.hd_exn srt;; 

let test_sample () = let fl = open_in "Sample.txt" in
  let numtests = Int.of_string (input_line fl) in
  let tests = List.map (filter_tests(readlines fl (2 * numtests))) int_list_of_string in
  assert(List.map tests solve = [3; 2; 3]);;
(*(fun x -> List.nth_exn (words x) 1)) int_list_of_string in
  assert(List.map tests add_friends = [0; 1; 2; 0]);;*)

let test_small () = let fl = open_in "Sample.txt" in
  let numtests = Int.of_string (input_line fl) in
  let tests = List.map (filter_tests(readlines fl (2 * numtests))) int_list_of_string in
  List.mapi tests (fun x y-> printf "Case #%d: %d\n" (x+1) (solve y));
  assert(List.map tests solve = [3; 2; 3]);;

let test_various () = assert(solve[5] = 4);
  assert(solve[5;4;3;2;1] = 5);
  assert(solve[5;4;3;2] = 5);
  assert(solve[5;4;3] = 5);
  assert(solve[5;4] = 5);
  assert(solve[4;1;1;1] = 3);;
  assert(solve[4;3;1] = 4);;

let main () = 
  test_sample ();
  test_various ();;

main ();;
