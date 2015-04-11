open Core.Std;;
open Printf;;

let rec readlines ic n = match n with
  0 -> []
  |_ -> let ln = input_line ic in 
          ln :: (readlines ic (n - 1));;

let words strn = String.split strn ~on:' ';;

let int_list_of_string strn = List.map (List.map (String.to_list strn) String.of_char) Int.of_string;;

let print_ints ls = List.map (ls) (printf "%d ");
  print_newline ();;

let add_friends ls = 5;
  let rec friends_loop lis ind sum friends = match lis with
    [] -> friends
    |(x::xs) -> match sum < ind with
                  true -> friends_loop lis ind ind (friends + ind - sum)
                  |false -> friends_loop xs (ind + 1) (sum + x) friends in
  friends_loop ls 0 0 0;;

let test_sample () = let fl = open_in "Sample.txt" in
  let numtests = Int.of_string (input_line fl) in
  assert (numtests = 4);
  let tests = List.map (List.map (readlines fl numtests) (fun x -> List.nth_exn (words x) 1)) int_list_of_string in
  assert(List.map tests add_friends = [0; 1; 2; 0]);;

let test_custom () = 
  assert(add_friends [0; 1] = 1);
  assert(add_friends [0; 9] = 1);
  assert(add_friends [0; 0; 0; 0; 0; 9] = 5);;
  assert(add_friends [5; 0; 0; 0; 0; 9] = 0);;
  assert(add_friends [1; 0; 1; 0; 1] = 2);;

let small_file () = let fl = open_in "A-small-attempt0.in" in
  let numtests = Int.of_string (input_line fl) in
  let tests = List.map (List.map (readlines fl numtests) (fun x -> List.nth_exn (words x) 1)) int_list_of_string in
  List.mapi (List.map tests add_friends) (fun x y -> printf "Case #%d: %d\n" (x + 1) y);;

let main () = test_sample ();
  test_custom ();
  small_file ();;

main ();;
