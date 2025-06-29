open Sys

let rec fact p =
  if p=0 then
    1
  else
    p*fact (p-1)

let fact2 p =
  let rec aux k acc =
    if k=0 then
      acc
    else
      aux (k-1) k*acc 
  in
  aux p 1

let binom n k =
  assert (0<=k && k<=n);
  (fact2 n) / ((fact2 k)*(fact2 (n-k)))

let rec binom2 n k =
  assert (0<=k && k<=n);
  if k=0 || k=n then
    1
  else
    binom2 (n-1) (k-1) + binom2 (n-1) k

let print_line_pascal n =
  let rec aux n k =
    if k=(-1) then
      Printf.printf "\n"
    else
      begin 
        Printf.printf "%d " (binom2 n k);
        aux n (k-1)
      end
  in
  aux n n

let print_triangle_pascal n =
  let rec aux k = 
    if k=(n+1) then
      Printf.printf "\n"
    else
      begin 
        print_line_pascal k;
        aux (k+1)
      end
  in
  aux 0

(*let () =
  let res = fact 0 in
  Printf.printf "%d\n" res;
  let res2 = fact2 5 in
  Printf.printf "%d\n" res2;
  let res3 = binom 5 3 in
  Printf.printf "%d\n" res3;
  let res4 = binom2 5 5 in
  Printf.printf "%d\n" res4;
  print_line_pascal 5;
  triangle_pascal 7*)

let () =
  print_triangle_pascal (int_of_string Sys.argv.(1))
