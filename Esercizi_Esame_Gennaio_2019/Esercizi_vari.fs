module Esercizi_vari

let rec take n lst =
    match (n,lst) with 
    |(_,[])->[]
    |(0,_)->[]
    |(n,x::xs)->x :: (take(n-1)(xs))


let rec drop n lst =
    match (n,lst) with 
    |(_,[])->lst
    |(0,_)->lst
    |(n,x::xs)-> (drop(n-1)(xs))

let pack l =
    let rec aux l l1 l2 =
        match l with
        [] -> l2@[l1]
        |[x] -> l2@[l1@[x]] 
        |x::y::xs -> 
            if x<>y then aux (y::xs) [] (l2@[l1@[x]])
            else aux (y::xs) (l1@[x]) l2
    in aux l [] []

(*
10. Run-length encoding of a list. (easy)
If you need so, refresh your memory about run-length encoding.

Here is an example:

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
1
*)

let encode lst =
    let rec aux lst count output =
        match lst with
        |[x]->(output@[(count + 1,x)])
        |x::y::xs when x = y -> aux (y::xs) (count + 1) output
        |x::y::xs ->aux (y::xs) 0 (output@[(count+1,x)])
    in aux lst 0 []

(*
18. Extract a slice from a list. (medium)
Given two indices, i and k, the slice is the list 
containing the elements between the i'th and k'th 
element of the original list (both limits included). 
Start counting the elements with 0 (this is the way 
the List module numbers elements).

# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let rec slice lst i k =
    let rec aux count lst=
        match lst with 
        |[]->[]
        |x::xs when (count >= i && count <= k )->
                    x::(aux (count + 1 ) xs)
        |x::xs -> aux (count + 1 ) xs
    in aux 0 lst


(*
26. Generate the combinations of K distinct 
objects chosen from the N elements of a list. 
In how many ways can a committee of 3 be chosen
from a group of 12 people? We all know that
there are C(12,3) = 220 possibilities (C(N,K)
denotes the well-known binomial coefficients).
For pure mathematicians, this result may be great.
But we want to really generate all the possibilities in a list.


# extract 2 ["a";"b";"c";"d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
2
*)
let rec subseq lst =
    let rec seq_of_elem elem lst =
        match lst with 
        |[]->[]
        |x::xs -> [elem;x] :: (seq_of_elem elem xs)
    match lst with
    |[x]->[]
    |x::xs -> (seq_of_elem x xs)@subseq xs

let rec extract k list =
    if k <= 0 then [ [] ]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
            let without_h = extract k tl in
            with_h @ without_h;;