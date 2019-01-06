module Esercitazione6

(*
Esercizio 1:

Scrivere la funzione

che, dato un valore size (intero positivo non nullo) 
e una lista l (non vuota), restituisce la lista di 
liste ottenuta dividendo l in liste di lunghezza al più size.

Esempi di test:

> chunkBySize 1 [1;2;3;4;5;6;7;8;9];;
val it : int list list = [[1]; [2]; [3]; [4]; [5]; [6]; [7]; [8]; [9]]

> chunkBySize 2 [1;2;3;4;5;6;7;8;9];;
val it : int list list = [[1; 2]; [3; 4]; [5; 6]; [7; 8]; [9]]

> chunkBySize 3 [1;2;3;4;5;6;7;8;9];;
val it : int list list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]

> chunkBySize 8 ['a';'b';'c';'d';'e'];;
val it : char list list = [['a'; 'b'; 'c'; 'd'; 'e']]
*)
let rec divide lst n =
    match (lst,n)with 
    |([],_)->([],[])
    |(_,0)-> ([],lst)
    |x::xs,nan-> 
                let (l1,l2) = divide xs (n - 1)
                in (x::l1,l2)

let rec chunkBySize n lst =
    match lst with
    |[]->[]
    |x::xs->    
            let (l1,l2) = divide lst n 
            in [l1]@chunkBySize n l2

(*
Esercizio 2:

Scrivere la funzione 

che, data una lista l, non vuota, restituisce 
la lista contenente tutti gli elementi di l senza duplicati.

Esempi di test:

> distinct [1;2;3;4;5;8;3;6;2;7;8;0;4];;
val it : int list = [1; 2; 3; 4; 5; 8; 6; 7; 0]

> distinct [1;2;3;4];;
val it : int list = [1; 2; 3; 4]

> distinct [1;1;2;3;4;2;4;5];;
val it : int list = [1; 2; 3; 4; 5]
*)
let rec distinct lst =
    let rec remove lst elem =
        match lst with
        |[]->[]
        |x::xs when elem = x -> remove xs elem
        |x::xs -> x::(remove xs elem)
    in
    match lst with 
    |[]->[]
    |x::xs -> x::(distinct (remove xs x))

(*
Esercizio 3:

Scrivere la funzione ricorsiva

partition : p: ('a -> bool) -> l:'a list -> 'a list * 'a list
che, dato un predicato unario boleano p e una lista l 
non vuota, restituisce una coppia di liste in cui nella 
prima lista sono contenuti tutti gli elementi per i quali 
il predicato risulta essere vero, nella seconda lista sono 
contenuti tutti gli elementi per i quali il predicato risulta essere falso.
Esempi di test:

let isEven x = x % 2 = 0

> partition isEven [1];;
val it : int list * int list = ([], [1])

> partition isEven [2];;
val it : int list * int list = ([2], [])

> partition isEven [1;2;3;4;5;6;7;8;9;0];;
val it : int list * int list = ([2; 4; 6; 8; 0], [1; 3; 5; 7; 9])
*)
let rec partition f lst =
    match lst with 
    |[]->[],[]
    |x::xs ->let (pos,neg) = partition f xs in 
                if f x then (x::pos,neg)
                else (pos,x::neg)
