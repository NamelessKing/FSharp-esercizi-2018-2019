module Esercitazione5
(*
Esercizio 1:

Scrivere la funzione ricorsiva

last_index : x:'a -> l:'a list -> int when 'a : equality
che, dato un valore x e una lista l, restituisce la 
posizione dell'ultima occorenza di x nella lista. 
Se x non è presente nella lista l la funzione restituisce -1.

Esempi di test:

 last_index 1 [1; 2; 3];;
//val it : int = 0

 last_index 3 [3; 4; 5; 1; 3; 5];;
//val it : int list = 4

 last_index 2 [];;
//val it : int list = -1
*)
let rec last_index v lst =
    let rec aux i vpos lst=
        match lst with 
        |[]->vpos
        |x::xs when x = v -> aux (i + 1) (i) xs
        |x::xs -> aux (i + 1) (vpos) xs
    in aux 0 -1 lst

(*
Esercizio 2:

Scrivere la funzione ricorsiva

delete : l:'a list -> 'a list when 'a : equality
che, data una lista l, non vuota, elimina dalla 
lista tutti gli elementi che seguono l'ultima 
sequenza di due elementi  consecutivi uguali, 
i due elementi non vanno eliminati.

Esempi di test:

delete [1;2;3;4;4;4;4;5;6;7];;
//val it : int list = [1; 2; 3; 4; 4; 4; 4]

delete [1;2;3;4;4;4;4;5;6;7;7];;
//val it : int list = [1; 2; 3; 4; 4; 4; 4; 5; 6; 7; 7]

delete [1;1];;
//val it : int list = [1; 1]

delete [1];;
//val it : int list = [1]

delete [1;2;3;4;5;6];;
//val it : int list = [1; 2; 3; 4; 5; 6]
*)

let rec auxdelete lst =
    match lst with 
    |[x]->([x],x,false)
    |x::xs-> 
            let (l,vprima,bool) = auxdelete xs in
            if bool=true then (x::l,x,true) else 
            if x = vprima then (x::vprima::[],x,true)
            else (x::l,x,bool)

let rec delete lst =
    let(result,_,_) = auxdelete lst
    in result

(*
val it : int list = [1; 2; 3; 4; 5; 6]
Esercizio 3:

Scrivere la funzione ricorsiva

duplica : l:'a list -> 'a list when 'a : equality
che, data una lista l non vuota, restituisce la 
lista in cui tutte le coppie di elementi consecutivi vengono duplicate.
Esempi di test:

duplica [1];;
//val it : int list = [1]

duplica [1;2;2];;
//val it : int list = [1; 2; 2; 2; 2]

 duplica [1;2;2;3;2;4];;
//val it : int list = [1; 2; 2; 2; 2; 3; 2; 4]

 duplica [1;2;2;2;3;2;4];;
//val it : int list = [1; 2; 2; 2; 2; 2; 3; 2; 4]
*)
let rec duplica lst =
    match lst with 
    |[]->[]
    |[x]->[x]
    |x::y::xs when x = y -> x::y::x::y:: (duplica xs)
    |x::y::xs -> x::duplica(y::xs)
    


