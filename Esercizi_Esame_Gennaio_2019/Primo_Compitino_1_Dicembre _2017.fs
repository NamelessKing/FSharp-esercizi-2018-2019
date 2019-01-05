module Primo_Compitino_1_Dicembre__2017

(*
Esercizio 1. Scrivere una funzione ricorsiva applica 
rovescia che data una lista di coppie (elemento, funzione)
restituisca la lista che si ottiene concatenando i 
risultati ottenuti dall’applicazione di ogni
funzione al primo elemento della coppia. Gli elementi
in output devono comparire in ordine inverso
rispetto alle relative coppie in input.

Per esempio:

> applica rovescia [(3, (fun x -> x + 1)); (7, (fun x -> x - 1))];;
val it : int list = [6; 4]

> applica rovescia [(8.4, (fun x -> x * 2.0)); (9.0, (fun x -> x / 3.0))];;
val it : float list = [3.0; 16.8]

> applica rovescia [(’a’, (fun x -> x = ’a’)); (’c’, (fun x -> x = ’b’))];;
val it : bool list = [false; true]

Infine si scriva il tipo della funzione applica rovescia.
*)

let rec applica_rovescia (lst) =
    match lst with 
    |[]->[]
    |(data,f)::xs->(applica_rovescia xs ) @ [f data]
//val applica_rovescia : l:(’a * (’a -> ’b)) list -> ’b list

(*

Esercizio 2. Scrivere una funzione ricorsiva ripeti che 
data una lista l e un intero n restituisca una
lista dove ogni elemento di l viene ripetuto n volte.

Per esempio:

> ripeti [1; 2; 3] 4;;
val it : int list = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3]

> ripeti [’a’; ’b’; ’c’] 3;;
val it : char list = [’a’; ’a’; ’a’; ’b’; ’b’; ’b’; ’c’; ’c’; ’c’]

> ripeti [1.0; 2.0; 2.0; 3.0] 2;;
val it : float list = [1.0; 1.0; 2.0; 2.0; 2.0; 2.0; 3.0; 3.0]

> ripeti [’a’; ’b’; ’c’] 0;;
val it : char list = []
*)

let rec ripeti lst n =
    match lst with 
    |[]->[]
    |x::xs->
            let rec aux n =
                match n with 
                |0->[]
                |_->x::(aux (n - 1))
            in (aux n) @ (ripeti xs n)