module Esercitazione4

(*
Esercizio 1:

Scrivere la funzione ricorsiva

semplifica : l:'a list list -> 'a list
che, data una lista, non vuota, di liste, 
non vuote, l, restituisce la lista ottenuta
concatenando tutte le liste contenute in l.

Esempi di test:

semplifica [[1]; [2]; [3]];;
//val it : int list = [1; 2; 3]

semplifica [[1; 2; 3]; [22]; [3; 0; 9]];;
//val it : int list = [1; 2; 3; 22; 3; 0; 9]

semplifica [[1]];;
//val it : int list = [1]
*)
let rec semplifica lst =
    match lst with
    |[]->[]
    |x::xs -> x@semplifica xs

(*
Esercizio 2:

Scrivere la funzione ricorsiva

sum_by : f: (int -> int -> int) -> l:int list -> int

che, data una funzione f e una lista l, 
restituisce l'applicazione della funzione 
f al primo elemento della lista e al risultato
della funzione sum_by avente come parametri 
la stessa funzione f e la coda della lista l. 
Se la lista l è vuota la funzione restituisce il valore 0.

Esempi di test:

sum_by (+) [1; 2; 3];;
//val it : int = 6

sum_by (-) [1; 2; 3];;
//val it : int = 2

sum_by (*) [1; 2; 3];;
//val it : int = 0

sum_by (+) [];;
//val it : int = 0
*)
let rec sum_by f lst =
    match lst with
    |[]->0
    |x::xs-> f x (sum_by f xs) 

(*
Esercizio 3:

Scrivere la funzione

inversa : a:'a list -> b:'a list -> bool when 'a : equality
che, date due liste a e b restituisce 
true se b è uguale alla lista inversa di a.

Esempi di test:

 inversa [1; 2; 3] [3; 2; 1];;
//val it : bool = true
 inversa [] [];;
//val it : bool = true
inversa [1; 2; 3] [1];;
//val it : bool = false
inversa [1; 2; 3] [1; 2; 3];;
//val it : bool = false
*)
let rec inversa l1 l2 =
    let rec rev l1 = 
        match l1 with 
        |[]->[]
        |x::xs -> (rev xs) @ [x]
    in l2 = rev l1