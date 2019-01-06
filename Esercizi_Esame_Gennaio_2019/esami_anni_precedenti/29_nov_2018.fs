module _29_nov_2018

(*
Esercizio 1. Scrivere una funzione 
ricorsiva conta valli che data una 
lista di elementi su cui e definito 
un ordinamento, conta gli elementi 
che sono minori di tutti i successivi.

Per esempio:

conta_valli [ ];;
//val it : int = 0

conta_valli [6; 4; 5; 3; 1; 10; 3; 3];;
//val it : int = 2

conta_valli [6.0; 4.1; 8.2; 6.5; 1.2; 10.2; 3.5; 7.4; 5.3; 4.0];;
//val it : int = 3

conta_valli ['a'];;
//val it : int = 1

Infine si scriva il tipo della funzione conta valli.

Suggerimento: generalizzare la funzione conta valli.
Per esempio, si puo definire la funzione ausiliaria 
conta valli min che, data una lista non vuota di
elementi su cui e definito un ordinamento, 
restituisca una coppia ` (n,min) dove n e il numero delle `
valli nella lista l e min e il minimo elemento della lista.
*)
let rec conta_valli_min lst =
    match lst with 
    |[x]->(x,1)
    |x::xs -> let (vprima,count) = conta_valli_min xs
                in if x >= vprima then (vprima,count)
                else (x,count + 1)
let rec conta_valli lst =
    match lst with 
    |[]->0
    |x::xs->let (min,count) = conta_valli_min lst 
                in count

(*
Esercizio 1. Scrivere una funzione 
ricorsiva count peaks che data una 
lista di elementi su cui e defi- `
nito un ordinamento, conta gli elementi
che sono maggiori di tutti i successivi.

Per esempio:

countpeaks [ ];;
//val it : int = 0
countpeaks [6; 4; 5; 3; 1; 10; 3; 3];;
//val it : int = 2
countpeaks [6.0; 4.1; 8.2; 6.5; 10.2; 1.0; 7.5; 2.4; 1.3; 4.0];;
//val it : int = 3
countpeaks ['a'];;
//val it : int = 1

Infine si scriva il tipo della funzione count peaks.
Suggerimento: generalizzare la funzione count peaks.
Per esempio, si puo definire la funzione ausiliaria ` 
count peaks max che, data una lista non vuota
di elementi su cui e definito un ordinamento,
restituisca una coppia ` (n,max) dove n e il numero dei `
picchi nella lista l e max e il massimo elemento della lista
*)
let rec isMax elem lst =
    match lst with
    |[]-> true
    |x::xs -> elem > x && isMax elem xs

let rec countpeaks lst =
    match lst with 
    |[]->0
    |x::xs when (isMax x xs) -> 1 + countpeaks xs
    |x::xs -> countpeaks xs