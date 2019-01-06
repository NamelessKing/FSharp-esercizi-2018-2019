module _26_gen_2018

(*
Esercizio 1
Scrivere una funzione ordinati: ’a -> ’a list -> ’a list che,
dato un elemento init e una
lista input, restituisca la lista output tale che:

• il primo elemento della lista output `e il primo 
elemento della lista input maggiore o uguale a
init (se input contiene un tale elemento); gli 
elementi successivi della lista output si ottengono
eliminando dalla lista input tutti quelli che 
non renderebbero la lista output ordinata. In altre
parole, gli elementi della lista output compaiono 
nello stesso ordine in cui occorrono nella lista
input, e ciascuno `e maggiore o uguale al prededente.

• Se la lista input non contiene alcun elemento 
maggiore o uguale a init, allora output = [].

Ad esempio,
ordinati 3 [1;2;3;0;5;4;8;7;10;9] = [3; 5; 8; 10]
*)

let rec order_list lst =
    match lst with
    |[]->[]
    |[x]->[x]
    |x::y::xs when x < y -> x::(order_list (y::xs) )
    |x::y::xs when x >= y -> order_list(x::xs)

let rec ordinati init list =
    match list with 
    |[]->[]
    |x::xs when x>=init -> order_list(x::xs)
    |x::xs -> ordinati init xs

(*
Esercizio 2
Scrivere una funzione sub ord: 
’a list -> ’a list list, 
che, applicata a una lista di elementi
(di un tipo con uguaglianza) lst, restituisca 
una lista di liste, ciascuna delle quali `e ordinata (in
senso non decrescente) e contiene il massimo 
numero possibile di elementi consecutivi di lst. 
Ciascun elemento di lst deve occorrere in una 
ed un’unica lista di sub ord lst.
Ad esempio:
sub ord [4;4;10;20;5;30;6;10] = [[4;4;10;20];[5;30];[6;10]]
sub ord [5;6;4;3;2;1] = [[5;6];[4];[3];[2];[1]]
*)
let rec partiton ls resList restList = 
    match ls with 
    |[]->(resList,restList)
    |[x]->(resList@[x],restList)
    |x::y::xs when x<=y -> partiton (y::xs) (resList@[x]) (restList)
    |x::y::xs -> partiton ([]) (resList@[x]) (y::xs)

let rec suborder lst =
    match lst with 
    |[]->[]
    |x::xs-> let (resList,restList) = (partiton lst [] [] )
                in resList :: (suborder restList)