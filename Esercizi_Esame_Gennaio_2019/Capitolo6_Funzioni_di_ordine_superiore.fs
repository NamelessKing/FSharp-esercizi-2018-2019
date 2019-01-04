module Capitolo6_Funzioni_di_ordine_superiore

let rec num_cifre_che_soddisfano_iter n test = 
    if n < 0 then num_cifre_che_soddisfano_iter (-n) test else
    let rec aux  num countAcc =
        if num < 10  then 
            if test num then countAcc + 1 else countAcc
        else 
            if test (num%10) then aux (num/10) (countAcc + 1)
            else aux (num/10) (countAcc)
    in aux n 0
            

(*
ESERCIZIO	6.4
Scrivere	una	funzione	generale che,	dati in	input	
due	interi,	low e	high,	e	una	funzione	f,	calcoli
f(low) + f(low+1) + ... + f(high).	Mostrare	 come	 
tale	 funzione	 può essere	 usata	per	sommare	i	
quadrati	degli	interi	compresi	tra	5	e 10	e	
per	sommare	le radici	quadrate	degli	interi	
compresi	tra	10	e	20.
*)
let rec sum_low_to_high low high f:float =
    if low = high then f high
    else (f low) + (sum_low_to_high (low + 1.) high f)

(*
ESERCIZIO	6.5
Scrivere	 una	 funzione make_exponentiater che	 
prende	 in	 input	 un	 singolo	 parametro	 e
(esponente)	e	ritorna	una	funzione	che prende	
anch’essa	un	solo	parametro,	che	verrà poi	
elevato	alla	potenza	e.	Come	esempi,	
definire le	funzioni square e	cube come	segue:

# let square = make_exponentiater 2;;
# let cube = make_exponentiater 3;;

# square 4;;
val it: int = 16

# cube 4;;
val it: int = 64
*)

let make_exponentiater e =
    fun x -> (int)((float)x **((float)e))

let square = make_exponentiater 2
let cube = make_exponentiater 3

(*
ESERCIZIO	6.6
Si	considerino	le	funzioni	make_multiplier e	 
make_exponentiater definite	sopra.	Si	noti
che	queste	due	funzioni	sono	molto simili.	
Potremmo	astrarne	le	somiglianze	creando	
una	funzione ancora	più generale	
make_generator tale	da	poter semplicemente	scrivere:

# let make_multiplier = make_generator … ;;
# let make_exponentiater = make_generator expt;;

(dove	 expt è una	 procedura	 che	 calcola	 
l’esponenziale). Scrivere la definizione della funzione
make_generator.
*)
//let make_generator f =

(*
ESERCIZIO	6.7
Definire	una	funzione che	possa	essere	usata	
per	produrre sia	la	funzione factorial (fattoriale)	
che	la	funzione sum_of_first (somma	dei	primi	
n numeri	interi,	dove	n è l’unico	parametro).
*)
let general_fun f = 
    let rec the_repeated_version acc i =
        if i = 1 then acc
        else the_repeated_version ((f acc i)) (i - 1)
    in the_repeated_version 

let fact_rull n x =  n * x
let sum_rull n x =  n + x
let factorial = general_fun fact_rull 1
let sum_of_first = general_fun sum_rull 0

(*
ESERCIZIO 6.8
Generalizzare la	soluzione	dell’esercizio	precedente	
in	modo	tale	che	possa	essere	usata	anche	per	
produrre la	 funzione sum_of_squares (somma	dei	primi	n 
numeri	interi	elevati	al	quadrato)	e	sum_of_cubes 
(somma	dei	primi	n numeri	interi	elevati	al	cubo).
*)
let general_fun_sum_square f = 
    let rec the_repeated_version acc i =
        if i = 0 then 0 else
        if i = 1 then acc
        else the_repeated_version (acc+(f i)) (i - 1)
    in the_repeated_version 

let square2 x = x*x
let sum_of_squares = general_fun_sum_square square2 1

let cube2 x = x*x*x
let sum_of_cube = general_fun_sum_square cube2 1

//=======================================================
(*6.4 – Funzioni	di	ordine	superiore	e	liste*)
//=======================================================
let rec fold_right f a lst =
    match lst with
    [] -> a
    | x :: xs -> f x (fold_right f a xs)
//val fold_right: f:(‘a -> ‘b -> ‘b) -> a:‘b -> lst:‘a list -> ‘b 

let sumlist = fold_right (+) 0
//val sumlist : int list -> int

let concat: (int list list -> int list)= fold_right (@) []
//val concat : 'a list list -> 'a list 

let And = fold_right (&&) true
//val And : bool list -> bool

let Or = fold_right (||) false
//val Or : bool list -> bool 

let length:(int list -> int) = 
    let oneplus x n = 
        n+1 in fold_right oneplus 0 
//val length : int list -> int