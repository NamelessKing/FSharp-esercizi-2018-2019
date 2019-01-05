module Capitolo7_Tipi_derivati

(*Tipi Enumerati*)
type seme = Bastoni | Coppe | Denari | Spade

type valore = Asso | Due | Tre | Quattro | Cinque | Sei | Sette | Fante | Cavallo | Re

let settebello = (Sette, Denari)

let briscola =  [(Asso, Denari); (Due, Spade); (Tre, Bastoni)]

let value x = 
    match x with
    |Asso -> 1
    | Due -> 2
    | Tre -> 3
    | Quattro -> 4
    | Cinque -> 5
    | Sei -> 6
    | Sette -> 7
    | Fante -> 8
    | Cavallo -> 9
    | Re -> 10

(*I Tipi Unione*)
type identifier = 
    | Name of name:string
    | Code of code:int
//====================================
type temp = 
    | Celsius of float
    | Farhenheit of float  
    | Kelvin of float

let convCels x = 
    match x with
    | Celsius n -> Celsius n
    | Farhenheit n -> Celsius ((5.0 / 9.0) * (n - 32.0))
    | Kelvin n -> Celsius (n + 273.0)

let convTempToFloat x = 
    match x with
    | Celsius n -> n
    | Farhenheit n -> n
    | Kelvin n -> n
//====================================

type direzione = Su | Giu | Destra | Sinistra
type posizione = Pos of int * int * direzione
type muovimento = Movimento of (int * direzione)
type player = Player of posizione
type path = Path of muovimento list


let xcoord (Pos(x,_,_)) = x
let ycoord (Pos(_,y,_)) = y
let dir (Pos(_,_,d)) = d
let punto (Pos(x,y,_)) = (x,y)

let azionaMovimento (Player player)(Movimento movimento) =
    match movimento with 
    |(y,Su)         -> let newY = ycoord player + y in Pos(xcoord player,newY,Su)
    |(y,Giu)        -> let newY = ycoord player - y in Pos(xcoord player,newY,Giu)
    |(x,Destra)     -> let newX = xcoord player + x in Pos(newX,ycoord player,Destra)
    |(x,Sinistra)   -> let newX = xcoord player - x in Pos(newX,ycoord player,Sinistra)


let rec percorriPercorso (Path listaMovimento) (player:player) =
    match listaMovimento with 
    |[] -> []
    |x::xs-> 
            let newPos = (azionaMovimento player x)
            in newPos ::(percorriPercorso (Path xs) (Player newPos ))