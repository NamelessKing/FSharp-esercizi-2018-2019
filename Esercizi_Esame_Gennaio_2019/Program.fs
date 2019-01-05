// Learn more about F# at http://fsharp.org

open System
open Capitolo6_Funzioni_di_ordine_superiore

let lookForValue value maxValue =
  let mutable continueLooping = true
  let randomNumberGenerator = new Random()
  while continueLooping do
    // Generate a random number between 1 and maxValue.
    for i = 0 to 10 do
        printf "="
    Console.Clear()
       

lookForValue 10 20
