module Synthesis

open System.Diagnostics.Tracing

let abelar a = 
    (a > 12 && a < 3097 && a % 12 = 0)

let area abase height = 
    match height < 0.0 ||abase < 0.0 with
    |true -> failwith("invalid height or base")
    |false -> (0.5 * (abase) * (height))

let zollo number =
    match number < 0 with
    |true -> number * -1
    |false -> number * 2

let min value1 value2 =
    match value1 < value2 with
    |true -> value1
    |false -> value2

let max value1 value2 =
    match value1 > value2 with
    | true -> value1
    |false -> value2

let ofTime hours minutes seconds =
    hours*60*60 + minutes*60 + seconds

let toTime seconds =
    let second = (seconds%60)
    let minutes = (seconds-(second))/60
    let minute = (minutes%60)
    let hours = (minutes-minute)/60
    match seconds < 0 with
    | true -> (0,0,0)
    | false -> (hours, minute, second)
    
let digits x =
    let rec digit value:int=  
        match (10 > value && -10 < value)with
        | false -> 1 + digit(value/10)
        | true -> 1
    digit x

let minmax (w,x,y,z) =
    (min (min w x) (min y z), ( max (max w x) (max y z)))

    //failwith "Not implemented"
let isLeap value =
    match value < 1582 with
    | true -> failwith "Not implemented"
    | false -> ((value % 4 = 0) && ((value % 100) <> 0) || ((value % 400) = 0))
    

let month = function
    | 1 -> ("January", 31)
    | 2 -> ("February", 28)
    | 3 -> ("March", 31)
    | 4 -> ("April", 30)
    | 5 -> ("May", 31)
    | 6 -> ("June", 30)
    | 7 -> ("July", 31)
    | 8 -> ("August", 31)
    | 9 -> ("September", 30)
    | 10 -> ("October", 31)
    | 11 -> ("November", 30)
    | 12 -> ("December", 31)
    | _ -> failwith("Invalid")

let toBinary value =
    match value < 0 with
    | true -> failwith("Error")
    | false ->
        let rec binaryChanger value stringV =
            match value = 0 || value = 1 with
            | true -> string(value) + stringV
            | false -> (binaryChanger (value/2)  (string(value % 2) + stringV))
        binaryChanger value ""
        
   // 111001010011010111100100
   

let bizFuzz value =
    match value < 0 with
    | true -> (0,0,0)
    | false -> 
        let rec bizFuzzer value counter (x, y, z) = 
           match counter < value with
           | false -> (x,y,z)
           | true -> ()
            
        bizFuzzer value 0 (0,0,0)

    //failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"