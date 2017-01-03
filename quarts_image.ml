
(* Par quentin et tyty *)

(* image de type     12
                     43*)
                     
(*Q1_____________*)
let rec minidiviq1 l n = match l with (*n=0 moitiée gauche liste*)
[] -> []
| _ -> (match (n)<((List.length l)/2) with
    false -> []
    | true -> (List.nth l n)::(minidiviq1 l (n+1)));;

let rec diviq1 l n = match l with (*n=0 moitiée gauche*)
[] -> []
| _ -> (match (n)<((List.length l)/2) with
    false -> []
    | true -> (minidiviq1 (List.nth l n) 0)::(diviq1 l (n+1)));;

let dim = 4;;

(*Q2_____________*)
let rec minidiviq2 l n = match l with (*moitiée droite d'une liste*)
[] -> []
| _ -> (match (n)<((List.length l)) with
    false -> []
    | true -> (List.nth l n)::(minidiviq2 l (n+1)));;

let rec diviq2 l n = match l with (*n=0 moitiée droite de la sous liste*)
[] -> []
| _ -> (match (n)<((List.length l)/2) with
    false -> []
    | true -> (minidiviq2 (List.nth l n) (dim/2))::(diviq2 l (n+1)));;

(*Q3_____________*)
let rec minidiviq3 l n = match l with (*moitiée droite d'une liste*)
[] -> []
| _ -> (match (n)<((List.length l)) with
    false -> []
    | true -> (List.nth l n)::(minidiviq3 l (n+1)));;


let rec diviq3 l n = match l with (*n=dim/2 moitiée droite de la sous liste*)
[] -> []
| _ -> (match (n)<((List.length l)) with
    false -> []
    | true -> (minidiviq3 (List.nth l n) (dim/2))::(diviq3 l (n+1)));;

(*Q4_____________*)
let rec minidiviq4 l n = match l with (*moitiée gauche de la grande liste*)
[] -> []
| _ -> (match (n)<((List.length l)/2) with
    false -> []
    | true -> (List.nth l n)::(minidiviq4 l (n+1)));;

let rec diviq4 l n = match l with (*n=dim/2*) (*moitiée gauche de la sous liste*)
[] -> []
| _ -> (match (n)<((List.length l)) with
    false -> []
    | true -> (minidiviq4 (List.nth l n) 0)::(diviq4 l (n+1)));;
