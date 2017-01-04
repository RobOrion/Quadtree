(*------------------------ Enregistrement ------------------------------*)

let supprime_element i l = if i < 0 || i>= List.length l then failwith
                           else 
                              let rec delete i l = match l with 
                                          [] -> []
                                          |h::t when i = 0 -> t
                                          |h::t -> h::delete(i-1) t
                                in delete i l;;
                                
let rec fusion m1 m2 n = match m2 with
[]-> []
|_ -> 


