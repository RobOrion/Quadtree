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
              |_ -> ((List.nth m1 n)@(List.nth m2 0))::(fusion m1 (supprime 0 m2) (n+1));;
              
let creerMat a = [[a]];;

let rec transfoArbre a = match a with
              [] -> []
              |Pixel(_,v) -> creerMat(v)
              |Noeud(v,f1,f2,f3,f4) -> fusion ((transfoArbre f1)@(transfoArbre f3)) ((transfoArbre f2)(transfoArbre f4)) 0;;
              
let PixeltoString pixel = (string_of_int pixel.r)^" "^(string_of_int pixel.g)^" "^(string_of_int pixel.b)^" ";;

let rec StringToInt l = match l with
		[] -> []
		|tete::suivant -> (int_of_string (tete) :: convStrToInt (suivant));;


