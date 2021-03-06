(*---------------------------------------------------------------------------------*)
(*------------------------ CHATELET Robin/ KANDEL Hugo ------------------------------*)
(*---------------------------------------------------------------------------------*)

(*------------------------ Projet Quadtree ------------------------------*)
(*------------------------ Application du quadtree sur une image ppm ------------------------------*)

(*------------------------ lecture de l'image et traduction en terme de listes ------------------------------*)

(* #use "main.ml";; *) 

#load "str.cma" ;;

let rec lire = function(fichier) ->
          try let i = input_line(fichier) in
                    i::lire(fichier)
                    with End_of_file -> [];;

let ligne l = Str.split (Str.regexp " ") l;;

let path = open_in "zombie.ppm";;

seek_in path 0;;

let f = lire path;;

let l = List.map ligne f;;

let imgIn = open_in "zombie.ppm";;
let imgOut = open_out "imgOut.ppm";;

let rec convStrToInt l = match l with
		[] -> []
		|tete::suivant -> (int_of_string (tete) :: convStrToInt (suivant));;

let param img = convStrToInt(Str.split (Str.regexp " ") (input_line img));;

let larg p = List.hd p;;

let haut p = List.nth p 1;;

let ecrParam h l = ("P3\n"^string_of_int(l)^" "^string_of_int(h)^" "^"255"^" ");;

let carac a n = List.nth a n;;

(*------------------------définition des types à utiliser------------------------------*)

type triplet = {r : int; g : int; b : int};;

type quadtree = Pixel of int * triplet|Noeud of triplet * quadtree * quadtree * quadtree * quadtree;;

let creerArbre (v,f1,f2,f3,f4) = Noeud (v,f1,f2,f3,f4);;

exception Arbre_vide;;

(*------------------------traduction des données binaires------------------------------*)

let retourneBinaire = 
          let rec retourne l1 l2 = match l2 with 
			[] -> l1 
                        |a :: tl -> retourne (a :: l1) tl  in  retourne[];;

let retourneLigneBinaire a = retourneBinaire (convStrToInt(Str.split (Str.regexp " ") (input_line a)));;

let ecrireBinaire n = match n with
	 0 ->("0 "^"0 "^"0 ")
	|1 ->("255 "^"255 "^"255 ")
	|_ ->("");;

let traduireBinaire a n = ecrireBinaire(carac a n);;

let rec traduireLigneBinaire a x = match x with
		(-1) -> " "
		|n -> (traduireBinaire a n)^(traduireLigneBinaire a (x-1));;

let listeFinaleBinaire a l = traduireLigneBinaire (a (l-1));;

(*------------------------construction de l'arbre------------------------------*)

	     
	     (*problème sur le constructeur Noeud*)
let rec longueurListe l =
        if List.tl(l) = [] then 1
        else 1 + longueurListe(List.tl(l));;


let moithaut l =
          let rec traitement m l =
                            if (m >= (longueurListe l )) then []
                            else ( (List.hd l)::(traitement (m+1) (List.tl l)) ) in traitement 0 l ;;

let moitbas l =
          let rec traitement m l = if (m >= (longueurListe l )) then l
                            	   else (traitement (m+1) (List.tl l))  in traitement 0 l ;;

let rec hautgauche1 l n = match l with (*n=0 moitiée gauche liste*)
	[] -> []
	| _ -> (match (n)<((List.length l)/2) with
    		false -> []
    		| true -> (List.nth l n)::(hautgauche1 l (n+1)));;

let rec hautgauche2 l n = match l with (*n=0 moitiée gauche*)
	[] -> []
	| _ -> (match (n)<((List.length l)/2) with
    		false -> []
    	| true -> (hautgauche1 (List.nth l n) 0)::(hautgauche2 l (n+1)));;

let hautdroit l =
          let rec quart2 l m = match l with 
                          [] -> []
                          |_ -> (moitbas (List.hd(l)))::quart2(List.tl(l)) (m+1) in quart2(moithaut l) 0;;

let rec basgauche1 l n = match l with
	[] -> []
	| _ -> (match (n)<((List.length l)/2) with
    		false -> []
    		| true -> (List.nth l n)::(basgauche1 l (n+1)));;

let rec basgauche2 l n = match l with
	[] -> []
	| _ -> (match (n)<((List.length l)) with
    		false -> []
    	| true -> (basgauche1 (List.nth l n) 0)::(basgauche2 l (n+1)));;

let basdroit l =
          let rec quart4 l m = match l with
                          [] -> []
                          |_ -> (moithaut (List.hd(l)))::quart4(List.tl(l)) (m+1) in quart4 (moitbas l) 0;;

let liste1 = hautgauche2 l;;
let liste2 = hautdroit l;;
let liste3 = basgauche2 l;;
let liste4 = basdroit l;;

let rec inverser l = match l with
          []-> []
          |a::[] -> [1::a]
          |a::r -> [1::a]@inverser r ;;

let rec sommeListePixelBinaire l = match l with
          []-> 0
          |([i])::([])-> l
          |([i])::([j])::r -> (i + j + sommeListePixelBinaire r);;

let moyenneFinaleListeBin l = (sommeListePixelBinaire(l))/ (longueurListe l);;

let moyenneListePixel a = [a];;

let egalite l = List.for_all (fun x -> if (x = (List.hd l)) then true else false) l;;

let rec creationArbre l f = match l with
          []-> Nul
          |a::[] -> creerArbre(moyenneListePixel(moyenneFinaleListeBin(l))) Nul Nul Nul Nul
          |a::r -> match (egalite l) with
                  true -> creerArbre (moyenneListePixel (finalmoyennelistepixelBinaire l)) Nul Nul Nul Nul
                  |false -> creerArbre (moyenneListePixel (finalmoyennelistepixelBinaire l)) (creationArbre (liste1 l (f)) (f/2)) (creationArbre (liste2 l (f)) (f/2)) (creationArbre (liste3 l (f)) (f/2)) (creationArbre (liste4 l (f)) (f/2));;

let arbreComplet = creationArbre (liste taille);;

(*------------------------opérations sur l'arbre------------------------------*)

let rec rotation90sensdirect arbre = match arbre with
  	| []    -> []    
  	| Noeud(n,x,y,v)  -> 
     			 let m = n/2 and k = n-1 in match (x < m, y < m) with
     					 | (true,  true)  -> Noeud(n, x, k-y, v)
      					 | (true,  false) -> Noeud(n, k-x, y, v)
      					 | (false, true)  -> Noeud(n, x-k, y, v)
      					 | (false, false) -> Noeud(n, x, y-k, v)

let rec mirroirHautBas arbre = match arbre with
  	| []    ->  [] 
  	| Noeud(n,x,y,v)  -> 
      			let m = n/2 and k = n-1 in match y < m with
      					| true  -> Noeud(n, x, k-y, v)
      					| false -> Noeud(n, x, y-k, v)

let rec mirroirDroiteGauche arbre = match arbre with
  	| []    ->  [] 
  	| Noeud(n,x,y,v)  -> 
      			let m = n/2 and k = n-1 in match x < m with
      					| true  -> Noeud(n, k-x, y, v)
      					| false -> Noeud(n, x-k, y, v)

let inverspixel pixel = {r=255 - pixel.r ; g= 255 - pixel.g ; b = 255 - pixel.b};;

(*------------------------compression------------------------------*)
let rec hauteur arbre = match arbre with 
        |Pixel(taille,u,v,w) -> 1
        |Noeud(r,f1,f2,f3,f4) -> 1+ max(hauteur(f1) hauteur(f2) hauteur(f3) hauteur(f4));;

let moyenneNoeud fils1 fils2 fils3 fils4 = (fils1+fils2+fils3+fils4)/4;;

let compression arbre = let rec ssCompression arbre taille index = match arbre with
                                                            |Pixel(taille,u,v,w)->(match (hauteur arbre-1=index) with 
        |true-> Pixel(taille/4,u,v,w)
        |false-> arbre)
                                                            |Noeud(r,f1,f2,f3,f4)->(match (hauteurarbre-1=index) with 
        |true-> Noeud(moyenneNoeud (f1,f2,f3,f4),f1,f2,f3,f4)
        |false-> arbre)
        
        
        in sCompression arbre (hauteur) 1;;

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
		
let rec convListe l = match l with
		[] -> ""
		|_ -> PixeltoString(List.hd l)^(convListe (supprime_element 0 l));;


let rec convMat m = match m with
		[] -> ""
		|_ -> convListe(List.hd m)^"\n"^(convMat (supprime_element 0 m));;

let ecrMat m = let h = List.length m in let l = List.length(List.hd m) in output_string imgOut ((ecrParam h l)^(convMat m));;

