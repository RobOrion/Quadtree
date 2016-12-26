(*---------------------------------------------------------------------------------*)
(*------------------------CHATELET Robin/ KANDEL Hugo------------------------------*)
(*---------------------------------------------------------------------------------*)

(*------------------------Projet Quadtree------------------------------*)
(*------------------------Application du quadtree sur une image ppm------------------------------*)

(*------------------------lecture de l'image et traduction en terme de listes------------------------------*)

#load "str.cma" ;;

let rec lire = function(fichier) ->
          let i = input_line(fichier) in
                    i::lire(fichier)
                    with End_of_file -> [];;

let ligne l = Str.split (Str.regexp " ") l;;

let path = open_in "nom_du_fichier";;

seek_in path 0;;

let f = lire path;;

let b = List.map ligne f;;

let imgIn = open_in "p3.ppm";;
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

type Pixel={r : int ; g : int ; b : int};;

type quadtree=Pixel of triplet|Noeud of triplet * quadtree * quadtree * quadtree * quadtree;;

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

let rec timgB nb l = match nb with 
			0 -> ""
			|_ -> let a = retourneLigneBinaire imgIn in (listeFinaleBinaire a l)^"\n"^(timgB (nb-1) l);;

let bin img = 
	let p = param img in 
	let h = haut p in 
	let l = larg p in output_string imgOut ((ecrParam h l)^(timgB h l));;

(*------------------------construction de l'arbre------------------------------*)

let getRacine a = match a with
              Nul -> raise Arbre_vide
              |Noeud (v,_,_,_,_) -> v;;

let rec longueurListe l =
        if tl(l) = [ ] then 1
        else 1 + longueurListe(List.tl(l));;

let rec inserer a x = match a with
          Nul -> Noeud (Nul,x,Nul)
          | Noeud (fg,r,fd) ->
          if x=r then a else
          if x>r then Noeud (fg,r,inserer fd x)
          else Noeud (inserer fg x,r,fd);;
 
let moithaut l =
          let rec traitement m l =
                            if (m >= (longueurListe l )) then []
                            else ( (List.hd l)::(traitement (m+1) (List.tl l)) ) in traitement 0 l ;;

let moitbas l =
          let rec traitement m l =
                            if (m >= (longueurListe l )) then l
                            else (traitement (m+1) (List.tl l))  in traitement 0 l ;;

let hautgauche l =
          let rec quart1 l m = match l with
                          [] -> []
                          |_ -> if (longueurListe (l)) mod 2 = 0 then (moitbas (List.hd(l)))::quart1(List.tl(l)) (longueurListes(l) div 2) in (quart1(moithaut l) 0)
                                else (moitbas (List.hd(l)))::quart1(List.tl(l)) ((longueurListe (l) div 2)+1) in (quart1(moithaut l) 0);;
(*à modifier*)
let hautdroit l =
          let rec quart2 l m = match l with
                          [] -> []
                          |_ ->  (moitbas (List.hd(l)))::quart1(List.tl(l)) (m+1) in quart1(moithaut l) 0;;

let basgauche l =
          let rec quart3 l m = match l with
                          [] -> if (longueurListe(l)) mod 2 = 0 then (moithaut (List.hd(l)))::quart1(List.tl(l)) (longueurListe(l) div 2) in (quart1(moitbas l) 0)
                                else (moithaut (List.hd(l)))::quart1(List.tl(l)) ((longueurListe(l) div 2)+1) in (quart1(moitbas l) 0);;
(*à modifier*)
let basdroit l =
          let rec quart4 l m = match l with
                          [] -> []
                          |_ -> (moithaut (List.hd(l)))::quart4(List.tl(l)) (m+1) in quart4 (moitbas l) 0;;

let liste1 = hautgauche l;;
let liste2 = hautdroit l;;
let liste3 = basgauche l;;
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

let rec creationArbre l f = match l with
          []-> Nul
          |a::[] -> creerArbre(moyenneListePixel(moyenneFinaleListeBin(l))) Nul Nul Nul Nul
          |a::r -> match (egalite l) with
                  true -> creerArbre (moyenneListePixel (finalmoyennelistepixelBinaire l)) Nul Nul Nul Nul
                  |false -> creerArbre (moyenneListePixel (finalmoyennelistepixelBinaire l)) (creationArbre (liste1 l (f)) (f/2)) (creationArbre (liste2 l (f)) (f/2)) (creationArbre (liste3 l (f)) (f/2)) (creationArbre (liste4 l (f)) (f/2));;

let arbreComplet = creationArbre (liste taille);;

let rec rotation90sensdirect quadtree = 
  match quadtree with
  | []    -> []    
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in match (x < m, y < m) with
      | (true,  true)  -> Noeud(n, x, k-y, v)
      | (true,  false) -> Noeud(n, k-x, y, v)
      | (false, true)  -> Noeud(n, x-k, y, v)
      | (false, false) -> Noeud(n, x, y-k, v)

let rec mirroirHautBas quadtree = 
  match quadtree with
  | []    ->  [] 
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in match y < m with
      | true  -> Noeud(n, x, k-y, v)
      | false -> Noeud(n, x, y-k, v)

let rec mirroirDroiteGauche quadtree =
  match quadtree with
  | []    ->  [] 
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in match x < m with
      | true  -> Noeud(n, k-x, y, v)
      | false -> Noeud(n, x-k, y, v)

let inverspixel pixel = {r=255 - pixel.r ; g= 255 - pixel.g ; b = 255 - pixel.b};;

(*à modifier*)
(* creates a quadtree size 'n' with values inserted from the list of 
   (x,y,v) tuples - 'x','y' coordinates and 'v' the value *)

let faire_arbre n l =
  let rec trouver n l quadtree =
    match l with
      | []    -> quadtree
      | x::r -> ( match x with
          | (x1,x2,x3) -> trouver n r (insert quadtree (x1,x2) x3) )
  in trouver n l (quadtree (n, n/2, n/2, n/2, n/2))
