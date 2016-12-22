
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

type quadtree=Pixel of triplet|Noeud of triplet * quadtree * quadtree * quadtree * quadtree;;

(*création d'un noeud*)

let creerArbre (v,f1,f2,f3,f4) = Noeud (v,f1,f2,f3,f4);;

exception Arbre_vide;;

let getRacine a = match a with
              Nul -> raise Arbre_vide
              |Noeud (v,_,_,_,_) -> v;;

let TailleImage = open_in "image.ppm";;

let rec longueurListe(l) =
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
                            if (m >= (List.length l )) then []
                            else ( (List.hd l)::(traitement (m+1) (List.tl l)) ) in traitement 0 l ;;

let moitbas l =
          let rec traitement m l =
                            if (m >= (List.length l )) then l
                            else (traitement (m+1) (List.tl l))  in traitement 0 l ;;

let hautgauche l =
          let rec quart1 l m = match l with
                          [] -> []
                          |_ -> if List.length mod 2 = 0 then (moitbas (List.hd(l)))::quart1(List.tl(l)) (List.length div 2) in (quart1(moithaut l) 0)
                                else (moitbas (List.hd(l)))::quart1(List.tl(l)) ((List.length div 2)+1) in (quart1(moithaut l) 0);;
(*à modifier*)
let hautdroit l =
          let rec quart2 l m = match l with
                          [] -> []
                          |_ ->  (moitbas (List.hd(l)))::quart1(List.tl(l)) (m+1) in quart1(moithaut l) 0;;

let basgauche l =
          let rec quart3 l m = match l with
                          [] -> if List.length mod 2 = 0 then (moithaut (List.hd(l)))::quart1(List.tl(l)) (List.length div 2) in (quart1(moitbas l) 0)
                                else (moithaut (List.hd(l)))::quart1(List.tl(l)) ((List.length div 2)+1) in (quart1(moitbas l) 0);;
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
  | Pixel(n,nw,ne,sw,se) -> Pixel(n,(rotate ne),(rotate se),(rotate nw),(rotate sw))
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in
      match (x < m, y < m) with
      | (true,  true)  -> Noeud(n, x, k-y, v)
      | (true,  false) -> Noeud(n, k-x, y, v)
      | (false, true)  -> Noeud(n, x-k, y, v)
      | (false, false) -> Noeud(n, x, y-k, v)

let rec mirroirHautBas quadtree = 
  match quadtree with
  | []    ->  [] 
  | Pixel(n,nw,ne,sw,se) -> Pixel(n, (mirroirHautBas sw), (mirroirHautBas se), (mirroirHautBas nw), (mirroirHautBas ne))
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in 
      match y < m with
      | true  -> Noeud(n, x, k-y, v)
      | false -> Noeud(n, x, y-k, v)

let rec mirroirDroiteGauche quadtree =
  match quadtree with
  | []    ->  [] 
  | Pixel(n,nw,ne,sw,se) -> Pixel(n, (mirroirDroiteGauche ne), (mirroirDroiteGauche nw), (mirroirDroiteGauche se), (mirroirDroiteGauche sw))
  | Noeud(n,x,y,v)  -> 
      let m = n/2 and k = n-1 in 
      match x < m with
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
