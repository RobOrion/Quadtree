(*------------------------compression------------------------------*)
let rec hauteur arbre = match arbre with 
        |Pixel(taille,u,v,w) -> 1
        |Noeud(r,f1,f2,f3,f4) -> 1+ max(hauteur(f1) hauteur(f2) hauteur(f3) hauteur(f4));;

let moyenneNoeud fils1 fils2 fils3 fils4 = (fils1+fils2+fils3+fils4)/4;;

let compression arbre = let rec ssCompression arbre taille index = match arbre with
                                                            |Pixel(taille,u,v,w)->(match hauteurarbre-1=index) with 
        |true-> Pixel(taille/4,u,v,w)
        |false-> arbre 
                                                            |Noeud(r,f1,f2,f3,f4)->(match hauteurarbre-1=index) with 
        |true-> Noeud(moyenneNoeud (f1,f2,f3,f4),f1,f2,f3,f4)
        |false-> arbre
        
        
        in sCompression arbre (hauteur) 1;;

        

