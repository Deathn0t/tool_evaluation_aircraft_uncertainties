open Types

(* Genere un chromosome valide en appelant la fonction de generation *)
(* de donnee puis en evaluant la fitness de l'element *)
let generateChrom () = 
  let loc=Local.genData() in 
  {r_fit=(Local.evalData loc);s_fit=ref 0.0;data=loc;clus=ref None}

(* Prend en parametre le nombre n d'elements a construire et retourne *)
(* une liste contenant n chromosomes valides *)
let rec initPop= function
  0->[] |
  n->generateChrom()::(initPop (n-1))


