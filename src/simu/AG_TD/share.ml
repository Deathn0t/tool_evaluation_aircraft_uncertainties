open Types


(*** Do we use complex dmax-dmin computation (see dmean and minmaxfactor) *)
(*** See "complex_sharing" variable in Gvars module *)

(*** Local variables for dmin and dmax in clustering *)
(* dmax = dmean / minmaxfactor *)
let minmaxfactor = ref 1.0
let dmean = ref 0.5

(*** Find the best element of a population *)
(*******************************************)
let find_best pop =
 let maxf= ref (-1.0/.0.0) and best = ref 0 in 
  for i = 0 to (Array.length pop)-1 do 
    if (pop.(i).r_fit> !maxf) then (maxf:=pop.(i).r_fit;best:=i)
  done; 
  !best

(*** Compute distance and returns a couple (used by fold_left) *)
(***************************************************************)
let func x1 (x2,d) x3 =
  let newd= Local.dataDistance x1 !(x3.center) in
    if newd<d 
      then (x3,newd)
      else  (x2,d)

(*** Merge two clusters when too close to each other *)
(*****************************************************)
let merge pop x cl =
  let center=Local.dataBarycenter !(x.center) !(x.nb_elems) 
                                  !(cl.center) !(cl.nb_elems) and
      nb_elems= !(cl.nb_elems)+ !(x.nb_elems) in
  cl.center:=center;cl.nb_elems:=nb_elems;
  for i=0 to (Array.length pop) -1 do 
     if !(pop.(i).clus)= (Some x) then pop.(i).clus:= (Some cl)
  done;
  cl

(*** Try to merge clusters *)
(***************************)
let havetomergecluster pop clust clist dmax =
  let toto = ref true in
  let rec mergeclusters cl  = function
    []   -> toto:=false;[cl]|
    x::l -> if x=cl 
            then (mergeclusters cl l)
            else 
             let d=Local.dataDistance !(x.center) !(cl.center)
             and dmin = dmax/.3.0 in
	     if d>=dmin then x::(mergeclusters cl l)
                        else (merge pop x cl)::l
    in
    while (!toto) do clist:=(mergeclusters clust !clist) done; !clist
                   

(*** Make clustering for sharing *)
(*********************************)
let do_clusters pop = 
  let clist=ref [{center=ref pop.(0).data;nb_elems=ref 1}]  in
  pop.(0).clus:= Some (List.hd !clist);
  for i=1 to (Array.length pop)-1 do 
    let first= (List.hd !clist) in
    let inf=(Local.dataDistance pop.(i).data !(first.center)) in
    let (cl,d)=(List.fold_left (func pop.(i).data) 
      	                        (first,inf) !clist)
    and dmax = !dmean /. !minmaxfactor in
    if d>dmax 
     then (let newclus= {center=ref (pop.(i).data);nb_elems= ref 1} in
           clist:= newclus:: !clist;pop.(i).clus:=  Some newclus)
     else
          (let newcenter= Local.dataBarycenter !(cl.center) !(cl.nb_elems)
                                               pop.(i).data 1 in
           cl.center := newcenter;cl.nb_elems := !(cl.nb_elems)+1;
           pop.(i).clus:=Some cl;clist:= (havetomergecluster pop cl clist dmax) 
           )
  done; !clist;;

(* Attention, il y a un probleme avec la notion d'elitisme et de sharing.
Qu'est ce que ca veut dire etre a 90% du meilleur element? Ca n'a pas
de sens sur [-Inf;Inf]. En utilisant la s_fitness, on provoque une forte 
instabilite liee a la distribution des valeurs des elements de population.
Ici, je suppose que l'on travaille avec une r_fit sur [0;Inf] A refaire.
*)
let rec get_best sharing bestgen pop = 
  function
      [] -> []
    | x::l -> let maxf= ref (-1.0/.0.0) and best = ref (-1) in
      for i=0 to (Array.length pop) -1 do
	if (Some x) = !(pop.(i).clus) && !maxf< pop.(i).r_fit
	then (maxf:= pop.(i).r_fit;best:=  i)
      done;
      if !maxf>sharing*.pop.(bestgen).r_fit then
	!best::(get_best sharing bestgen pop l)
      else 
	(get_best sharing bestgen pop l);;

(*** Compute mean distance to compute dmin and dmax *)
(****************************************************)
let computeDMean pop clus =
  let d = ref 0.0 and lpop = (Array.length pop) in
  let rec add_distance = function
    ([],_) -> 0.0|
    (x::l,data) -> 
      (Local.dataDistance data !(x.center) ) 
	+. add_distance(l,data)

  in
    for i=0 to (lpop-1) do
      d:=!d +. (add_distance (clus,pop.(i).data))
    done;
  (!d /. (float)(lpop) /. (float)(List.length clus) )


(*** Make sharing *)
(******************)
let share elitist sharing complex_sharing pop =
  let bestgen = (find_best pop) in
  if sharing=0.0 
  then if elitist then [bestgen] else [] (* returns the best element from population *)
  else (
    if (complex_sharing<>0.0)
    then ( dmean:= complex_sharing; minmaxfactor:= 1.0 );
    let clus=(do_clusters pop) in
           (* Get the best elements' list from clusters *)
    let bestlist=(get_best sharing bestgen pop clus) in
    (*
    Printf.fprintf !Gvars.fileout "nbclust_opt: %d\n" 
      (List.length bestlist); 
    Printf.fprintf !Gvars.fileout "nbclust: %d\n" 
      (List.length clus); 
    *)
    if (complex_sharing=0.0)
    then (
                  (* If we use complex sharing computation, then recompute *)
                  (* dmean and minmaxfactor variables *)
      let nopt = List.length bestlist and
	  nclust = List.length clus in
      let max = (float)nopt /. (float)nclust in
      dmean := computeDMean pop clus;
      if max>0.85 && !minmaxfactor<100.0 
      then minmaxfactor:= (!minmaxfactor)*.(1.05);
      if max<0.75 && !minmaxfactor>1.0 
      then minmaxfactor:= (!minmaxfactor)*.(0.95);
      (*
      (Printf.fprintf !Gvars.fileout
	 "dmax=%f minmaxfactor=%f\n" (!dmean/. !minmaxfactor)
	 !minmaxfactor)
      *)
    );
    (* The sharing function *)
    for i=0 to (Array.length pop)-1 do
      match !(pop.(i).clus) with
	Some a ->
	  pop.(i).s_fit:= !(pop.(i).s_fit)/.(float !(a.nb_elems))
      | None -> ()
    done;
    if elitist then 
	 bestlist
    else 
      []
      )
      
