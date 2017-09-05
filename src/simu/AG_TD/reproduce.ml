open Types


(* Modifie la s_fit pour faire une selection de type ranking *)
let recomputeFit v =
  let n = Array.length v
  and l = ref [] in
  for i=0 to n-1 do l:= i::(!l) done;
  l:= Sort.list (fun i j -> !(v.(i).s_fit) > !(v.(j).s_fit)) !l;
  let compt=ref 0 in
  List.iter (fun i-> v.(i).s_fit := (float (n- !compt)); incr compt) !l;
  ();;



(* Calcule la somme des fitness de la population *)
let computeSumFit v = let sum= ref 0.0 in
  for i = 0 to (Array.length v)-1 do sum := !sum+. !(v.(i).s_fit) done; !sum

(* Calcule a partir du tableau des fitness (v) le tableau des fitness *)
(* divise par la somme des fitness (v2) *)
let computeNFit v v2=  let sum=computeSumFit v in
  for i = 0 to (Array.length v)-1 do v2.(i) <- !(v.(i).s_fit)/.sum done


let computeTab v v1 v2 p= let n =(Array.length v) in
  for i=0 to n-1 do
    let nbf = v.(i) *.float(n) in let nbi=(truncate nbf) in
    v1.(i) <- nbi;v2.(i) <- nbf-.(float nbi); p:=!p+nbi done

let computeTab2 v s v2= let sum=ref 0.0 in
  for i=0 to (Array.length v)-1 do
  v2.(i) <- (v.(i)+. !sum)/.s; sum := !sum+. v.(i) done


let computeTab4 v1 v2 rs n = let i= ref 0 in
  for j = 0 to n-1  do
  while rs.(j)> v2.(!i) do i:= !i+1 done;
  v1.(!i) <- v1.(!i)+1 done

let rec list_rfloat = function
 0 -> [] |
 n -> (Random.float 1.0)::(list_rfloat (n-1))

let computeTab3 v1 v2 rem=
  computeTab4 v1 v2 (Array.of_list (Sort.list (<) (list_rfloat rem))) rem


let makeTab v1 v2 prot =
  let k = ref 0 and newprot = ref [] and l=ref [] in
  for i=0 to (Array.length v1)-1 do
    if (List.mem i prot) then
      newprot:= !k::(!newprot);
    for j=1 to v2.(i) do
      l:= v1.(i)::(!l);
      k:=!k+1
    done
  done;
  let pop2=Array.of_list (List.rev !l) in
  newprot:=(Sort.list (fun i j -> pop2.(i).r_fit>pop2.(j).r_fit)
  !newprot);
  (!newprot,pop2);;

exception Cant_Protect

let get_largest v1 prot =
  let best= ref (-1) and largest= ref 0 in
  for i=0 to (Array.length v1) -1 do
    if (v1.(i)> !largest) && ((v1.(i)>1) || not (List.mem i prot))
      then (largest:=v1.(i);best:=i)
  done;
  if !best= -1 then raise Cant_Protect;
  !best

let verifytab v1 prot =
  for i=0 to (Array.length v1) -1 do
    if v1.(i)=0 && (List.mem i prot) then
       let j = (get_largest v1 prot) in
       v1.(i)<- v1.(i)+1;v1.(j)<-v1.(j)-1
   done

(* Cette fonction construit une nouvelle population en utilisant la *)
(* technique du stochastic remainder  *)
let reproduce pop1 prot = let n = Array.length(pop1) in
  let v=Array.make n 0.0 and v1=Array.make n 0 and 
    v2=Array.make n 0.0 and p =ref 0 and v3=Array.make n 0.0 in

(* Ranking selection *)
  recomputeFit pop1;

(* On calcule le tableau v contenant les fitness divisees chacune par *)
(* la somme des fitness de facon a ce que la somme des elements de v *)
(* fasse 1.0 *)
  (computeNFit pop1 v);

(* On calcule : *)
(*  - v1 qui contient le nombre de representants "certains" pour chaque *)
(*    element de population; *)
(*  - v2 qui contient le remainder pour chaque element de population *)
(*  - p qui est la somme des elements de v1 *)
  (computeTab v v1 v2 p);

(* On calcule : *)
(*  - v3 qui est le tableau des remainders cumulees divise par la somme *)
(*    des remainders. Le dernier element de ce tableau vaut 1 par *)
(*    construction. Si l'on tire un nombre aleatoire r entre 0 et 1, il *)
(*    suffit de chercher l'indice de v3 tel v3(i)<r<v3(i+1) pour *)
(*    savoir qu'il faut augmenter d'une unite le nombre de *)
(*    representants de l'element i dans la nouvelle population *)
  (computeTab2 v2 (float (n- !p)) v3);

(* On met a jour v1 en fonction des informations de v3, comme on vient *)
(* de l'expliquer *)
  (computeTab3 v1 v3 (n- !p));

(* Maintenant on verifie que tous les elements proteges sont biens *)
(* dans le tableau. S'ils n'y sont pas on les rajoute  *)
  (verifytab v1 prot);

(* On construit la nouvelle population a partir de l'ancienne en *)
(* fonction du nombre de representants de chaque element donne par v1 *)
(* et on tire le resultat de facon a ce que lors du croisement, ce *)
(* soit les meilleurs elements qui soient conserves en priorite *)
  (makeTab pop1 v1 prot);;
