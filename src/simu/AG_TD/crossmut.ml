open Types

let chooseCoupleForCross n =
  let r = ref 0 and s = ref 0
  in
    r := (Random.int n);
    s := (Random.int n);
    while (!s)=(!r) do s := (Random.int n) done;
  (!r,!s)

let cross v1 v2 first nb = 
  for k =0 to nb-1 do let n=(Array.length v1) in
    let (i,j) = (chooseCoupleForCross n) in 
    let (a,b)= (Local.cross v1.(i).data v1.(j).data) in
    v2.(first+2*k) <- {r_fit=Local.evalData a;s_fit=ref 0.0;
			data=a;clus=ref None}; 
    v2.(first+2*k+1) <- {r_fit=Local.evalData b;s_fit=ref 0.0;
			  data=b;clus=ref None}
  done

let  mutate v1 v2 first nb = let n=(Array.length v1) in
  for k=0 to nb-1 do let i =(Random.int n) in 
    let a =Local.mutate v1.(i).data in
    v2.(first+k) <- { r_fit=Local.evalData a; s_fit=ref 0.0;
		      data=a; clus=ref None}
  done
  
(**** 
let makelistint n = let l = ref [] in
  for i= 0 to n-1 do l:= i::(!l) done; !l

let rec suppressLE e = function
  [] -> []|
  x::l -> if x=e then (suppressLE e l) else x::(suppressLE e l)

let rec suppressLL l1  = function
  [] -> l1|
  x::l -> if (List.mem x l1) then (suppressLL l (suppressLE x l1))
                             else (suppressLL l l1)


let makelist n protected nb = 
  let size = (List.length protected) in
  if (size>=nb) 
    then (Misc.getnfirst protected nb)
    else
    let l= ref (makelistint n) and rlist=ref protected in
    l:= (suppressLL !l protected);
    for i=1 to nb-size do
      let j = (Random.int (List.length !l)) in
      let e = (List.nth !l j) in
      rlist:= e::(!rlist);
      l:= (suppressLE e !l)
    done;
    !rlist
****)


let makelist prot nb nbelems = 
  let size = (List.length prot) in
  if (size>=nb) 
    then (Misc.getnfirst prot nb)
    else
    let rlist=ref prot and  generated= ref 0 in
      while (!generated<nb-size) do
      let j = (Random.int nbelems) in
      if not  (List.mem j !rlist) then 
             (rlist:= j::(!rlist);
              generated:= !generated+1)
    done;
    !rlist


let  fromOldPop v1 v2 first nb prot evolutive = 
  let l = ref (makelist prot nb (Array.length v1)) in
  for k=0 to nb-1 do let i = (List.hd !l) in
    l:= List.tl !l;
    if not evolutive 
      then 
       v2.(first+k)<-{r_fit=v1.(i).r_fit;s_fit=ref 0.0;
                      data=v1.(i).data;clus=ref None} 
      else 
       v2.(first+k)<-{r_fit=Local.evalData v1.(i).data;s_fit=ref 0.0;
                      data=v1.(i).data;clus=ref None} 
  done

let crossmut v1 v2 prot evolutive pcross pmut  = 
  let n = Array.length v1  in 
  let n1=(truncate (float(n)*.pcross))/2 
  and n2=truncate (float(n)*.pmut) in
  (cross v1 v2 0 n1);
  (mutate v1 v2 (2*n1) n2);
  (fromOldPop v1 v2 (2*n1+n2) (n-2*n1-n2) prot evolutive)

