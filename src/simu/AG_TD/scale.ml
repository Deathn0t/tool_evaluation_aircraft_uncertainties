open Types

let c = 2.0 

let computeMeanSigma pop = 
 let sum= ref 0.0 and sum2= ref 0.0 in 
  for i = 0 to (Array.length pop)-1 do 
    sum := !sum+. !(pop.(i).s_fit) ;
    sum2 := !sum2 +. !(pop.(i).s_fit) *. !(pop.(i).s_fit)
  done; 
  sum:= !sum /. (float (Array.length pop));
  sum2:= !sum2 /. (float (Array.length pop)) -. (!sum *. !sum);
  sum2:= sqrt !sum2;
  (!sum,!sum2)

let computeMaxfMinf pop = 
 let maxf= ref (-1.0/.0.0) 
  and minf=ref (1.0/.0.0) in 
  for i = 0 to (Array.length pop)-1 do 
    if (!(pop.(i).s_fit)> !maxf) then maxf:= !(pop.(i).s_fit);
    if (!(pop.(i).s_fit)< !minf) then minf:= !(pop.(i).s_fit)
  done; 
  (!maxf,!minf)


let normalize pop = 
  let (maxf,minf)=(computeMaxfMinf pop) in	
  for i=0 to (Array.length pop)-1 do
    pop.(i).s_fit := (!(pop.(i).s_fit)-.minf)/.(maxf-.minf) 
  done

let sigma_truncation pop = let 
 (moy,sigma)=(computeMeanSigma pop) and fit=ref 0.0 in
  for i=0 to (Array.length pop)-1 do 
    fit:= !(pop.(i).s_fit) -. (moy -. (c *. sigma));
    if (!fit<0.0) then fit:= 0.0;
    pop.(i).s_fit:= !fit
  done;
  (normalize pop)

let power_low pop =
  let s=0.1 and s0=0.1 and p1=0.05 and p2=0.1 and alpha=0.1 in
   let t1=p2*.((s0/.s)**alpha)
   and t2=((float)(!Gvars.numgen))/.
           ((float)(Gvars.nbgens+1))*.(3.1415927/.2.0) in
    let t3=(tan(t2))**t1 and t4=(s/.s0)**p1 in
     let k=t3*.t4 and fit=ref 0.0 in
      for i=0 to (Array.length pop)-1 do
        fit:= !(pop.(i).s_fit);
        if (!fit<0.0) then fit:= 0.0;
        pop.(i).s_fit:= (!fit)**k
      done;
      (normalize pop)

let scale pop scaling =
  for i=0 to (Array.length pop)-1 do 
    pop.(i).s_fit:=pop.(i).r_fit 
  done;
  (normalize pop);
  if scaling=1 then (sigma_truncation pop);
  if scaling=2 then (power_low pop)
(* si scaling=0 alors rien d'autre a faire *)

(* EOM *)
