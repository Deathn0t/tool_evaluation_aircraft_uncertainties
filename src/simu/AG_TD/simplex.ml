(* calcul du barycentre de points.(0) ... points.(n-1) *)
let barycentre n points =
  let b = ref (fst points.(0))
  in for i = 1 to n-1 do
    b := Local.dataBarycenter !b i (fst points.(i)) 1
  done;
  !b

(* initialisation de l'ensemble des points du simplex *)
(*let init p alpha =
  let n = Local.dim in
  let v = Array.create (n+1) Local.foodata
  in v.(0) <- p;
     for i = 1 to n do
       v.(i) <- Local.calcNext p (i-1) alpha
     done;
     v;;
*)
let init p alpha =
  let n = Local.dim in
  let l = ref [p]
  in 
     for i = 1 to n do
       l := (Local.calcNext p (i-1) alpha)::(!l)
     done;
     (Array.of_list !l);;


let print_point p fp s =
  Printf.fprintf !Gvars.fileout "%s" s;
  Local.print_data p; 
  Printf.fprintf !Gvars.fileout " eval: %f\n" fp


let shift v0 n vs = 
  for i = n downto 1 do
    vs.(i) <- vs.(i-1)
  done;
  vs.(0) <- v0

(* minimisation de 'f' en partant de 'p' en 'iterations' etapes *)
let simplex_4 f p iterations alpha precision =
  let l = init p alpha and n = Local.dim in
  let vs = (Array.map (fun x -> (x, f x)) l) in 
  Misc.array_sort (fun (_,x) (_,y) -> x > y) vs;
  let num_iter = ref 1 in
  while (!num_iter<> iterations) && 
        ((Local.dataDistance (fst vs.(0)) (fst vs.(n)))>precision) do
         incr num_iter;
       let vb = barycentre n vs in
         let vr = Local.calcNew (fst vs.(n)) vb (-1.0) (* le point reflechi *)
          in let fvr = f vr in 
            if fvr > snd vs.(0) then  (* le point étendu *)
              let ve = Local.calcNew (fst vs.(n)) vb (-2.0) in
              let fve = f ve in
                if fve > fvr then
	        shift (ve,fve) n vs (* l'étendu est le meilleur *)
              else
                (*le réfléchi est meilleur quel'etendu*)
                shift (vr,fvr) n vs 
            else (* le contracté *) 
            let vc = Local.calcNew (fst vs.(n)) vb 0.5 in
            let fvc = f vc in
              if fvc > snd vs.(n-1) || fvr > snd vs.(n-1) then
              let v = if fvr > fvc
                      then (vr,fvr) (* le réfléchi est le meilleur *)
                      else (vc,fvc) (* le contracté est le meilleur *)
              in begin (* on insere le nouveau point a sa place *)
                let i = ref n
                in while !i > 0 && snd vs.(!i-1) < snd v do
                     vs.(!i) <- vs.(!i-1);
                     decr i
                done;
                vs.(!i) <- v
              end else begin (* on contracte *)
                for i = 1 to n do
		  let vi = Local.calcNew (fst vs.(i)) (fst vs.(0)) 0.5
                  in vs.(i) <- (vi, f vi)
                done;
		Misc.array_sort (fun (_,x) (_,y) -> x > y) vs
	      end
      done;
      fst vs.(0)


let simplex start simp_iterations simp_alpha simp_precision = 
  simplex_4 Local.evalData start 
            simp_iterations simp_alpha simp_precision

