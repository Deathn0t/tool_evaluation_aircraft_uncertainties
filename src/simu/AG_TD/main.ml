let computeRMeanSigmaBestMaxfMinf pop = 
  let sum = ref 0.0 and sum2 = ref 0.0 in
  let maxf = ref (-1.0 /. 0.0) and minf = ref (1.0 /. 0.0) and best = ref 0 in
  for i = 0 to Array.length pop - 1 do 
    sum := !sum +. pop.(i).Types.r_fit;
    sum2 := !sum2 +. pop.(i).Types.r_fit *. pop.(i).Types.r_fit;
    if pop.(i).Types.r_fit > !maxf then (
      maxf := pop.(i).Types.r_fit;
      best := i);
    if pop.(i).Types.r_fit < !minf then minf := pop.(i).Types.r_fit;
  done; 
  sum := !sum /. float (Array.length pop);
  sum2 := !sum2 /. float (Array.length pop) -. (!sum *. !sum);
  sum2 := sqrt !sum2;
  (!sum, !sum2, pop.(!best), !maxf, !minf);;

let stat pop = 
  let (moy, sigma, best_elem, val_best, _) = 
    computeRMeanSigmaBestMaxfMinf pop in
  (*
    Printf.fprintf !Gvars.fileout "generation : %d\n" !numgen;
    Printf.fprintf !Gvars.fileout "best_elem=";
  *)
  Local.print_data best_elem.Types.data;
  (*
    Printf.fprintf !Gvars.fileout "Types.r_fit = %f\n" best_elem.Types.r_fit;
    Printf.fprintf !Gvars.fileout "moy=%f sigma=%f\n\n" moy sigma;
    flush !Gvars.fileout;;
  *)
  () 
    
let one_step pop1 scaling sharing complex_sharing elitist evolutive pcross pmut = 
  Scale.scale pop1 scaling;
  let prot = Share.share elitist sharing complex_sharing pop1 in
  let (newprot, pop2) = Reproduce.reproduce pop1 prot in
  Crossmut.crossmut pop2 pop1 newprot evolutive pcross pmut;
  stat pop1;
  flush stdout;;

let algogen () = 
  Printf.fprintf !Gvars.fileout "Starting AG\n";
  flush !Gvars.fileout;
  let top = Sys.time () in
  let (pcross, pmut,
       scaling, elitist, sharing, complex_sharing, evolutive,
       simp_alpha, simp_precision, simp_iterations) = Gvars.read_config () in
  let pop1 = Array.of_list (Initpop.initPop !Gvars.nbelems) in
  while !Gvars.numgen < Gvars.nbgens do 
    incr Gvars.numgen;
    one_step pop1 scaling sharing complex_sharing elitist evolutive
      pcross pmut;
  done;
  Scale.scale pop1 scaling;
  let best_elems = List.sort (fun i j -> 
    compare pop1.(j).Types.r_fit pop1.(i).Types.r_fit)
    (Share.share elitist sharing complex_sharing pop1) in
  let indbest = List.hd best_elems in
  (*
    Printf.fprintf !Gvars.fileout "\nResultats:\n";
    Printf.fprintf !Gvars.fileout "best_elem=";
  *)
  Local.print_data pop1.(indbest).Types.data;
  Local.print_best pop1.(indbest).Types.data;
  (*
    Printf.fprintf !Gvars.fileout "Types.r_fit = %f\n" pop1.(indbest).Types.r_fit;
    flush !Gvars.fileout;
    print_string "Meilleurs elements :\n";
    (List.iter 
    (function i -> print_string "element= "; 
    Local.print_data pop1.(i).Types.data;
    Printf.printf "Types.r_fit=%f\n" pop1.(i).Types.r_fit) 
    best_elems);
  *)
  if simp_iterations <> 0 then (
    let simp = Simplex.simplex 
      pop1.(indbest).Types.data 
      simp_iterations 
      simp_alpha 
      simp_precision in
    (*
      Printf.fprintf !Gvars.fileout "Apres simplex:\n";
      Printf.fprintf !Gvars.fileout "best_elem= "; Local.print_data simp;    
      Printf.fprintf !Gvars.fileout "Types.r_fit = %f\n" (evalData simp);
      flush !Gvars.fileout;
    *)
    Local.endlocal simp;
    (*
      print_string "Meilleurs elements:\n";
    *)
    List.iter (fun i ->
      let (* x *) _ = Simplex.simplex 
	pop1.(i).Types.data
	simp_iterations
	simp_alpha
	simp_precision in
      (*
	print_string "element= "; Local.print_data x;
	Printf.printf "Types.r_fit=%f\n" (evalData x);
      *)
      ()) best_elems)
  else Local.endlocal pop1.(indbest).Types.data;
  Local.print_data pop1.(indbest).Types.data;
  Printf.printf "%f sec.\n%!" (Sys.time () -. top)
    
(*    
      let _= try (Printexc.catch algogen ();flush !Gvars.fileout;exit 0)
      with _ ->
      Printf.fprintf !Gvars.fileout "\n";
      flush !Gvars.fileout;
      Local.endlocalerror ()
*)

let _ = 
  try 
    algogen ();
    flush !Gvars.fileout;
  with x -> 
    Printf.printf "%s\n%!" (Printexc.to_string x);
    Printf.fprintf !Gvars.fileout "%s\n" (Printexc.to_string x);
    Printexc.print_backtrace !Gvars.fileout;
    flush !Gvars.fileout
