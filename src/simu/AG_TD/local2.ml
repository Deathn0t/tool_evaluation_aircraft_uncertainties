let lag = 15.            (* time to apply maneuver (sec) *)
let alpha = 5.           (* angle discretisation (deg) *)
let max_turn = 45.       (* max turn angle (deg) *)

let alpha_rad = alpha *. Xy.radians
let max_turn_rad = max_turn *. Xy.radians
let lagt0t1 = 10.

type pb = { 
    tmin: Acft.time;
    tmax: Acft.time;
    t0min: Acft.time array;
    pref: float array;  (* -1. | 0. | 1.*)
    acft: Acft.t array;
    incert: int;
}

type man = {t0: float; t1: float; h: float}

type data =
    {
     d: man array;
     f: float array;
     mutable acft: Acft.t array;
   }

let t_bgn route = fst route.(0)
let t_end route = fst route.(Array.length route - 1)

let pb =
  let angle (_, xy1) (_, xy2) = 
    Xy.angle (Xy.sub xy2 xy1) in
  let (incert,acft) = Acft.load Sys.argv.(1) in
  let t = t_bgn (Acft.predict acft.(0)) in
  let tend = Array.map (fun a -> t_end (Acft.predict a)) acft in
  let tmax = (t +. lag +. Array.fold_left max t tend) /. 2. in
  let t0min = Array.map (fun a -> max t (t_bgn (Acft.route a) +. lag)) acft in
  let alpha_min = alpha_rad /. 2. in
  let pref = Array.map (fun a -> 0.) acft in
  Array.iteri (fun i a ->
    let tp = Acft.predict a in
    let l = Array.length tp in
    if 2 < l then (
      let dh = Xy.sub_angle (angle tp.(0) tp.(l - 1)) (angle tp.(0) tp.(1)) in
      if abs_float dh < alpha_min then (
	let turn = Xy.sub_angle (angle tp.(1) tp.(2)) (angle tp.(0) tp.(1)) in
	if turn <= -.alpha_min then pref.(i) <- -1. 
	else if alpha_min <= turn then pref.(i) <- 1.;
	Acft.turn a t0min.(i) 0. t0min.(i);
	Acft.apply a)))
    acft;
  {tmin=t +. lag; tmax=tmax; t0min=t0min; pref=pref; acft=acft ; incert=incert}

(*    
let pbcopy pb =
  {tmin=pb.tmin;tmax=pb.tmax;t0min=pb.t0min;pref=Array.copy pb.pref;incert=pb.incert;
   acft=Array.init (Array.length pb.acft) (fun i -> Acft.copier pb.acft.(i))}
*)
    
let scale data =
  Array.iteri (fun i man ->
    let t0 = max pb.t0min.(i) (min pb.tmax man.t0) in
    let t1 = max t0 (min pb.tmax man.t1) in
    let h_alpha = alpha_rad *. floor (man.h /. alpha_rad +. 0.5) in
    let h = max (-.max_turn_rad) (min max_turn_rad h_alpha) in
    let l = Array.length (Acft.predict pb.acft.(i)) in
    data.d.(i) <-
      if l = 2 && h = 0. then {t0=pb.t0min.(i); t1=pb.t0min.(i); h=0.}
      else if t1 < t0 +. lagt0t1 then
	if t0+.lagt0t1<=pb.tmax then {t0=t0;t1=t0+.lagt0t1;h=h}
(*	else if t1-.lagt0t1 >=pb.t0min.(i) then {t0=t1-.pb.t0min.(i);t1=t1;h=h} *)
	else {t0=t0; t1=t0; h=0.}
      else {t0=t0; t1=t1; h=h})
    data.d;
  {d = data.d; f = Array.map (fun _ -> 0.) data.d; acft=data.acft}


  
    
let get_nb_dev data =
  Array.fold_left (fun n m -> 
    if m.t0 <> m.t1 then (n +. 1.) else n)
    0. data.d 

let get_delay () =
  Array.fold_left (fun d a -> d +. Acft.delay a) 0. pb.acft

let evalData data =
  let lpb=pb in
  Array.iteri (fun i man -> Acft.turn lpb.acft.(i) man.t0 man.h man.t1) data.d;
  Array.iter (Acft.set_man pb.incert) lpb.acft;
(* 
let _ = Parmap.array_parmapi ~ncores:4  (fun i man ->
  Acft.turn lpb.acft.(i) man.t0 man.h man.t1;
  Acft.set_man lpb.incert lpb.acft.(i);
  Acft.printdev lpb.acft.(i) i

  ) data.d in *)
  

(*
  for i=0 to Array.length pb.acft -1 do
  Acft.set_man pb.acft.(i) pb.incert;
  done;
 *)
  let (mat,confnico)=Acft.detect_traj_glob pb.incert lpb.acft in
  Array.iteri (fun i x -> data.f.(i) <-x) mat;
(*
  Array.iteri (fun i _ ->
    Array.iter (fun conf_t ->
      if conf_t then (
	data.f.(i) <- data.f.(i) +. 1.;
	confnico := !confnico +. 1.))
      (Acft.detect_traj pb.incert pb.acft i)) pb.acft; *)
  
 (* let conf = ref 0. in
    Array.iteri (fun i dt ->
    data.f.(i) <- data.f.(i) +. dt;
    conf := !conf +. dt) 
    (Acft.detect pb.acft); *)
  
  let n = float (Array.length lpb.acft) in
  Printf.printf "confnico=%f\n" confnico; 
  if confnico = 0. then (
    let nb_dev = get_nb_dev data in
    let delay = get_delay () in
    let first_t0 = Array.fold_left (fun t0 d ->
      if d.t0 <> d.t1 then min d.t0 t0 else t0)
      pb.tmax data.d in
    let rt0 = (first_t0 -. pb.tmin) /. (pb.tmax -. pb.tmin) in
    let (_, pref) = Array.fold_left (fun (i, p) m ->
      (i + 1, if 0. < m.h *. pb.pref.(i) then p +. 1. else p))
      (0, 0.) data.d in
    1. +. (n -. nb_dev) /. n +. rt0 +. 100. /. (100. +. delay) +. 2.*.pref /.n)
  else (n /. (n +. confnico))

let genData () = 
  let dh = 2. *. max_turn_rad and dt = pb.tmax -. pb.tmin in
  let d = Array.mapi (fun i _ -> 
    { t0 = pb.t0min.(i) +. Random.float dt;
      t1 = pb.t0min.(i) +. Random.float dt;
      h = -.max_turn_rad +. Random.float dh })
      pb.acft in
(*  Array.iteri (fun i man -> Acft.turn pb.acft.(i) man.t0 man.h man.t1) data.d;
  Array.iter (Acft.set_man pb.incert) pb.acft; *)
  scale {d=d;f = Array.map (fun _ -> 0.) d; acft=[||]}

let cross a b = 
  let rnd_bary v1 v2 =
    let c = -0.5 +. Random.float 2. in
    v1 *. c +. v2 *. (1. -. c) in
  let newad = Array.mapi (fun i _ ->
    if a.f.(i) < b.f.(i) then a.d.(i)
    else if b.f.(i) < a.f.(i) then b.d.(i)
    else if Random.int 2 = 0 then a.d.(i)
    else b.d.(i))
    a.d in
  let newbd = Array.mapi (fun i _ -> 
    { t0 = rnd_bary a.d.(i).t0 b.d.(i).t0;
      t1 = rnd_bary a.d.(i).t1 b.d.(i).t1;
      h = rnd_bary a.d.(i).h b.d.(i).h })
    a.d in
  (scale {d=newad;f = Array.map (fun _ -> 0.) newad; acft=[||]},
   scale  {d=newbd;f = Array.map (fun _ -> 0.) newbd; acft=[||]})

let mutate a =
(*  let n=(Array.length a.d) in
  let mi=ref (Random.int n) in
  let fi=ref a.f.(!mi) in
  for k=0 to n-1 do
    let ki=Random.int n in
    if a.f.(ki)> !fi then (mi:=ki;fi:=a.f.(ki))
  done;
  let i= !mi in *)
  let i = Random.int (Array.length a.d) in 
  let dh = 2. *. max_turn_rad in
  let dt = pb.tmax -. pb.tmin in
  let newad = Array.mapi (fun j man ->
    if i = j then (
      let k = Random.int 3 in
      { t0 = if k = 0 then pb.t0min.(i) +. Random.float dt else man.t0;
	t1 = if k = 1 then pb.t0min.(i) +. Random.float dt else man.t1;
	h  = if k = 2 then -.max_turn_rad +. Random.float dh else man.h })
    else man)
    a.d in
  scale  {d=newad;f = Array.map (fun _ -> 0.) newad; acft=[||]}

let print_data data = ()
    
let print_best data = 
  Array.iteri (fun i man -> Acft.turn pb.acft.(i) man.t0 man.h man.t1) data.d;
  Array.iteri (fun i _ -> Acft.set_man pb.incert pb.acft.(i)) data.d;
  Array.iteri (fun i m ->
    Printf.printf "%.0f %.0f %.0f %.0f\n" 
      pb.t0min.(i) m.t0 (Xy.degrees *. m.h) m.t1) data.d;
  Printf.printf "dev: %.0f, delay: %.0f\n" (get_nb_dev data) (get_delay ());
(*  Acft.print_pln pb.acft; *)
  Array.iter Acft.apply pb.acft;
  Acft.save Sys.argv.(2) pb.acft pb.incert

let dataDistance d1 d2 =
  let dist = ref 0 in
  Array.iteri (fun i _ -> if d1.d.(i).h *. d2.d.(i).h < 0. then incr dist) d1.d;
  float !dist

let dataBarycenter d1 n1 d2 n2 = 
  d1
    
let calcNext data i alpha =
  data
    
let calcNew data source factor =
  source

let dim = 1
    
let endlocal data = 
  let fit = evalData data in
  Printf.printf "best: f=%f\n%!" fit

let endlocalerror data = ()
    
