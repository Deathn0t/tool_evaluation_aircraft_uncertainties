let delta = 5.           (* trace time steps (sec) *)
let lag = 30.            (* time to apply maneuver (sec) *)
let alpha = 5.           (* angle discretisation (deg) *)
let std_turn = 120.      (* standard turn (sec/360°) *)
let max_turn = 10.       (* max turn (deg) *)
let sep = 5.             (* minimal separation (Nm) *)
let dspeed = 0.05        (* Speed uncertainty rate *)

let readplns ()=
  let chin = open_in Sys.argv.(1) in
  let maxt = ref 0. in
  let l = Str.split (Str.regexp "[ \t]") (input_line chin) in
  let (nbacft, tdeb) = match l with
    | a::b::_ -> (int_of_string a, float_of_string b)
    | _ -> failwith "erreur dans le nombre de plns de vol" in
  let tabl = Array.init nbacft (fun i -> []) in
  let speeds = Array.init nbacft (fun i -> 0.) in
  let () = 
    try
      while true do
	let l = Str.split (Str.regexp "[ \t]") (input_line chin) in
	let i = int_of_string (List.hd l) in
	let (t, x, y, speed) = match List.map float_of_string (List.tl l) with
	  | t::x::y::speed::[] -> (t, x, y, speed)
	  | _ -> failwith "erreur lecture plns" in
	if t > !maxt then maxt := t;
	tabl.(i) <- (t, (x, y))::tabl.(i);
	speeds.(i) <- speed;
      done
    with End_of_file -> () in
  close_in chin;
  let plns = Array.init nbacft (fun i -> Array.of_list (List.rev tabl.(i))) in
  (nbacft, tdeb, !maxt, plns, speeds)

let (nbacft, tdeb, maxt, plns, speeds) = readplns ()

let copyplans plns=
  let file = open_out Sys.argv.(2) in
  Printf.fprintf file "%d %f\n" (Array.length plns) tdeb;
  Array.iteri (fun i pln ->
    Array.iter (fun (t, (x,y)) ->
      Printf.fprintf file "%d %f %f %f %f\n" i t x y speeds.(i)) pln) plns;
  close_out file

let int_alpha = truncate alpha
let t0min = truncate (tdeb +. lag)
let deltat0 =  (truncate (maxt -. tdeb)) / 2
let t0max = (t0min + deltat0)
let t1min =  t0min 
let deltat1 = (truncate (maxt -. tdeb)) / 2
let t1max =  (t1min + deltat1)
let hmin = -40
let hmax = 40
let lagt0t1 = 10

(* Float coords 2D --------------------------------------------------------- *)

type xy = float * float

let mul k (x, y) = (k *.x, k *.y)
let add (x1, y1) (x2, y2) = (x1 +.x2, y1 +.y2)
let sub (x1, y1) (x2, y2) = (x1 -.x2, y1 -.y2)
let sca (x1, y1) (x2, y2) = x1 *.x2 +.y1 *.y2
let det (x1, y1) (x2, y2) = x1 *.y2 -.y1 *.x2

let norm2 v = sca v v
let norm v = sqrt (norm2 v)

let bary (t1, xy1) (t2, xy2) t = 
  add xy1 (mul ((t -.t1) /.(t2 -.t1)) (sub xy2 xy1))

let round a = truncate (floor (a +.0.5))

let pi = atan2 0. (-1.)
let twopi = 2. *.pi
let radians = pi /.180.

let angle (x, y) = atan2 y x

let sub_angle a1 a2 =
  let d = a1 -.a2 in
  if d < -.pi then d +.twopi else if pi <= d then d -.twopi else d

let turn a b c = sub_angle (angle (sub b a)) (angle (sub c a)) 

(* PLN & routes ------------------------------------------------------------ *)

type pln = (float * xy) array  (* (time in seconds, coords in Nm) *)

let create_pln speed ds t0 xys =
  let t = ref t0 in
  Array.mapi (fun i xyi ->
    if i <> 0 then (
      let dt = 3600./.speed /.(1.+.Random.float (2.*.ds) -.ds) in
      t := !t +.norm (sub xyi xys.(i - 1)) *.dt);
    (!t, xyi)) xys


(* Conflicts detection ---------------------------------------------------- *)

let roots1 a b =
  (* solve ax + b < 0. in [0, 1] *)
  if a = 0. then (if b < 0. then [0., 1.] else [])
  else (
    let x = -.b /.a in
    if a < 0. then (if x < 1. then [max 0. x, 1.] else [])
    else if 0. < x then [0., min 1. x] else [])

let roots2 a b c =
  (* solve ax2 + 2bx + c < 0. for x in [0, 1] *)
  if a = 0. then roots1 (2.*.b) c
  else (
    let delta = b ** 2. -.a *.c in
    if 0. < delta then (
      let r = sqrt delta in
      let x1 = (-.b -.r) /.a and x2 = (-.b +.r) /.a in
      if a < 0. then (
	if x1 < 1. then (
	  if 0. < x2 then [(0., x2); (x1, 1.)]
	  else [max 0. x1, 1.])
	else if 0. < x2 then [0., min 1. x2] 
	else [])
      else if x1 < 1. then (
	if 0. < x2 then [max 0. x1, min 1. x2]
	else [])
      else [])
    else if a < 0. then [0., 1.]
    else [])

let seg_detect orig1 dest1 orig2 dest2 sep dsep =
  let x = sub orig2 orig1 in
  let v = sub (sub dest2 dest1) x in
  roots2 (norm2 v -.dsep ** 2.) (sca x v -.sep *.dsep) (norm2 x -.sep ** 2.)

let pred_detect pred1 pred2 speed1 speed2 =
  let s=ref 0. and tpsmin=ref (maxt-.tdeb) in
  let vsep = dspeed *.(speed1 +.speed2) /.3600. in
  let rec detect leg1 orig1 leg2 orig2 td sep_t =
    if leg1 < Array.length pred1 && leg2 < Array.length pred2 then (
      let t1 = fst pred1.(leg1) and t2 = fst pred2.(leg2) in
      let next_t = min t1 t2 in
      let (dest1, next_leg1) =
	if t1 = next_t then (snd pred1.(leg1), leg1 + 1) 
	else (bary pred1.(leg1 - 1) pred1.(leg1) next_t, leg1) in
      let (dest2, next_leg2) =
	if t2 = next_t then (snd pred2.(leg2), leg2 + 1) 
	else (bary pred2.(leg2 - 1) pred2.(leg2) next_t, leg2) in
      let next_sep = sep +.vsep *.(next_t -.fst pred1.(0)) in
      detect next_leg1 dest1 next_leg2 dest2 next_t next_sep;
      List.iter (fun (t1, t2) ->
	tpsmin:= min !tpsmin (td+.t1*.(next_t-.td)-.tdeb);
	s:= !s+.(t2-.t1)*.(next_t-.td)
      )
	(seg_detect orig1 dest1 orig2 dest2 sep_t (next_sep -.sep_t))) in
  if fst pred1.(0) <> fst pred2.(0) then
    Printf.printf "pred_detect: %f\n%!" (fst pred1.(0) -.fst pred2.(0));
  detect 1 (snd pred1.(0)) 1 (snd (pred2.(0))) (fst pred1.(0)) sep;
  (!tpsmin, !s)

let conflictstab plns tab t =
  let stot=ref 0. and tpsmin=ref (maxt-.tdeb) in
  Array.iteri (fun i1 pln1 ->
    for i2 = 0 to i1 - 1 do
      let (mini,s)=pred_detect pln1 plns.(i2) speeds.(i1) speeds.(i2) in
      tab.(i1) <- tab.(i1)+.s;
      tab.(i2) <- tab.(i2)+.s;
      stot:= !stot +. s;
      tpsmin:= min !tpsmin mini
    done) plns;
  (!stot, !tpsmin)

(* Config ------------------------------------------------------------------ *)

type man = {t0: int; t1: int; h: int}
  
type data = {d: man array; f: float array}

let get_step pln t =
  let rec dicho i j =
    let k = (i + j) / 2 in
    if k = i then i else if t < fst pln.(k) then dicho i k else dicho k j in
  dicho 0 (Array.length pln - 1)

let clean pln =
  let eps = alpha *.radians /.2. in
  let rec reduce = function
    | a::b::(c::_ as tl) when abs_float (turn a b c) < eps -> reduce (a::tl)
    | a::tl -> a::reduce tl
    | [] -> [] in
  Array.of_list (reduce (Array.to_list pln))

let nav speed xys =
  let max_a = max_turn *.radians in
  let da = alpha *.radians in
  let epsilon = da /.2. in
  let radius = speed /.3600.*.std_turn /.pi in
  let min_d = radius *.max_a in
  let rec get hdg = function
    | xy1::xy2::tl when xy1 = xy2 -> get hdg (xy1::tl)
    | xy1::xy2::tl ->
      let v = sub xy2 xy1 in
      let d = norm v in
      let dir = angle v in
      let keep_xy2 = match tl with
	| [xy] when norm (sub xy1 xy) < d +.1.2 *.radius -> false
	| xy::_ -> epsilon < abs_float (sub_angle (angle (sub xy xy2)) dir)
	| _ -> true in
      if keep_xy2 then (
	let turn = sub_angle dir hdg in
	if max_a +.epsilon < abs_float turn then (
	  let h = sub_angle hdg (if turn < 0. then max_a else -.max_a) in
	  let xy = add xy1 (mul min_d (cos h, sin h)) in
	  let d2 = norm (sub xy xy2) in
	  xy1::get h (xy::if radius < d2 || tl = [] then xy2::tl else tl))
	else (
	  let a = if tl = [] then turn else (da *.float (round (turn /.da))) in
	  let h = sub_angle hdg (-.a) in
	  xy1::get h (add xy1 (mul (max d min_d) (cos h, sin h))::tl)))
      else get hdg (xy1::tl)
    | l -> l in
  get (angle (sub (List.nth xys 1) (List.hd xys))) xys

let modifplan m oldplan vit =
  (* let t0=float m.t0 and t1=float m.t1 and h=float m.h in *)
  let t0 = float (if Array.length oldplan <> 2 then t0min else m.t0) in
  let t1 = float m.t1 and h = float m.h in
  let before = get_step oldplan t0 in
  let after = before + 1 in
  let (_, pd) = oldplan.(before)
  and (_, pf) = oldplan.(after) in 
  let (dx, dy) = sub pf pd in
  let cap = atan2 dy dx in
  let deb = bary oldplan.(before) oldplan.(after) t0 in
  let (_, dest) = oldplan.(Array.length oldplan-1) in
  (*
  let pln_end=
    if m.h=0 then [|deb;dest|]  
    else
      if t1>t0 then 
	let newcap=cap +. h/.180.*.pi in
	let fin=add deb (mul ((t1-.t0)*.vit/.3600.) (cos newcap,sin newcap)) in
	[|deb;fin;dest|] 
      else [|pf;dest|] in 
  let xys=Array.append (Array.map (fun x ->snd x) (Array.sub oldplan 0 after)) pln_end in
  let xysr=clean xys in *)
  let xys_end =
    if t1 <= t0 then [pd; deb; dest]
    else (
      let newcap = cap +. h /. 180. *. pi in
      let dist = (t1 -. t0) *. vit /.3600. in
      let fin = add deb (mul dist (cos newcap, sin newcap)) in
      [pd; deb; fin; dest]) in
  let first = Array.map snd (Array.sub oldplan 0 before) in
  let next = Array.of_list (nav vit xys_end) in
  create_pln vit 0. tdeb (Array.append first next)

(*
let simplify oldplan vit =
  let nbpts=Array.length oldplan in
  if nbpts<=2 then oldplan 
  else 
    let dest=snd oldplan.(nbpts-1) in
    let next=snd oldplan.(1) in
    let curr=snd oldplan.(0) in
    let eps = alpha *.radians /.2. in
    let ta=turn curr next dest in
    if (abs_float ta<eps) & (norm (sub next curr)/.vit*.3600.>lag) then
      begin
(*	Printf.printf "%f %f\n" ta eps; flush stdout;*)
	create_pln vit 0. tdeb [|curr;dest|]
      end
    else oldplan
*)

let simplify oldplan vit =
  let t = tdeb +. lag in
  let i = get_step oldplan t in
  let xy = bary oldplan.(i) oldplan.(i + 1) t in
  let xys = [snd oldplan.(i); xy; snd oldplan.(Array.length oldplan - 1)] in
  let xys_end = Array.of_list (nav vit xys) in
  let pln_end = create_pln vit 0. (fst oldplan.(i)) xys_end in
  Array.append (Array.sub oldplan 0 i) pln_end

let prefer oldplan =
  let nbpts = Array.length oldplan in
  if nbpts <= 2 then {t0=t0min; t1=t1max; h=0} 
  else 
    let dest = snd oldplan.(nbpts - 1) in
    let next = snd oldplan.(1) in
    let curr = snd oldplan.(0) in
    let suiv = snd oldplan.(2) in
    let eps = alpha *.radians /.2. in
    let ta = turn curr next dest in
    if abs_float ta < eps then
      begin
	let a = round ((turn next suiv dest)*.180./.pi) in
	(* Printf.printf "h=%d\n" a; flush stdout;*)
	{t0=t0min; t1=t1max; h=a}
      end
    else {t0=t0min; t1=t1max; h=0}

let direct plan =
  Array.init nbacft (fun i ->
    simplify plan.(i) speeds.(i)) 

let prefs = Array.init nbacft (fun i -> prefer plns.(i)) 
   
let plns = direct plns

let nbman tabman =
  snd (Array.fold_left (fun (i, n) {t0=t0; t1=t1; h=h} ->
    if h * prefs.(i).h > 0 || (h = 0 && prefs.(i).h = 0) then (i + 1, n)
    else (i + 1, n + 1)) (0, 0) tabman)

let delay plns newplns =
  snd (Array.fold_left (fun (i, d) pln -> 
    let curr = snd plns.(i).(0) in
    let dest = snd plns.(i).(Array.length plns.(i) - 1) in
    let tps = norm (sub curr dest) /. speeds.(i) *. 3600. in
    let t_end = fst newplns.(i).(Array.length newplns.(i) - 1) in
    (i + 1, d +. t_end -. tdeb -. tps)) (0, 0.) plns)

let debut tabman =
  Array.fold_left (fun s {t0=t0; t1=t1; h=h} -> 
    if h = 0 then s else (s + t0max -t0)) 0 tabman

let eval data =
  let newplns=Array.mapi(fun i pln -> modifplan data.d.(i) pln speeds.(i)) plns in
  let (conf,tpsmin)=conflictstab newplns data.f tdeb in
  if conf>0. then (1./.(1.+.conf))*.tpsmin/.(maxt-.tdeb)
  else 
    let nbmans=float (nbman data.d)
    and delays=delay plns newplns 
    and debut=float (debut data.d) in
    (* Printf.printf "%f %f\n" nbmans delays; *) 
    (* 1.+.((float nbacft)-.nbmans)+.(1./.(1.+.delays+.debut)) *)
    1.+.((float nbacft)-.nbmans)+.(1./.(1.+.2.*.delays+.debut))
   
let scale data =
  {d=Array.init nbacft (fun i ->
    let newt0=min (max data.d.(i).t0 t0min) t0max in
    (*
    let newt1=	if data.d.(i).t1> t1max then t1max
      else if data.d.(i).t1<newt0+lagt0t1 then min t1max (newt0+lagt0t1)
      else data.d.(i).t1 in
    let newh= if newt1=newt0 then 0 else max hmin (min hmax (int_alpha*(data.d.(i).h/int_alpha))) in
    *)
    let newt1 = min (max data.d.(i).t1 t1min) t1max in
    let newh = min (max (int_alpha * (data.d.(i).h / int_alpha)) hmin) hmax in
    {t0=newt0;t1=newt1;h=newh});f=data.f} 

let generate () = 
  let d = Array.init nbacft (fun i ->
    {t0 = t0min + Random.int deltat0;
     t1 = t1min + Random.int deltat1;
     h = hmin + int_alpha * Random.int ((hmax - hmin) / int_alpha + 1)}) in
  scale {d = d; f = Array.init nbacft (fun i -> 0.)}

let cross a b = 
  let newa =
    Array.init nbacft (fun i -> 
      if Random.bool () then a.d.(i) else b.d.(i)) in
  scale {d = newa; f = Array.init nbacft (fun i -> 0.)}

let mutate a =
  let i = Random.int nbacft in
  let newa = Array.mapi (fun j aj ->
    if i = j then 
      { t0 = t0min + Random.int deltat0;
	t1 = t1min + Random.int deltat1;
	h = hmin + int_alpha * Random.int ((hmax - hmin) / int_alpha + 1) }
    else aj) a.d in
  scale {d = newa; f = Array.init nbacft (fun i -> 0.)}
    
let print_data data = 
  (*
    print_newline ();  
    Array.iteri (fun i x -> Printf.printf "t0=%d %d %d t1=%d %d %d  h=%d f=%f\n" t0min x.t0 t0max t1min x.t1 t1max x.h data.f.(i)) data.d
  *)
  ()

let print_best data =
  let newplns = Array.mapi (fun i pln -> 
    modifplan data.d.(i) pln speeds.(i)) plns in
  copyplans newplns

let _ =
  try
    let best_f = ref 0. and n_best = ref 0 in
    let continue numgen pop =
      let f = snd pop.(0) in
      if f = !best_f then incr n_best else (best_f := f; n_best := 0);
      !best_f < 1. || !n_best < 100 in
    let pb = {
      Ga.generate = generate;
      Ga.eval = eval;
      Ga.mutate = mutate;
      Ga.cross = cross;
      Ga.continue = continue;
    } in
    let top = Sys.time () in
    let pop = Ga.solve ~nb_elems:200 ~nb_gen:1000 ~pcross:0.4 ~pmut:0.4 pb in
    Printf.printf "GA: f=%f\n%f sec.\n%!" 
      (snd pop.(0)) (Sys.time () -. top);
    print_best (fst pop.(0))
  with x -> Printf.printf "### GA: %s\n%!" (Printexc.to_string x)

