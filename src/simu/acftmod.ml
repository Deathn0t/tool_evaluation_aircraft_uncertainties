let sep = 5.            (* separation distance (NM) *)
let step = 10.          (* trajectory prediction step (s) *)
let lag = 30.
let max_speed = 550.    (* max speed (knots) *)
let dspeed = 0.05       (* speed uncertainty rate *)
let vector = 60.        (* speed vector size (s) *)
let comet_step = 5.     (* comet plots separation (s) *)
let std_turn = 3.       (* standard turn (°/s) *)
let alpha = 5.          (* heading discretisation (°) *)
let max_turn = 30.      (* max turn angle (°) between each plot *)
let max_start_t = 300.  (* max difference between entry times *)
let min_conf_t = 120.   (* min time of first conflict after entry time *)

let hour = 3600.
let alpha_rad = alpha *. Xy.radians
let alpha_min = alpha_rad /. 2.
let max_turn_rad = max_turn *. Xy.radians
let turn_step = max_turn /. std_turn

type speed = float       (* speed (NM/s) *)
type time = float        (* time (s) *)
type xy = float * float  (* NM, NM *)
type route = (time * Xy.xy) array
type man = time * float * time

type t = {
  speed: speed;                (* Nominal speed (NM/s) *)
  mutable route: route;        (* Real route, with turn segments and dspeed *)
  mutable predict: route;      (* Trajectory prediction from current time *)
  mutable dev: route;          (* Deviation prediction from current time *)
  mutable man: man;
  mutable traj: ((float * float * float) list * float * float) array;
}

let new_acft speed = 
  {speed=speed; route=[||]; predict=[||]; dev=[||]; 
   man= (0., 0., 0.); traj=[||]}

let to_string route =
  let str (t, (x, y)) = Printf.sprintf "%.3f:%.3f,%.3f" t x y in
  String.concat " " (Array.to_list (Array.map str route))

let of_string str =
  let rec get tl = 
    try Scanf.sscanf tl "%f:%f,%f %[^\n]" (fun t x y tl -> (t, (x, y))::get tl)
    with _ -> [] in
  Array.of_list (get str)

(* Debug ------------------------------------------------------------------- *)

let safe name f x =
  try f x with err -> Printf.printf "*** %s\n%!" name; raise err

let check_leg route t leg =
  let l = Array.length route in
  if l < 2 || t <= fst route.(1) then
    if leg = 0 then leg else failwith "check_leg: 0"
  else if fst route.(l - 2) < t then
    if leg = l - 2 then leg else failwith "check_leg: l-2"
  else if fst route.(leg) < t && t <= fst route.(leg + 1) then leg
  else failwith "check_leg"

(* Conflicts detection with Funcnico --------------------------------------- *)

module TP = Funcnico

let cfg =
  let err = 2. in
  { TP.nb_ac = 2; 
    TP.norm2 = sep *. sep;
    TP.norm = sep; 
    TP.norm_r2 = sep *. sqrt 2.;
    TP.vnorm = 1.;
    TP.nb_steps = truncate (900. /. step);
    TP.step = step;
    TP.errspeed = 0.02 *. err;
    TP.err = err;
    TP.t0max = 0;
    TP.errt0 = 10. *. err;
    TP.t0low = 0.;
    TP.t0delta = 0.;
    TP.t1max = 3;
    TP.errt1 = 10. *. err;
    TP.t1low = 0.;
    TP.t1delta = 0.;
    TP.amax = 0;
    TP.erra = 2. *. err;
    TP.alow = 0.;
    TP.adelta = 0.;
    TP.nbman = 0;
    TP.run = 0;
    TP.nb_fl = 0;
    TP.flmin = 0.;
    TP.fldelta = 0.;
    TP.crerr = 0.;
    TP.tv = step *. Xy.pi /. 60.;
  }

let incert_route = Array.map (TP.convert cfg) [|0; 1; 8; 9|]
let incert_man = Array.map (TP.convert cfg) (Array.init 16 (fun i -> 2 * i))
let incert_all = Array.map (TP.convert cfg) (Array.init 64 (fun i -> i))

let set_traj a =
  let to_xyz (_, (x, y)) = (x, y, 0.) in
  let l = Array.length a.dev in
  let (t0, h, t1) = a.man in
  let xyz_end = to_xyz a.dev.(l - 1) in
  let rec extract i =
    if l - 1 <= i || t0 < fst a.dev.(i + 1) then (
      let t2 = max (fst a.dev.(i)) (min t1 (fst a.dev.(i - 1) +. vector)) in
      Printf.printf "set_traj: i=%d ti=%.0f\n%!" i (fst a.dev.(i + 1));
	let (x2, y2) = Xy.bary a.dev.(i - 1) a.dev.(i) t2 in
	[(x2, y2, 0.); to_xyz a.dev.(l - 1)])
    else to_xyz a.dev.(i)::extract (i + 1) in
  let t = fst a.dev.(0) -. mod_float (fst a.dev.(0)) cfg.TP.step in
  let (x0, y0) = Xy.bary a.dev.(0) a.dev.(1) t in
  let xyzs = (x0, y0, 0.)::if 2 < l then extract 1 else [xyz_end] in
  let route = {TP.wpts=xyzs; TP.speed=a.speed; TP.cr=0.} in
  let alpha = h /. Xy.radians in
  let n = List.length xyzs - 1 in
  let man =
    {TP.t0=max 0. (t0 -. t); TP.t1=t1 -. t; TP.a=alpha; TP.fl=0.; TP.n=n} in
  let incert = if t0 = t1 then incert_route else incert_all in
  let pts = Array.init cfg.TP.nb_steps (fun _ -> []) in
  Array.iter (fun pts_traj ->
    Array.iteri (fun t pt -> 
      match pt with Some p -> pts.(t) <- p::pts.(t) | None -> ()) 
      pts_traj)
    (Array.map (TP.navigate cfg route man) incert);
  a.traj <- Array.map (fun pts -> (TP.enveloppe pts, 0., 0.)) pts
    
let get_traj a = a.traj

let set_man a =
  let dt = turn_step *. 1.1 in
  let vect i = Xy.sub (snd a.dev.(i + 1)) (snd a.dev.(i)) in
  let sgn x = if 0. < x then 1 else if x = 0. then 0 else -1 in
  let sgn_det i = sgn (Xy.det (vect i) (vect (i + 1))) in
  let no_inflex i = 0 <= sgn_det (i - 1) * sgn_det i in
  let turn i = fst a.dev.(i + 1) -. fst a.dev.(i) < dt in
  let rec prev i = if 0 < i && turn i && no_inflex i then prev (i - 1) else i in
  if 2 < Array.length a.dev then (
    let i1 = prev (Array.length a.dev - 3) in
    let i0 = prev (i1 - 1) in
    Printf.printf "set_man: i0=%d i1=%d\n%!" i0 i1;
    let h = Xy.angle (vect i1) in
    if i1 = 0 then a.man <- (fst a.dev.(0), h, fst a.dev.(1))
    else a.man <- (fst a.dev.(i0 + 1), h, fst a.dev.(i1 + 1)))
  else a.man <- (fst a.dev.(0), 0., fst a.dev.(0));
  set_traj a

let detect_traj acft i =
  let cfg = {cfg with TP.nb_ac = Array.length acft} in
  Array.iter (fun a ->
    if Array.length (get_traj a) = 0 then set_traj a) acft;
  let trajs = Array.map get_traj acft in
  let mmtab = TP.minmax cfg trajs in
  let conf = Array.init cfg.TP.nb_steps (fun _ -> false) in
  Array.iteri (fun j aj ->
    if i <> j then (
      List.iter (fun t ->
	if not conf.(t) then (
	  let (traj_i, _, _) = trajs.(i).(t) in
	  let (traj_j, _, _) = trajs.(j).(t) in
	  if TP.all_pairs cfg.TP.norm2 traj_i traj_j false then
	    conf.(t) <- true))
	(TP.prefiltre cfg mmtab.(i) mmtab.(j))))
    acft;
  conf

let detect_traj acft = safe "detect_traj" (detect_traj acft)

(* Aircraft information and route prediction ------------------------------- *)

let speed a = a.speed
let route a = a.route
let predict a = a.predict
let dev a = a.dev

let get_leg route t =
  let rec dicho i j =
    let k = (i + j) / 2 in
    if k = i then i else if t <= fst route.(k) then dicho i k else dicho k j in
  (* check_leg route t (dicho 0 (Array.length route - 1)) *)
  dicho 0 (Array.length route - 1)

let get_heading route leg =
  Xy.angle (Xy.sub (snd route.(leg + 1)) (snd route.(leg)))

let roundxy (x, y) = (floor (x *. 1000.)/.1000., floor (y *. 1000.)/.1000.) 

let set_time a t =
  if t < fst a.route.(0) then (
    let (t0, xy0) = a.route.(0) in
    let xy1 = snd a.route.(1) in
    let t1 = t0 +. Xy.norm (Xy.sub xy1 xy0) /. a.speed in
    a.predict <- Array.copy a.route;
    a.predict.(0) <- (t, roundxy (Xy.bary a.route.(0) (t1, xy1) t)))
  else (
    let leg = get_leg a.route t in
    let nth = if fst a.route.(leg + 1) <= t then leg + 1 else leg in
    a.predict <- Array.sub a.route nth (Array.length a.route - nth);
    a.predict.(0) <- (t, roundxy (Xy.bary a.route.(leg) a.route.(leg + 1) t)));
  for i = 1 to Array.length a.predict - 1 do
    let (t, xy) = a.predict.(i - 1) in
    let xyi = snd a.predict.(i) in
    a.predict.(i) <- (t +. Xy.norm (Xy.sub xyi xy) /. a.speed, xyi)
  done;
  a.dev <- a.predict

let vector a =
  let t = fst a.predict.(0) in
  let leg = get_leg a.route t in
  Xy.bary a.route.(leg) a.route.(leg + 1) (t +. vector)

let comet a =
  let t = floor (fst a.predict.(0) /. comet_step) *. comet_step in
  let route = if t < fst a.route.(0) then a.predict else a.route in
  let leg = ref (get_leg route t) in
  Array.init 5 (fun i ->
    let ti = t -. comet_step *. float i in
    while 0 < !leg && ti < fst route.(!leg) do decr leg done;
    Xy.bary route.(!leg) route.(!leg + 1) ti)

let delay a =
  if 3 <= Array.length a.dev then (
    let (t0, xy0) = a.dev.(0) in
    let (t1, xy1) = a.dev.(Array.length a.dev - 1) in
    t1 -. t0 -. Xy.norm (Xy.sub xy1 xy0) /. a.speed)
  else 0.

let get_turn_info a t =
  if t <= fst a.route.(0) then (
    let t_step = fst a.route.(0) +. turn_step in
    let xy =  Xy.bary a.predict.(0) a.predict.(1) t_step in
    (Array.sub a.predict 0 1, get_heading a.predict 0, (t_step, xy)))
  else (
    let leg = get_leg a.predict t in
    let t_next = fst a.predict.(leg + 1) in
    let t_step = min t_next (max t (fst a.predict.(leg) +. turn_step)) in
    let xy = Xy.bary a.predict.(leg) a.predict.(leg + 1) t_step in
    (Array.sub a.predict 0 (leg + 1), get_heading a.predict leg, (t_step, xy)))

let deviate_aux a (past_route, heading, (t, xy)) xy_dev =
  let turn_dist = turn_step *. a.speed in
  let radius = a.speed *. 361. /. std_turn /. Xy.pi /. 2. in
  let rec nav heading t = function
    | xy1::xy2::tl ->
      let vect = Xy.sub xy2 xy1 in
      let dist= Xy.norm vect in
      if radius < dist then (  (* target is xy1 *)
	let h = Xy.angle vect in
	let dh = Xy.sub_angle h heading in
	let turn = 
	  if tl = [] then dh 
	  else alpha_rad *. floor (dh /. alpha_rad +. 0.5) in
	if abs_float turn <= max_turn_rad then (  (* xy1 is reachable *)
	  if tl <> [] then (
	    let h2 = Xy.angle (Xy.sub (List.hd tl) xy1) in
	    if alpha_min <= abs_float (Xy.sub_angle h h2) then (
	      let new_heading = Xy.add_angle heading turn in
	      let dir = Xy.polar new_heading in
	      let xy = roundxy (Xy.add xy1 (Xy.mul dist dir)) in
	      (t, xy1)::nav new_heading (t +. dist /. a.speed) (xy::tl))
	    else nav heading t (xy1::tl))
	  else [(t, xy1); (t +. dist /. a.speed, xy2)])
	else (  (* target is now xy2 *)
	  let dh = if turn < 0. then -.max_turn_rad else max_turn_rad in
	  let new_heading = Xy.add_angle heading dh in
	  let dir = Xy.polar new_heading in
	  let xy = roundxy (Xy.add xy1 (Xy.mul turn_dist dir)) in
	  (t, xy1)::nav new_heading (t +. turn_step) (xy::xy2::tl)))
      else if tl = [] then [(t, xy1); (t +. dist /. a.speed, xy2)]
      else nav heading t (xy1::tl)
    | [xy] -> [(t, xy)]
    | [] -> [] in
  let xy_end = snd a.predict.(Array.length a.predict - 1) in
  let dev = match nav heading t [xy; xy_dev; xy_end] with
    | (_, xy1)::((_, xy2)::_ as tl) as l ->
      let turn = Xy.sub_angle heading (Xy.angle (Xy.sub xy2 xy1)) in
      if abs_float turn < alpha_min then tl else l
    | l -> l in
  a.dev <- Array.append past_route (Array.of_list dev)

let deviate a t xy =
  if t +. comet_step < fst (a.predict.(Array.length a.predict - 1)) then 
    deviate_aux a (get_turn_info a t) xy

let turn a t0 alpha t1 =
  if t0 +. comet_step < fst (a.predict.(Array.length a.predict - 1)) then (
    let (past_route, heading, (t_dev, xy_dev) as info) = get_turn_info a t0 in
    let a2 = alpha /. 2. in
    let dist1 = abs_float (sin a2) *. a.speed /. 360. /. std_turn /. Xy.pi in
    let xy1 = Xy.add xy_dev (Xy.mul dist1 (Xy.polar (heading +. a2))) in
    let dt = abs_float alpha /. std_turn /. Xy.radians in
    let dist2 = a.speed *. (max 0. (t1 -. t_dev -. dt)) in
    let xy_dev = Xy.add xy1 (Xy.mul dist2 (Xy.polar (heading +. alpha))) in
    deviate_aux a info xy_dev)

let apply a =
  if 2 <= Array.length a.dev then (
    let t = fst a.dev.(0) in
    let leg = get_leg a.route (max t (fst a.route.(0))) in
    let past_route = Array.sub a.route 0 (leg + 1) in
    let nth = ref 1 in
    if 2 < Array.length a.dev then (
      let h1 = get_heading a.route leg in
      let h2 = get_heading a.dev 1 in
      if abs_float (Xy.sub_angle h1 h2) < alpha_min then nth := 2);
    let dev_route = Array.sub a.dev !nth (Array.length a.dev - !nth) in
    (* Add speed uncertainties without changing the current position *)
    let (t1, xy1) = a.route.(leg) and (t2, xy2) = a.route.(leg + 1) in
    let speed0 = Xy.norm (Xy.sub xy2 xy1) /. (t2 -. t1) in
    let min_speed = a.speed *. (1. -. dspeed) in
    let ds = 2. *. dspeed *. a.speed in
    Array.iteri (fun i (_, xy) ->
      let speed = if i = 0 then speed0 else (min_speed +. Random.float ds) in
      let (t0, xy0) = if i = 0 then (t1, xy1) else dev_route.(i - 1) in
      dev_route.(i) <- (t0 +. Xy.norm (Xy.sub xy xy0) /. speed, xy))
      dev_route;
    (* *)
    a.route <- Array.append past_route dev_route;
    a.predict <- a.dev)

let transform a dev ratio =
  a.dev <- dev;
  apply a;
  let l = Array.length dev in
  if 3 < l && ratio < 1. then (
    let t0 = fst dev.(0) and t1 = fst dev.(l - 2) in
    let t = (1. -. ratio) *. t0 +. ratio *. t1 in
    let (_, _, (_, xy) as info) = get_turn_info a t in
    deviate_aux a info xy)

(* Conflicts detection ----------------------------------------------------- *)

let pos_detect acft =
  let conf = Array.map (fun _ -> false) acft in
  Array.iteri (fun i ai ->
    let xyi = snd acft.(i).predict.(0) in
    for j = 0 to i - 1 do
      let xyj = snd acft.(j).predict.(0) in
      if Xy.norm (Xy.sub xyi xyj) < sep then (
	conf.(i) <- true;
	conf.(j) <- true)
    done)
    acft;
  conf

let roots1 a b =
  (* solve ax + b < 0. in [0, 1] *)
  if a = 0. then (if b < 0. then [0., 1.] else [])
  else (
    let x = -.b /. a in
    if a < 0. then (if x < 1. then [max 0. x, 1.] else [])
    else if 0. < x then [0., min 1. x] else [])

let roots2 a b c =
  (* solve ax2 + 2bx + c < 0. for x in [0, 1] *)
  if a = 0. then roots1 (2. *. b) c
  else (
    let delta' = b ** 2. -. a *. c in
    if 0. < delta' then (
      let r = sqrt delta' in
      let x1 = (-.b -. r) /.a and x2 = (-.b +. r) /.a in
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

let seg_detect orig1 dest1 orig2 dest2 orig_sep dsep =
  let x = Xy.sub orig2 orig1 in
  let v = Xy.sub (Xy.sub dest2 dest1) x in
  let a = Xy.norm2 v -. dsep ** 2. in
  roots2 a (Xy.sca x v -. orig_sep *. dsep) (Xy.norm2 x -. orig_sep ** 2.)

let detect_iter route1 route2 vsep last_t f =
  let next route (leg, t) =
    if fst route.(leg) = t then (leg + 1, snd route.(leg))
    else (leg, Xy.bary route.(leg - 1) route.(leg) t) in
  let first_t = max (fst route1.(0)) (fst route2.(0)) in
  let l1 = Array.length route1 and l2 = Array.length route2 in
  let rec iter (leg1, orig1) (leg2, orig2) t =
    if t < last_t && leg1 < l1 && leg2 < l2 then (
      let next_t = min last_t (min (fst route1.(leg1)) (fst route2.(leg2))) in
      let (_, dest1 as next1) = next route1 (leg1, next_t) in
      let (_, dest2 as next2) = next route2 (leg2, next_t) in
      let sep_t = sep +. vsep *. (t -. first_t) in
      let dt = next_t -. t in
      let dsep = vsep *. dt in
      List.iter (fun (t1, t2) -> 
	f leg1 leg2 (t +. t1 *.dt) (t +. t2 *. dt))
	(seg_detect orig1 dest1 orig2 dest2 sep_t dsep);
      iter next1 next2 next_t) in
  let leg_xy route =
    if fst route.(0) < first_t then (
      let leg = 1 + get_leg route first_t in 
      (leg, Xy.bary route.(leg - 1) route.(leg) first_t))
    else (1, snd route.(0)) in
  iter (leg_xy route1) (leg_xy route2) first_t

let detect acft =
  let times = Array.map (fun _ -> 0.) acft in
  for i = 1 to Array.length acft - 1 do for j = 0 to i - 1 do
      let vsep = dspeed *. (acft.(i).speed +. acft.(j).speed) in
      let conf_t = ref 0. in
      detect_iter acft.(i).dev acft.(j).dev vsep max_float (fun _ _ t1 t2 ->
	conf_t := !conf_t +. t2 -. t1);
      times.(i) <- times.(i) +. !conf_t;
      times.(j) <- times.(j) +. !conf_t;
    done done;
  times

let detect_lines acft =
  let bary route leg t1 t2 =
    let f = Xy.bary route.(leg - 1) route.(leg) in (f t1, f t2) in
  let lines = Array.map (fun _ -> []) acft in
  let t = fst acft.(0).dev.(0) in
  for i = 1 to Array.length acft - 1 do for j = 0 to i - 1 do
      let detect routei routej vsep last_t =
	detect_iter routei routej vsep last_t (fun legi legj t1 t2 ->
	  lines.(i) <- (j, bary routei legi t1 t2)::lines.(i);
	  lines.(j) <- (i, bary routej legj t1 t2)::lines.(j)) in
      let vsep = dspeed *. (acft.(i).speed +. acft.(j).speed) in
      detect acft.(i).dev acft.(j).dev vsep max_float;
      detect acft.(i).route acft.(j).route 0. t;
    done done;
  lines

(* Conflicts detection V2 --------------------------------------------------- *)

let cut route1 route2 last_t =
  let next route leg next_t =
    if fst route.(leg) = next_t then (leg + 1, snd route.(leg))
    else (leg, Xy.bary route.(leg - 1) route.(leg) next_t) in
  let l1 = Array.length route1 and l2 = Array.length route2 in
  let rec get (leg1, xy1) (leg2, xy2) t =
    if t < last_t && leg1 < l1 && leg2 < l2 then (
      let tt = min last_t (min (fst route1.(leg1)) (fst route2.(leg2))) in
      (t, xy1, xy2)::get (next route1 leg1 tt) (next route2 leg2 tt) tt)
    else [(t, xy1, xy2)] in
  let t = max (fst route1.(0)) (fst route2.(0)) in
  let leg_xy route =
    if fst route.(0) < t then (
      let leg = 1 + get_leg route t in 
      (leg, Xy.bary route.(leg - 1) route.(leg) t))
    else (1, snd route.(0)) in
  get (leg_xy route1) (leg_xy route2) t

let conf_iter route1 route2 first_t last_t vsep f =
  let rec iter = function
    | (t, orig1, orig2 as x1)::((tt, dest1, dest2 as x2)::_ as tl) ->
      let sep_t = sep +. vsep *. (t -. first_t) in
      let dsep = vsep *. (tt -. t) in
      List.iter (f x1 x2) (seg_detect orig1 dest1 orig2 dest2 sep_t dsep);
      iter tl
    | _ -> () in
  iter (cut route1 route2 last_t)

let detect2 acft =
  let first_t = fst acft.(0).predict.(0) in
  let times = Array.map (fun _ -> 0.) acft in
  for j = 1 to Array.length acft - 1 do for i = 0 to j - 1 do
      let vsep = dspeed *. (acft.(i).speed +. acft.(j).speed) in
      let rec conf_t = ref 0. in
      conf_iter acft.(i).dev acft.(j).dev first_t max_float vsep
	(fun (t, _, _) (tt, _, _) (t1, t2) ->
	  conf_t := !conf_t +. (tt -. t) *. (t2 -. t1));
      times.(i) <- times.(i) +. !conf_t;
      times.(j) <- times.(j) +. !conf_t;
    done done;
  times

let conf_segments2 acft =
  let bary_seg a1 a2 t1 t2 = (Xy.bary a1 a2 t1, Xy.bary a1 a2 t2) in
  let first_t = fst acft.(0).predict.(0) in
  let seg = Array.map (fun _ -> []) acft in
  for j = 1 to Array.length acft - 1 do for i = 0 to j - 1 do
      let ai = acft.(i) and aj = acft.(j) in
      let vsep = dspeed *. (ai.speed +. aj.speed) in
      let add_segs route1 route2 last_t =
	conf_iter route1 route2 first_t last_t vsep 
	  (fun (_, o1, o2) (_, d1, d2) (t1, t2) ->
	    seg.(i) <- (j, (bary_seg (0., o1) (1., d1) t1 t2))::seg.(i);
	    seg.(j) <- (i, (bary_seg (0., o2) (1., d2) t1 t2))::seg.(j)) in
      add_segs ai.dev aj.dev max_float;
      add_segs ai.route aj.route first_t;
    done done;
  seg

(* Random traffic situation ------------------------------------------------ *)

let first_conf_t acft i =
  let ai = acft.(i) and t = ref max_float in
  for j = 0 to i - 1 do
    let vsep = dspeed *. (ai.speed +. acft.(j).speed) in
    detect_iter ai.dev acft.(j).dev vsep max_float
      (fun _ _ t1 t2 -> t := min !t t1)
  done;
  !t

let random size n =
  let scramble alpha = alpha -. 0.5 +. Random.float 1. in
  let rnd_acft alpha =
    let xy1 = roundxy (Xy.mul (-.size) (Xy.polar alpha)) in
    let alpha2 = scramble alpha in
    let xy2 = roundxy (Xy.mul size (Xy.polar alpha2)) in
    let dist = Xy.norm (Xy.sub xy2 xy1) in
    let speed = max_speed *. (0.7 +. Random.float 0.3 -. dspeed) /. hour in
    let t = Random.float max_start_t in
    let route = [|(t, xy1); (t +. dist /. speed, xy2)|] in
    let a = new_acft speed in
    a.route <- route;
    set_time a 0.;
    a in
  let slices = max n (truncate (Xy.pi *. size /. sep /. 1.5)) in
  let s = float slices in
  let alpha = Array.init slices (fun i -> 2. *. float i *. Xy.pi /. s) in
  Array.sort (fun _ _ -> Random.int 3 - 1) alpha;
  let acft = Array.init n (fun i -> rnd_acft alpha.(i)) in
  Array.iteri (fun i _ ->
    while first_conf_t acft i < fst acft.(i).route.(0) +. min_conf_t do
      acft.(i) <- rnd_acft alpha.(i);
    done) 
    acft;
  acft

(* Save and load traffic file ---------------------------------------------- *)

let save filename acft =
  let file = open_out filename in
  let t = fst acft.(0).predict.(0) in
  Printf.fprintf file "%d %.4f\n" (Array.length acft) t;
  Array.iter (fun a -> 
    Printf.fprintf file "%.4f %s\n" a.speed (to_string a.route))
    acft;
  close_out file

let load filename =
  let file = open_in filename in
  Scanf.sscanf (input_line file) "%d %f" (fun n t ->
    let acft = Array.init n (fun _ ->
      Scanf.sscanf (input_line file) "%f %[^\n]" (fun speed route ->
	let a = new_acft speed in
	a.route <- of_string route;
	set_time a t;
	a)) in
    close_in file;
    acft)

