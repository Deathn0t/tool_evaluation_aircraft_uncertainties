let step = 5.  (* time steps (sec) for turns *)
let sep = 5.  (* separation (Nm) *)
let max_speed = 500.  (* max speed (knots) *)
let dspeed = 0.05  (* speed uncertainty *)
let alpha = 5.  (* angle discretisation (deg) *)
let max_turn = 30.  (* max turn on each point (deg) *)
let std_turn = 3.  (* standard turn (deg / sec) *)
let max_start_t = 300.  (* Maximal start time *)
let min_conf_t = 120.  (* Minimal time of first conflict *)

type time = float  (* sec *)
type speed = float  (* Nm / sec *)
type heading = float  (* radians *)
type xy = float * float  (* Nm x Nm *)
type t = (time * xy) array

let roundf x dx = dx *. floor (x /. dx +. 0.5)

let pi = atan2 0. (-1.)
let hour = 3600.
let vsep = 2. *. dspeed *. max_speed /. hour
let alpha_rad = alpha *. Xy.radians
let alpha_min = alpha_rad /. 2.
let max_turn_rad = max_turn *. Xy.radians
let turn_step = max_turn /. std_turn

let to_string route =
  String.concat " " (Array.to_list (Array.map (fun (t, (x, y)) ->
    Printf.sprintf "%.3f:%.3f,%.3f" t x y) route))

let of_string str =
  let rec get tl =
    try Scanf.sscanf tl "%f:%f,%f %[^\n]" (fun t x y tl -> (t, (x, y))::get tl)
    with _ -> [] in
  Array.of_list (get str)

let nav speed hdg t xys =
  let turn_dist = turn_step *. speed in
  let radius = speed *. 360. /. std_turn /. Xy.pi /. 2. in
  let rec get hdg t = function
    | xy1::xy2::tl ->
      let vect = Xy.sub xy2 xy1 in
      let dist= Xy.norm vect in
      if radius < dist then (
	let hdg2 = Xy.angle vect in
	let turn_a = Xy.sub_angle hdg2 hdg in
	let turn = if tl = [] then turn_a else roundf turn_a alpha_rad in
	if abs_float turn <= max_turn_rad then (
	  if tl <> [] then (
	    let a = Xy.sub_angle hdg2 (Xy.angle (Xy.sub (List.hd tl) xy1)) in
	    if alpha_min <= abs_float a then (
	      let new_hdg = Xy.add_angle hdg turn in
	      let xy = Xy.add xy1 (Xy.mul dist (Xy.polar new_hdg)) in
	      (t, xy1)::get new_hdg (t +. dist /. speed) (xy::tl))
	    else get hdg t (xy1::tl))
	  else [(t, xy1); (t +. dist /. speed, xy2)])
	else (
	  let a = if turn < 0. then -.max_turn_rad else max_turn_rad in
	  let new_hdg = Xy.add_angle hdg a in
	  let xy = Xy.add xy1 (Xy.mul turn_dist (Xy.polar new_hdg)) in
	  (t, xy1)::get new_hdg (t +. turn_step) (xy::xy2::tl)))
      else if tl = [] then [(t, xy1); (t +. dist /. speed, xy2)]
      else get hdg t (xy1::tl)
    | [xy] -> [(t, xy)]
    | [] -> [] in
  get hdg t xys

let create speed t = function
  | xy1::xy2::_ as xys ->
    Array.of_list (nav speed (Xy.angle (Xy.sub xy2 xy1)) t xys)
  | [xy] -> [|(t, xy)|]
  | [] -> [||]

let trace route = Array.map snd route

let t_start route = fst route.(0)

let t_end route = fst route.(Array.length route - 1)

let delay route speed =
  let last = Array.length route - 1 in
  let d = Xy.norm (Xy.sub (snd route.(last)) (snd route.(0))) in
  t_end route -. t_start route -. d /. speed

let get_leg route t =
  let rec dicho i j =
    let k = (i + j) / 2 in
    if k = i then i else if t <= fst route.(k) then dicho i k else dicho k j in
  let last_leg = Array.length route - 2 in
  if t < fst route.(1) then 0
  else if fst route.(last_leg) <= t then last_leg
  else dicho 0 (last_leg + 1)

let pos_heading route t =
  let leg = get_leg route t in
  let hdg = Xy.angle (Xy.sub (snd route.(leg + 1)) (snd route.(leg))) in
  (Xy.bary route.(leg) route.(leg + 1) t, hdg)

let comet route t =
  let tc = floor (t /. step) *. step in
  let leg = ref (get_leg route tc) in
  Array.init 5 (fun i ->
    let ti = tc -. step *. float i in
    while 0 < !leg && ti < fst route.(!leg) do decr leg done;
    Xy.bary route.(!leg) route.(!leg + 1) ti)

let update route dspeed speed t0 xy0 =
  let smin = speed *. (1. -. dspeed) and ds = 2. *. speed *. dspeed in
  let leg = get_leg route t0 + 1 in
  for i = leg to Array.length route - 1 do
    let (t, xy) = if i = leg then (t0, xy0) else route.(i - 1) in
    let speedi = if dspeed = 0. then speed else (smin +. Random.float ds) in
    let xyi = snd route.(i) in
    let ti = t +. Xy.norm (Xy.sub xyi xy) /. speedi in
    route.(i) <- (ti, xyi);
  done

let predict route speed t =
  let last = Array.length route - 1 in
  if t < fst route.(last) then (
    let leg = get_leg route t in
    let i = if fst route.(leg + 1) <= t then leg + 1 else leg in
    let pred = Array.sub route i (Array.length route - i) in
    if i = leg then pred.(0) <- (t, Xy.bary route.(leg) route.(leg + 1) t);
    update pred 0. speed t (snd pred.(0));
    pred)
  else [|(t, Xy.bary route.(last - 1) route.(last) t)|]

let join route leg hdg next =
  let xy = snd route.(leg) in
  let rec remove = function
    | (_, xy1)::tl when xy1 = xy -> tl
    | (_, xy1)::((_, xy2)::_ as tl) as txys ->
      let da = abs_float (Xy.sub_angle (Xy.angle (Xy.sub xy2 xy1)) hdg) in
      if alpha_min <= da then txys else remove tl
    | txys -> txys in
  Array.append (Array.sub route 0 (leg + 1)) (Array.of_list (remove next))

let rec get_leg_hdg_txy route t =
  let leg = get_leg route t in
  let hdg =  Xy.angle (Xy.sub (snd route.(leg + 1)) (snd route.(leg))) in
  let t_dev = max (fst route.(leg) +. turn_step) t in
  (leg, hdg, (t, Xy.bary route.(leg) route.(leg + 1) t_dev))

let dev_aux route speed (leg, hdg, (t, xy)) xy_dev =
  let next = nav speed hdg t [xy; xy_dev; snd route.(Array.length route - 1)] in
  join route leg hdg next

let dev route speed t0 xy_dev =
  dev_aux route speed (get_leg_hdg_txy route t0) xy_dev

let turn route speed t0 a t1 =
  let (leg, hdg, (t, xy) as leg_hdg_txy) = get_leg_hdg_txy route t0 in
  let half_a = a /. 2. in
  let d_turn = abs_float (sin half_a) *. speed /. 360. /. std_turn /. Xy.pi in
  let xy_turn = Xy.add xy (Xy.mul d_turn (Xy.polar (hdg +. half_a))) in
  let dt = abs_float a /. std_turn /. Xy.radians in
  let d_dev = speed *. (max 0. (t1 -. t0 -. dt)) in
  let xy_dev = Xy.add xy_turn (Xy.mul d_dev (Xy.polar (hdg +. a))) in
  dev_aux route speed leg_hdg_txy xy_dev

let apply route new_route speed t =
  let ta = max (fst route.(0)) t in
  let (leg, hdg, (t, xy)) = get_leg_hdg_txy route ta in
  let leg_dev = get_leg new_route t in
  let next = Array.sub new_route leg_dev (Array.length new_route - leg_dev) in
  next.(0) <- (t, xy);
  (* update next dspeed speed t xy; *)
  join route leg hdg (Array.to_list next)

(* Conflicts detection ----------------------------------------------------- *)

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
    let delta' = b ** 2. -.a *.c in
    if 0. < delta' then (
      let r = sqrt delta' in
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
  let x = Xy.sub orig2 orig1 in
  let v = Xy.sub (Xy.sub dest2 dest1) x in
  let a = Xy.norm2 v -.dsep ** 2. in
  roots2 a (Xy.sca x v -.sep *.dsep) (Xy.norm2 x -.sep ** 2.)

let detect route1 route2 t0 =
  let next_leg_xy route leg next_t =
    if fst route.(leg) = next_t then (leg + 1, snd route.(leg))
    else (leg, Xy.bary route.(leg - 1) route.(leg) next_t) in
  let rec get_times leg1 orig1 leg2 orig2 t =
    if leg1 < Array.length route1 && leg2 < Array.length route2 then (
      let next_t = min (fst route1.(leg1)) (fst route2.(leg2)) in
      let (next_leg1, dest1) = next_leg_xy route1 leg1 next_t in
      let (next_leg2, dest2) = next_leg_xy route2 leg2 next_t in
      let sep_t = sep +. vsep *. (t -.t0) in
      (* let dsep = sep +. vsep *. (next_t -. t0) -. sep_t in *)
      let dsep = vsep *. (next_t -. t) in
      let dt = next_t -. t in
      List.fold_left (fun times (t1, t2) ->
	(t +. t1 *.dt, t +. t2 *. dt)::times)
	(get_times next_leg1 dest1 next_leg2 dest2 next_t)
	(seg_detect orig1 dest1 orig2 dest2 sep_t dsep))
    else [] in
  let leg1 = get_leg route1 t0 in
  let orig1 = Xy.bary route1.(leg1) route1.(leg1 + 1) t0 in
  let leg2 = get_leg route2 t0 in
  let orig2 = Xy.bary route2.(leg2) route2.(leg2 + 1) t0 in
  get_times (leg1 + 1) orig1 (leg2 + 1) orig2 t0

let segments route1 route2 t0 =
  let segs1 = ref [] and segs2 = ref [] and leg1 = ref 1 and leg2 = ref 1 in
  let bary1 t = Xy.bary route1.(!leg1 - 1) route1.(!leg1) t in
  let bary2 t = Xy.bary route2.(!leg2 - 1) route2.(!leg2) t in
  List.iter (fun (t1, t2) ->
    while fst route1.(!leg1) < t2 do incr leg1 done;
    while fst route2.(!leg2) < t2 do incr leg2 done;
    segs1 := (bary1 t1, bary1 t2):: !segs1;
    segs2 := (bary2 t1, bary2 t2):: !segs2) (detect route1 route2 t0);
  (!segs1, !segs2)

let roundabout size n =
  let r = 0.9 *.size in
  let scramble x dx = x +.Random.float (2.*.dx) -.dx in
  let random_speed_route alpha =
    let u = Xy.polar alpha in
    let xy0 = Xy.add (Xy.mul (-.r) u) (scramble 0. sep, scramble 0. sep) in
    let xy1 = Xy.mul r (Xy.polar (scramble alpha 0.5)) in
    let dist = Xy.norm (Xy.sub xy1 xy0) in
    let speed = max_speed *. (0.7 +. Random.float 0.3 -. dspeed) /. hour in
    let t = Random.float max_start_t in
    (speed, [|(t, xy0); (t +. dist /. speed, xy1)|]) in
  let slices = max n (truncate (Xy.pi *. size /. sep /. 1.5)) in
  let s = float slices in
  let alpha = Array.init slices (fun i -> 2. *. float i *. Xy.pi /. s) in
  Array.sort (fun _ _ -> Random.int 3 - 1) alpha;
  let sr = Array.init n (fun i -> random_speed_route alpha.(i)) in
  let rec first_conf_t i j =
    if i < j then
      match detect (snd sr.(i)) (snd sr.(j)) (fst (snd sr.(i)).(0)) with
      | (t, _)::_ -> min t (first_conf_t (i + 1) j)
      | [] -> first_conf_t (i + 1) j
    else max_float in
  Array.iteri (fun i _ ->
    while first_conf_t 0 i < fst (snd sr.(i)).(0) +. min_conf_t do
      sr.(i) <- random_speed_route alpha.(i)
    done) sr;
  sr

let roundabout size n =
  let r = 0.9 *.size in
  let scramble x dx = x +.Random.float (2.*.dx) -.dx in
  let random_speed_route alpha =
    let u = Xy.polar alpha in
    let xy0 = Xy.add (Xy.mul (-.r) u) (scramble 0. sep, scramble 0. sep) in
    let xy1 = Xy.mul r (Xy.polar (scramble alpha 0.5)) in
    let dist = Xy.norm (Xy.sub xy1 xy0) in
    let speed = max_speed *. (0.7 +. Random.float 0.3 -. dspeed) /. hour in
    let t = Random.float max_start_t in
    (speed, [|(t, xy0); (t +. dist /. speed, xy1)|]) in
  let slices = max n (truncate (Xy.pi *. size /. sep /. 1.5)) in
  let s = float slices in
  let alpha = Array.init slices (fun i -> 2. *. float i *. Xy.pi /. s) in
  Array.sort (fun _ _ -> Random.int 3 - 1) alpha;
  let sr = Array.init n (fun i -> random_speed_route alpha.(i)) in
  let rec first_conf_t i j =
    if i < j then
      match detect (snd sr.(i)) (snd sr.(j)) (fst (snd sr.(i)).(0)) with
      | (t, _)::_ -> min t (first_conf_t (i + 1) j)
      | [] -> first_conf_t (i + 1) j
    else max_float in
  Array.iteri (fun i _ ->
    while first_conf_t 0 i < fst (snd sr.(i)).(0) +. min_conf_t do
      sr.(i) <- random_speed_route alpha.(i)
    done) sr;
  sr

let save filename t_spd_routes =
  let file = open_out filename in
  Printf.fprintf file "%d\n" (Array.length t_spd_routes);
  Array.iter (fun (tmin, speed, route) ->
    Printf.fprintf file "%.3f %.3f %s\n" tmin speed (to_string route))
    t_spd_routes;
  close_out file

let load filename =
  let file = open_in filename in
  Scanf.sscanf (input_line file) "%d" (fun n ->
    let t_speed_routes = Array.init n (fun _ ->
      Scanf.sscanf (input_line file) "%f %f %[^\n]" (fun tmin speed r ->
	(tmin, speed, of_string r))) in
    close_in file;
    t_speed_routes)
