let step = 5.  (* time steps (sec) *)
let alpha = 5.  (* angle discretisation (deg) *)
let std_turn = 3.  (* standard turn (deg / sec) *)

type time = float  (* sec *)
type speed = float  (* knots *)
type xy = float * float  (* Nm x Nm *)
type t = {
  speed: float;
  route: (time * xy) array;
}

let pi = atan2 0. (-1.)
let hour = 3600.

(* Trajectories ------------------------------------------------------------ *)

let roundf x dx = dx *. floor (x /. dx +. 0.5)

let create_route ?(dspeed = 0.) speed heading t xys =
  let speed_sec = speed /. hour in
  let get_speed () =
    if dspeed = 0. then speed_sec
    else (speed_sec *. (1. +. Random.float (2. *. dspeed) -. dspeed)) in
  let alpha_rad = alpha *. Xy.radians in
  let max_turn = roundf (step *. std_turn *. Xy.radians) alpha_rad in
  let radius = speed_sec *. 360. /. std_turn /. Xy.pi /. 2. in
  let rec get (t, xy as txy) hdg = function
    | next_xy::tl as xys ->
      let vect = Xy.sub next_xy xy in
      let dist = Xy.norm vect in
      if radius < dist then (
	let target = Xy.angle vect in
	let turn = Xy.sub_angle target hdg in
	let new_speed = get_speed () in
	let (new_hdg, dt, next_xys) =
	  if max_turn < abs_float turn then (
	    Xy.sub_angle hdg (if turn < 0. then max_turn else -.max_turn),
	    step,
	    xys)
	  else if tl <> [] then (
	    Xy.sub_angle hdg (-.roundf turn alpha_rad),
	    roundf (dist /. new_speed) step,
	    tl)
	  else (target, roundf (dist /. new_speed) step, []) in
	let new_xy = Xy.add xy (Xy.mul (dt *. new_speed) (Xy.polar new_hdg)) in
	txy::get (t +.dt, new_xy) new_hdg next_xys)
      else get txy hdg tl
    | [] -> [txy] in
  Array.of_list (get (t, List.hd xys) heading (List.tl xys))

let create ?(dspeed = 0.) speed t = function
  | xy1::xy2::_ as xys ->
    let hdg = Xy.angle (Xy.sub xy2 xy1) in
    {speed = speed; route = create_route ~dspeed:dspeed speed hdg t xys}
  | _ -> failwith "create: not enaugh xys"

let speed tp = tp.speed

let trace tp = Array.map snd tp.route

let start_t tp = fst tp.route.(0)

let end_t tp = fst tp.route.(Array.length tp.route - 1)

let delay tp =
  let (t0, xy0) = tp.route.(0) in
  let (te, xye) = tp.route.(Array.length tp.route - 1) in
  te -. t0 -. Xy.norm (Xy.sub xye xy0) *. hour /. tp.speed

let get_leg tp t =
  let rec dicho i j =
    let k = (i + j) / 2 in
    if k = i then i 
    else if t <= fst tp.route.(k) then dicho i k 
    else dicho k j in
  let last_leg = Array.length tp.route - 2 in
  if t < fst tp.route.(0) then 0
  else if fst tp.route.(last_leg) <= t then last_leg
  else dicho 0 (last_leg + 1)

let leg_xy_hdg tp t =
  let leg = get_leg tp t in
  let xy = Xy.bary tp.route.(leg) tp.route.(leg + 1) t in
  let hdg = Xy.angle (Xy.sub (snd tp.route.(leg + 1)) (snd tp.route.(leg))) in
  (leg, xy, hdg)

let dev_aux tp (leg, xy, hdg) t xy_dev =
  let last = Array.length tp.route - 1 in
  let next = create_route tp.speed hdg t [xy; xy_dev; snd tp.route.(last)] in
  let route =
    if t = fst tp.route.(0) then next
    else Array.append (Array.sub tp.route 0 (leg + 1)) next in
  {speed = tp.speed; route = route}

let turn tp t0 a t1 =
  let (_, xy, hdg as lxyh) = leg_xy_hdg tp t0 in
  let dist = (t1 -. t0) *. tp.speed /. hour in
  dev_aux tp lxyh t0 (Xy.add xy (Xy.mul dist (Xy.polar (hdg +. a))))

let dev tp t xy_dev =
  dev_aux tp (leg_xy_hdg tp t) t xy_dev
   
let split tp t =
  let (leg, xy, hdg) = leg_xy_hdg tp t in
  let n = Array.length tp.route - leg - 1 in
  let xys = Array.to_list (Array.map snd (Array.sub tp.route (leg + 1) n)) in
  create tp.speed t (xy::xys)

let merge ?(dspeed = 0.) tp1 tp2 t =
  let (leg1, xy, hdg) = leg_xy_hdg tp1 t in
  let leg2 = get_leg tp2 t in
  let n = Array.length tp2.route - leg2 - 1 in
  let xys = Array.to_list (Array.map snd (Array.sub tp2.route (leg2 + 1) n)) in
  let next = create_route ~dspeed:dspeed tp1.speed hdg t (xy::xys) in
  let route = 
    if t = fst tp1.route.(0) then next
    else Array.append (Array.sub tp1.route 0 (leg1 + 1)) next in
  {speed = tp1.speed; route = route}

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

(*
let pred_detect acft1 acft2 =
  let first_t = ref max_float and segs1 = ref [] and segs2 = ref [] in
  let vsep = dspeed *.(acft1.speed +.acft2.speed) /. hour in
  let pred1 = acft1.pred and pred2 = acft2.pred in
  let rec detect leg1 orig1 leg2 orig2 t sep_t =
    if leg1 < Array.length pred1 && leg2 < Array.length pred2 then (
      let t1 = fst pred1.(leg1) and t2 = fst pred2.(leg2) in
      let next_t = min t1 t2 in
      let (dest1, next_leg1) =
	if t1 = next_t then (snd pred1.(leg1), leg1 + 1) 
	else (Xy.bary pred1.(leg1 - 1) pred1.(leg1) next_t, leg1) in
      let (dest2, next_leg2) =
	if t2 = next_t then (snd pred2.(leg2), leg2 + 1) 
	else (Xy.bary pred2.(leg2 - 1) pred2.(leg2) next_t, leg2) in
      let next_sep = sep +.vsep *.(next_t -.fst pred1.(0)) in
      detect next_leg1 dest1 next_leg2 dest2 next_t next_sep;
      let bary1 = Xy.bary (0., orig1) (1., dest1) in
      let bary2 = Xy.bary (0., orig2) (1., dest2) in
      List.iter (fun (t1, t2) ->
	let seg1 = (bary1 t1, bary1 t2) and seg2 = (bary2 t1, bary2 t2) in
	first_t := min !first_t (t +. t1 *.(next_t -.t) -.fst pred1.(0));
	segs1 := seg1:: !segs1;
	segs2 := seg2:: !segs2)
	(seg_detect orig1 dest1 orig2 dest2 sep_t (next_sep -.sep_t))) in
  if fst pred1.(0) <> fst pred2.(0) then
    Printf.printf "pred_detect: %f <> %f\n%!" (fst pred1.(0)) (fst pred2.(0));
  detect 1 (snd pred1.(0)) 1 (snd (pred2.(0))) (fst pred1.(0)) sep;
  (!first_t, (!segs1, !segs2))

val detect: t -> t -> time * time
val seg_detect: t -> t -> (xy list list) * (xy list list)

val to_string: t -> string
val of_string: string -> t

*)


