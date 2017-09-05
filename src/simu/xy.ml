(* Float coords 2D --------------------------------------------------------- *)

type xy = float * float
      
let mul k (x, y) = (k *. x, k *. y)
let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let sca (x1, y1) (x2, y2) = x1 *. x2 +. y1 *. y2
let det (x1, y1) (x2, y2) = x1 *. y2 -. y1 *. x2

let norm2 v = sca v v
let norm v = sqrt (norm2 v)

let bary (t1, xy1) (t2, xy2) t =
  add xy1 (mul ((t -. t1) /.(t2 -. t1)) (sub xy2 xy1))

let pi = atan2 0. (-1.)
let twopi = 2. *. pi
let radians = pi /. 180.
let degrees = 180. /. pi

let angle (x, y) = atan2 y x

let polar a = (cos a, sin a)

let mod_twopi a =
  let b = mod_float a twopi in
  if pi <= b then (b -. twopi)
  else if b < -.pi then (b +. twopi)
  else b

let add_angle a1 a2 = mod_twopi (a1 +. a2)

let sub_angle a1 a2 = mod_twopi (a1 -. a2)

let dist_seg_seg (a, b) (c, d) =
  let dseg ap ab = (* distance (p, [ab]) *)
    let bp = sub ap ab in
    if sca ab ap < 0. then norm ap
    else if 0. < sca ab bp then norm bp
    else abs_float (det ab ap) /.norm ab in
  let ckw u v = (* orientation (u, v) *)
    let d = det u v in
    if 0. < d then 1 else if d < 0. then (-1) else (
      let uv = sca u v in
      if uv < 0. then (-1) else if uv <= sca u u then 0 else 1) in
  let ab = sub b a and ac = sub c a and ad = sub d a in
  let cd = sub d c and ca = sub a c and cb = sub b c in
  if ckw ab ac * ckw ab ad <= 0 && ckw cd ca * ckw cd cb <= 0 then 0.
  else min (min (dseg ca cd) (dseg cb cd)) (min (dseg ac ab) (dseg ad ab))

