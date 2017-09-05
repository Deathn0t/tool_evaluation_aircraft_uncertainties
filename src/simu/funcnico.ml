type config = {
    nb_ac: int; norm2: float; norm: float; norm_r2: float;vnorm: float; nb_steps: int; step: float;
    errspeed: float; err: float;
    t0max: int; errt0: float; t0low: float; t0delta: float;
    t1max: int; errt1: float; t1low: float; t1delta: float;
    amax: int; erra: float; alow: float; adelta: float;
    nb_fl: int; flmin: float; fldelta: float; crerr: float;
    nbman: int; run: int; tv:float;
    
  }

type xyz=float*float*float

type flight_plan=xyz list
(* t0 man start, t1 man end, a  man angle, n Flight Plan point to reach after man end*)      
type man={t0:float;t1:float;a:float;fl:float;n:int}
(* E0 when acft is not Maneuvered, Ec when it is maneuvering in heading, Ev vertically,  Eb for beacon points *)
type state= E0 | Ec | Ev | Eb

type curr={
    xyz:xyz; (* position *)
    cfl: float; (* target altitude*)
    s:float; (*speed*)
    vs: float; (*vertical speed*)
    h:float; (*current heading*)
    c:float; (*target heading*)
    e:state; (*horizontal state*)
    r:flight_plan (*remaining flight plan*)}
      
type incert={
  ber:int;(*0 if flyby nav, 1 if flyover nav*)
  ter0:float;(*time error in man start*)
  ter1:float;(*time error in man end*)
  ser:float;(*speed error*)
  crer:float;(*climbing/descending rate error*)  
  cer:float (*targeted angle error*) }
      
type route = {wpts: flight_plan; speed: float; cr: float}

let pi = acos (-1.)
let twopi = 2. *.pi
    
exception End_Nav 
let capture cfg vit =cfg.step*.vit*.(1.+. cfg.errspeed)*.(1.+. cfg.crerr) (*beacon capture distance*)
let capt=30./.pi/.tan (pi/.8.) (*mini angle for flyby nav =pi/8*)
    
let mul k (x, y,_) = (k *.x, k *.y)
let add (x1, y1,_) (x2, y2,_) = (x1 +.x2, y1 +.y2)
let sub (x1, y1,_) (x2, y2,_) = (x1 -.x2, y1 -.y2)
let sca (x1, y1) (x2, y2) = x1 *.x2 +.y1 *.y2
let det (x1, y1) (x2, y2) = x1 *.y2 -.y1 *.x2
    
let norm2 v = sca v v
let norm v = sqrt (norm2 v)
let dist a b = norm (sub a b)

(* initialise une config*)    
let init_cfg=
  let nb_ac = ref 2 and err = ref 3.
  and t0max = ref 3 and t0low = ref 0. and t0delta = ref 300.
  and t1max = ref 3 and t1low = ref 300. and t1delta = ref 600.
  and amax = ref 5 and alow = ref (-30.) and adelta = ref (60.)
  and nb_fl= ref 4 and flmin= ref (-20.) and fldelta= ref (40.)
  and vnorm= ref 8. in
  let nb_steps = ref 250 and step = ref 10. and norm = ref 5.
  and run = ref 0 in
  let args = Arg.align
      [ "-norm", Arg.Set_float norm,
	Printf.sprintf " Separation norm. Default: %.g NM" !norm;
	"-steps", Arg.Set_int nb_steps,
	Printf.sprintf " Number of simulation steps. Default: %d" !nb_steps;
	"-step", Arg.Set_float step,
	Printf.sprintf " Step size. Default: %g" !step;
	"-err", Arg.Set_float err,
	Printf.sprintf " Error coefficient. Default: %g" !err;
	"-d0",
	Arg.Tuple [ Arg.Set_int t0max; Arg.Set_float t0low;
		    Arg.Set_float t0delta ],
	Printf.sprintf
	  " Sets d0 manoeuvre (max, low, delta). Default: %d, %g, %g"
	  !t0max !t0low !t0delta;
	"-d1",
	Arg.Tuple [ Arg.Set_int t1max; Arg.Set_float t1low;
		    Arg.Set_float t1delta ],
	Printf.sprintf
	  " Sets d1 manoeuvre (max, low, delta). Default: %d, %g, %g"
	  !t1max !t1low !t1delta;
	"-a",
	Arg.Tuple [ Arg.Set_int amax; Arg.Set_float alow;
		    Arg.Set_float adelta ],
	Printf.sprintf
	  " Sets a manoeuvre (max, low, delta). Default: %d, %g, %g"
	  !amax !alow !adelta;
	"-fl",
	Arg.Tuple [ Arg.Set_int nb_fl; Arg.Set_float flmin;
		    Arg.Set_float fldelta ],
	Printf.sprintf
	  " Sets a manoeuvre (nb_fl, flmin, fldelta). Default: %d, %g, %g"
	  !nb_fl !flmin !fldelta;
	"-run", Arg.Set_int run, ""
      ] in
  let usage = Printf.sprintf "%s [options] nb_ac" Sys.argv.(0) in
  let anon = fun s -> nb_ac := int_of_string s in
  fun () ->
    Arg.parse args anon usage;
    Random.full_init [|!nb_ac; !run|];
    { nb_ac = !nb_ac; norm2 = !norm *. !norm; norm= !norm; norm_r2= !norm*.(sqrt 2.); vnorm= !vnorm;
      nb_steps = !nb_steps; step = !step; errspeed = 0.02 *. !err; err = !err;
      t0max = !t0max; errt0 = !err*.10.; t0low = !t0low; t0delta = !t0delta;
      t1max = !t1max; errt1 = !err*.10.; t1low = !t1low; t1delta = !t1delta;
      amax = !amax; erra = 1. *. !err; alow = !alow; adelta = !adelta;
      nbman = (!t0max + 1) * (!t1max + 1) * (!amax + 1 + !nb_fl); run = !run;
      nb_fl= !nb_fl;flmin= !flmin;fldelta= !fldelta;crerr= !err*.0.05;tv= !step*.pi/.60.
    }

(*initilalise des routes*)      
let init_routes cfg =
  Array.init
    cfg.nb_ac
    (fun i ->
      let rayon = 100. in
      let flmoyen= 280.+. (float (i mod 5))*.10. in
      let vit=0.13 in
      let vspeed=0.1 in
      let abruit = 1. *. ((Random.float 1.) -. 0.5) in
      let xbruit = 2. *. ((Random.float 10.) -. 5.) in
      let ybruit = 2. *. ((Random.float 10.) -. 5.) in
      let angle = ((float i) *. 2. *. pi) /. (float cfg.nb_ac) in
      let angle2 = abruit +. angle in
      let x = xbruit -. rayon *. (cos angle)
      and y = ybruit -. rayon *. (sin angle) in
      let rcosa2 = rayon *. cos angle2 and rsina2 = rayon *. sin angle2 in
      let wpts =
	[ (x,y,flmoyen);
(*	  (100.,0.,flmoyen);
	  (0.,100.,flmoyen); *)
	  (x +. 2. *. rcosa2,y +. 2. *. rsina2,flmoyen);
	  (x +. 4. *. rcosa2,y +. 4. *. rsina2,flmoyen)] in
      let speed = (0.8 +. Random.float 0.4)*.vit in
      let cr= (0.8 +. Random.float 0.4)*.vspeed in
      {wpts; speed;cr})

let pvect p1 p0 p2 =
  det (sub p1 p0) (sub p2 p0)

let pscal p1 p0 p2 =
  sca (sub p1 p0) (sub p2 p0)
    
(* is_inside=true si un point (x,y) est dans le convexe l qui est decrit dans
   le sens des aiguilles d'une montre*)
let is_inside a l debug =
  match l with
  | [] -> false
  | hd :: _ ->
    let rec is_in a l =
      match l with
      | b :: ((c :: _) as ctl) ->
	  pvect b c a > 0. && is_in a ctl
      | [b] -> pvect b hd a > 0.
      | _ -> failwith "is_inside: unreachable" in
    let res=is_in a l in
    if debug && res then raise Exit;
    res

let equal_points (pax,pay,_) (pbx,pby,_) = pax = pbx && pay = pby

(*    
let dist2 a b =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  dx *. dx +. dy *. dy
 *)

(* met chaque convexe dans un octogone *)
	
let minmax cfg tab =
  Array.init cfg.nb_ac 
    (fun i -> 
      let tabi=tab.(i) in
      Array.init cfg.nb_steps 
	(fun t ->
	  let (tabit,mi,ma)=tabi.(t) in
(*	      let zmin=ref infinity and zmax=ref neg_infinity in *)
	  let xmin=ref infinity and xmax=ref neg_infinity
	  and ymin=ref infinity and ymax=ref neg_infinity
	  and xpymin=ref infinity and xpymax=ref neg_infinity
	  and xmymin=ref infinity and xmymax=ref neg_infinity
	  in
	  List.iter (fun (x,y,z) ->
	    let xpy=x+.y and xmy=x-.y in
(*		zmin:=min !zmin z;zmax:=max !zmax z; *)
	    xmin:=min !xmin x;ymin:=min !ymin y;
	    xmax:=max !xmax x;ymax:=max !ymax y;
	    xpymin:=min !xpymin xpy;xmymin:=min !xmymin xmy;
	    xpymax:=max !xpymax xpy;xmymax:=max !xmymax xmy
		    ) tabit;
	  ((mi,ma),(!xmin, !xmax),(!ymin, !ymax),(!xpymin, !xpymax),(!xmymin, !xmymax))))
  
(* Récupère la liste des pas de temps pour lesquels il peut y avoir conflit *)
    
let prefiltre cfg tab1 tab2=
  let l=ref [] in
  for t=0 to cfg.nb_steps-1 do
    let ((z1min,z1max),(x1min,x1max),(y1min,y1max),(xpy1min,xpy1max),(xmy1min,xmy1max))=tab1.(t)
    and ((z2min,z2max),(x2min,x2max),(y2min,y2max),(xpy2min,xpy2max),(xmy2min,xmy2max))=tab2.(t) in
    if z1min <=z2max+.cfg.vnorm && z2min <=z1max+.cfg.vnorm 
	&& (x1min <= x2max +. cfg.norm)  &&  (x2min <= x1max +. cfg.norm) 
	&& (y1min <= y2max +. cfg.norm) && (y2min <= y1max +. cfg.norm)
	&& (xpy1min <= xpy2max +. cfg.norm_r2)  &&  (xpy2min <= xpy1max +. cfg.norm_r2) 
	&& (xmy1min <= xmy2max +. cfg.norm_r2)  &&  (xmy2min <= xmy1max +. cfg.norm_r2) 
    then l:= t:: !l
  done;
  !l

    
let dist2 a b=
  norm2 (sub a b)
    
(* distance point c au segment ab*)
let conflict_seg_pt norm2 a b c debug=
  let d2 =
    if equal_points a b || pscal b a c <= 0. then dist2 a c
    else if pscal a b c <= 0. then dist2 b c
    else let abc = pvect a b c in  (abc *. abc) /. dist2 a b in
  d2 < norm2

(* conflict entre le segment ab et le segment cd*)
let conflict_seg_seg norm2 a b c d debug=
  (((pvect b a c) *. (pvect b a d) < 0.) &&
   ((pvect d c a) *. (pvect d c b) < 0.)) ||
   conflict_seg_pt norm2 a b c debug || conflict_seg_pt norm2 a b d debug||
   conflict_seg_pt norm2 c d a debug || conflict_seg_pt norm2 c d b debug

let exist_couples norm2 l1 l2 debug=
  match l1, l2 with
  | [], _ | _, [] -> false
  | z1 :: _, z2 :: _ ->
      let rec loop2 a1 b1 l2 =
	match l2 with
	| [] -> failwith "exist_couples: loop2"
	| [a2] -> conflict_seg_seg norm2 a1 b1 a2 z2 debug
	| a2 :: ((b2 :: _) as r2) ->
	    conflict_seg_seg norm2 a1 b1 a2 b2 debug || loop2 a1 b1 r2 in
      let rec loop1 l1 =
	match l1 with
	| [] -> failwith "exist_couples: loop1"
	| [a1] -> loop2 a1 z1 l2
	| a1 :: ((b1 :: _) as r1) -> loop2 a1 b1 l2 || loop1 r1 in
      loop1 l1

(* conflit entre deux listes reprÃ©sentant deux convexes*)
let all_pairs norm2 l1 l2 debug =
  List.exists (fun a -> is_inside a l2 debug) l1 ||
  List.exists (fun a -> is_inside a l1 debug) l2 ||
  exist_couples norm2 l1 l2 debug

(*fonction detection de conflit*) 
let conflict cfg tab1 tab2 mmtab1 mmtab2 debug=
  let rec loop l =
    match l with
      [] -> false
    | i::tl ->
	let (l1,_,_)=tab1.(i) and (l2,_,_)=tab2.(i) in
	all_pairs cfg.norm2 l1 l2 debug|| loop tl in
  let l= prefiltre cfg mmtab1 mmtab2 in
  if debug then (List.iter (fun t -> Printf.printf "%d " t) l;Printf.printf "\n%!");
  let res = loop l in
  res

(* calcul de l'enveloppe convexe*)

let egalxy (x1,y1,_) (x2,y2,_)=
  x1=x2 && y1=y2
    
let pvect2  p1 p0 p2= 
  let (x1,y1,_)=p1.xyz and (x0,y0,_)=p0.xyz and (x2,y2,_)=p2.xyz in 
  (x1-.x0)*.(y2-.y0)-.(x2-.x0)*.(y1-.y0) 
    

let enveloppe xy_list =
  match xy_list with
  | [] -> []
  | [p] -> [p.xyz]
  | _ ->
    let pts =
      let pts = List.sort
          (fun p q ->
	    let (px,py,_)=p.xyz and (qx,qy,_)=q.xyz in
            if px=qx
             then compare py qy
             else compare px qx
          ) xy_list
      in
      let rec unicise l =
        match l with
        | [] -> []
        | [a] -> [a.xyz]
        | a :: (b :: _ as lf)->
            if egalxy a.xyz b.xyz
            then unicise lf
            else a.xyz::(unicise lf)
      in
      unicise pts
    in
    let chain l =
      let rec aux e l =
        match e , l with
        | conv, [] -> conv
        | [], p::lf
        | [_], p::lf -> aux (p :: e) lf
        | a :: (b :: _ as conv) , p :: lf ->
          if pvect b a p >= 0.
          then aux conv l
          else aux (p :: a :: conv) lf
      in
      aux [] l
    in
    match pts with
    | [] -> failwith "enveloppe: must not happen"
    | [p] -> [p]
    | _ ->
      let lower = chain pts in
      let upper = chain (List.rev pts) in
      (List.tl upper) @ (List.tl lower)

(*normalizes h in [-pi pi]*)
let rec normal h =
  if h > pi then normal (h-.2.*.pi)
  else if h +. pi <= 0. then normal (h+.2.*.pi)
  else h

(*converts int n into an incertainty*)     
let convert cfg n =
  {ber=n land 1;
   ter0=cfg.errt0*. (float ((n lsr 1) land 1));
   ter1=cfg.errt1*. (float ((n lsr 2) land 1));
   ser=cfg.errspeed*. (float (2*((n lsr 3) land 1)-1));
   cer=cfg.erra*. (float (2*((n lsr 4) land 1)-1));
   crer=cfg.crerr*. (float (2*((n lsr 5) land 1)-1))}

(*new angle converging toward target*)    
let converge cfg cap cible =
  let res=
    let cap=normal cap and cible=normal cible in
    if cap<cible then
      if cible-.cap <pi  then min (cap+.cfg.tv) cible
      else min (cap-.cfg.tv) cible
    else 
      if cap-.cible <pi  then max (cap-.cfg.tv) cible
      else max (cap+.cfg.tv) cible in
  normal res


(* returns new heading, new route, and new target for flyby navigation *)   
let new_cap_flyby cfg e (x,y,z) s r h c =
  match e with
    Eb -> failwith "new_cap d'une balise";
  | Ec -> (converge cfg h c,r,c) 
  | E0 | Ev -> 
      match r with
      | [] -> raise End_Nav
      | (px,py,pz)::[] ->       
	  if (dist (x,y,z) (px,py,pz) <capture cfg s) then (h,[],c)
	  else
	    let c = atan2 (py -. y) (px -. x) in (converge cfg h c,r,c)
      | (px,py,pz) :: (qx,qy,qz) :: tl ->
	  let nc = atan2 (qy -. py) (qx -. px) in
	  let diff = abs_float (nc -.h) in
	  let diff = min diff (twopi-.diff) in
	  let dis=dist (x,y,z) (px,py,pz) in
	  if  (dis-.s*.cfg.step)/.s> capt ||
	  (dis-.s*.cfg.step)/.s> 30./.pi/.tan ((pi-.diff)/.2.) then 
	    let c=atan2 (py -. y) (px -. x) in  
	    (converge cfg h c,r,c)
	  else 
	    begin
	      let nr= (qx,qy,qz)::tl in
	      (converge cfg h c, nr,nc) 
	    end

(* returns new heading, new route, and new target for flyover navigation *)
let new_flyover cfg  e (x,y,z) s r h c =
  match e with
    Eb -> failwith "new_cap d'une balise";
  | Ec -> (converge cfg h c,r,c) 
  | E0 | Ev -> 
      match r with
      | [] -> raise End_Nav
      | (px,py,pz)::[] ->       
	  if (dist (x,y,z) (px,py,pz) <capture cfg s) then (h,[],c)
	  else
	    let c = atan2 (py -. y) (px -. x) in (converge cfg h c,r,c)
      | (px,py,pz) :: (qx,qy,qz) :: tl ->
	  let c=atan2 (py -. y) (px -. x) in  
	  let dis=dist (x,y,z) (px,py,pz) in
	  if dis > capture cfg s then
	    (converge cfg h c,r,c)
	  else 
	    begin
	      let nc = atan2 (qy -. py) (qx -. px) in
	      let nr= (qx,qy,qz)::tl in
	      (converge cfg h c, nr,nc) 
	    end

(*calculates the targeted beacon according to n in the maneuver*)
let rec newr r n =
  if r=[] then failwith "plus de point"
  else if n=0 then r 
  else newr (List.tl r) (n-1)

(* move of 1 step flyby*)
let move_flyby cfg p ser cer crer=
  let {xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl;e=e;r=r} =p in
  let ns= if cfl>z then s*.(1.-.vs) else if cfl<z then s*.(1.+.vs) else s in
  let (nh,nr,nc)=new_cap_flyby cfg e (x,y,z) (ns*.(1.+.ser)) r h c in
  let x0 = x +. ns*.(1.+.ser)*.cfg.step *. (cos nh) and y0 = y +. ns*.(1.+.ser)*.cfg.step *. (sin nh)
  and z0=
    if cfl>z then min cfl (z +. vs*.(1.+.crer)*.cfg.step)
    else if cfl<z then max cfl (z -. vs*.(1.+.crer)*.cfg.step)
    else z in
  Some {xyz=(x0,y0,z0);s=s;vs=vs;h=nh;e=e;c=nc;cfl=cfl;r=nr}
    
(* move of 1 step flyover beacon  *)
let move_flyover cfg p ser cer crer=
  let {xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl;e=e;r=r} =p in
  let ns= if cfl>z then s*.(1.-.vs) else if cfl<z then s*.(1.+.vs) else s in
  let (nh,nr,nc)=new_flyover cfg e (x,y,z) (ns*.(1.+.ser)) r h c in
  let x0 = x +. ns*.(1.+.ser)*.cfg.step *. (cos nh) and y0 = y +. ns*.(1.+.ser)*.cfg.step *. (sin nh)
  and z0=
    if cfl>z then min cfl (z +. vs*.(1.+.crer)*.cfg.step)
    else if cfl<z then max cfl (z -. vs*.(1.+.crer)*.cfg.step)
    else z in
  Some {xyz=(x0,y0,z0);s=s;vs=vs;h=nh;e=e;c=nc;cfl=cfl;r=nr}
    
(* move point p according to man for 1 step with err from reference heading capevit and next route rmod after maneuver*)    
let movepoint cfg p err man t capevit rmod=
  match p with
    None -> None
  | Some p ->
      let {xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl;e=e;r=r} =p
      and {ber=ber;ter0=ter0;ter1=ter1;ser=ser;cer=cer;crer=crer}=err in
      let move= if ber=0 then move_flyby cfg else move_flyover cfg in
      try match e with
	Eb -> Some p
      | E0 ->
	  if (if ter1 = 0. then t +.cfg.step else t) > man.t1 +. ter1 then
	    move p ser 0. crer
	  else
	    if (if ter0 = 0. then t +. cfg.step else t) <= man.t0+.ter0 then
	      move p ser 0. crer
	    else
	      if man.fl <> 0. then 
		let newp={xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl+.man.fl;e=Ev;r=r} in
		move newp ser cer crer
	      else
		let cap = capevit +. (man.a+.cer)*.pi/.180. in
		let newp={xyz=(x,y,z);s=s;vs=vs;h=h;c=cap;cfl=cfl;e=Ec;r=rmod} in
		move newp ser cer crer 
      | Ev ->
	  if (if ter1 = 0. then t +. cfg.step else t) > man.t1 +. ter1 then
	    let newp={xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl-.man.fl;e=E0;r=r} in
	    move newp ser 0. crer;
	  else
	    move p ser cer crer
      | Ec ->
	  if (if ter1 = 0. then t +. cfg.step else t) > man.t1 +. ter1 then 
	    let newp={xyz=(x,y,z);s=s;vs=vs;h=h;c=c;cfl=cfl;e=E0;r=rmod} in
	    move newp ser 0. crer;
	  else
	    move p ser cer crer
      with End_Nav -> None
      
(*extracts k elements from a list l*)	
let extract l k =
  let rec rextract l k res=
    if k<=0 then res 
    else 
      match l with 
	[] -> res
      | hd::tl -> rextract tl (k-1) ({xyz=hd;s=0.;vs=0.;h=0.;e=Eb;cfl=0.;c=0.;r=[]}::res) in
  rextract l k []

(*adds beacons to a set of points needed*)
	
let addbeacons points =
  let rec scan (nlargest,largestwpts,nsmallest) pts =
    match pts with
      [] ->
      if nlargest = min_int
      then points
      else
        let res = extract largestwpts (nlargest-nsmallest) in
        res @ points
    | p :: ptsf ->
      if p.e = E0
      then
        let n = List.length p.r in
        if n < nsmallest
        then scan (nlargest,largestwpts,n) ptsf
        else if n > nlargest
        then scan (n,p.r,nsmallest) ptsf
        else scan (nlargest,largestwpts,nsmallest) ptsf
      else scan (nlargest,largestwpts,nsmallest) ptsf
  in
  scan (min_int,[],max_int) points

(*calculates the reference heading for maneuver*)
let rec func_capevit route vmax time=
  match route with 
    [] | [_] -> failwith "BUG capevit"
  | (x1,y1,z1)::(x2,y2,z2)::remroute->
      let d=dist (x1,y1,z1) (x2,y2,z2) in
      let dis=vmax*.time in 
      if d<dis then func_capevit ((x2,y2,z2)::remroute) vmax ((dis-.d)/.vmax)
      else atan2 (y2 -. y1) (x2 -. x1)

(* creates a tab of points navigating on a route with a maneuver and errors*)	  
let navigate cfg route man err=
  match route.wpts with
  | [] | [_] -> failwith "navigate: empty route"
  | (x0,y0,z0) :: ((x1,y1,z1) :: tl) ->
      let cap = atan2 (y1 -. y0) (x1 -. x0) in 
(*      let cap = heading in *)
      let tab = Array.init cfg.nb_steps (fun i -> None) in
      let e = if man.t1 < cfg.step then Ec else E0 in
      tab.(0) <- Some {xyz=(x0,y0,z0);s=route.speed;vs=route.cr;h=cap;e=e;c=cap;cfl=z0;r=(x1,y1,z1)::tl};
      (*
	let capevit=func_capevit route.wpts (route.speed*.(1.+.cfg.errspeed)*.(1.+.route.cr)) (man.t0+.cfg.errt0) in
       *)
      let capevit=0. in
      let rmod=newr route.wpts man.n in
      let t=ref 0. in
      let rec nav cfg i err man t capevit rmod=
	if tab.(i)<> None then
	  if i=cfg.nb_steps-2 then tab.(i+1) <- movepoint cfg tab.(i) err man t capevit rmod
	  else
	    begin
	      tab.(i+1)<-movepoint cfg tab.(i) err man t capevit rmod;
	      nav cfg (i+1) err man (t+.cfg.step) capevit rmod 
	    end in
      nav cfg 0 err man 0. capevit rmod;
(*      
	for i = 0 to cfg.nb_steps - 2 do
	t:= !t+.cfg.step;
	let p = movepoint cfg tab.(i) err man !t capevit rmod in
	tab.(i+1)<-p
	done; *)
      tab
	
(* transforme un nombre compris entre 0 et cfg.nbman en une manoeuvre*)
let i2tad cfg =
  fun x ->
    if x = cfg.nbman then (cfg.t0max, 0, (cfg.amax + 1) / 2, cfg.nb_fl/2,cfg.amax+cfg.nb_fl+1) else 
    let ad1max = (cfg.amax + 1+ cfg.nb_fl) * (cfg.t1max + 1) in
    let d0 = x / ad1max in
    let x = x mod ad1max in
    let d1 = (x / (cfg.amax + 1 + cfg.nb_fl)) + 1 in
    let apfl = x mod (cfg.amax + 1 + cfg.nb_fl) in
    let dif= apfl - cfg.amax - 1 in
    let fl=
      if dif>=0 then
	if dif < cfg.nb_fl/2 then dif
	else dif+1
      else cfg.nb_fl/2 in
    let a =
      if dif<0 then
	if apfl <= cfg.amax / 2 then apfl
	else apfl + 1
      else (cfg.amax+1)/2 in
    (d0,d1,a,fl,apfl)

(* initialise les trajectoires avec une manoeuvre aleatoire*)
    
let init_trajs cfg routes =
  Array.mapi
    (fun i routei ->
      let j=Random.int (cfg.nbman + 1) in
      let d0, d1, a ,fl ,apfl= i2tad cfg j in
      let t0 =
	((float d0) *. cfg.t0delta /. (float cfg.t0max)) +.cfg.t0low in
      let t1 =
	t0+. ((float d1) *. cfg.t1delta /. (float (cfg.t1max + 1))) +. cfg.t1low
      and a =
	((float a) *. cfg.adelta /. (float (cfg.amax + 1))) +. cfg.alow
      and fl = cfg.flmin+.
	  ((float fl) *. cfg.fldelta /. (float cfg.nb_fl))
      in
      let man = {t0=t0;t1=t1;a=a;fl=fl;n=2} in
	 Printf.printf "j=%d t0=%f t1=%f a=%f fl=%f\n" j t0 t1 a fl;
      let tabs = Array.init 64 (fun i -> 
	let err = convert cfg i in
(*	let (x0,y0,_)=List.hd routei.wpts
	and (x1,y1,_)=List.hd (List.rev routei.wpts) in
	let cap = atan2 (y1 -. y0) (x1 -. x0) in *)
	navigate cfg routei man err) in
      let multitab = Array.init cfg.nb_steps (fun i ->
	let (pts, mi, ma) = Array.fold_left (fun (l, mi, ma) t ->
	  match t.(i) with
	    None -> (l,mi,ma)
	  | Some p ->
	      let (_, _, z) = p.xyz in
	      (p::l, min mi z, max ma z)) ([], infinity, neg_infinity) tabs in
	(enveloppe (addbeacons pts), mi, ma)) in
      let traj_ij = multitab in
      traj_ij)
    routes

(* imprime les trajectoires dans un fichier*)    
let print_trajs filename trajs =
  let ch_man = open_out filename in
  Array.iteri
    (fun i traji ->
      Array.iteri
	(fun t (l,mi,ma) ->
	  Printf.fprintf ch_man "%d %d %1.2f %1.2f" i t mi ma;
	  List.iter
	    (fun (px,py,_)-> Printf.fprintf ch_man " %1.2f %1.2f" px py) l;
	  Printf.fprintf ch_man "\n%!")
	traji)
    trajs;
  close_out ch_man

(* detecte les conflits entre toutes les paires d'avions*)
let detect_conflicts cfg trajs minmaxtrajs =
  for i=0 to cfg.nb_ac -1 do
    for j=0 to i-1 do
      let c= conflict cfg trajs.(i) trajs.(j) minmaxtrajs.(i) minmaxtrajs.(j) false in
      let s=if c then "en conflit" else "pas de conflit" in
      Printf.printf "%d %d %s\n" i j s
    done
  done
    
(* programme principal*)
(*
let _ =
  let cfg=init_cfg () in
  let routes=init_routes cfg in
  let trajs=init_trajs cfg routes in
  let minmaxtrajs= minmax cfg trajs in
  print_trajs "trajs.txt" trajs;
  detect_conflicts cfg trajs minmaxtrajs
*)
