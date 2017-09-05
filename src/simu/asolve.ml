let lag = 30.
let man_cost = 30

type problem = {
  routes: Acft.route array array;        (* id -> [|route|] *)
  costs: int array array;                (* id, route -> delay *)
  couples: bool array array array array; (* id1, id2, route1, route2 -> bool *)
  heuristic: int array array;            (* id, r -> h *)
}

let get_problem times dtimes angles acft =
  let n = Array.length acft in
  let epsilon = 2. *. Xy.radians in
  let costs = Array.map (fun _ -> [||]) acft in
  let n_turned = ref 0 in
  let routes = 
    Array.mapi (fun i a ->
      let route = Acft.predict a in
      let length = Array.length route in
      let t0_min = max (fst (Acft.route a).(0) +. lag) times.(0) in
      let t0_max = times.(Array.length times - 1) in
      let turned = 
	2 < length && (
	  let h1 = Xy.angle (Xy.sub (snd route.(1)) (snd route.(0))) in
	  let h2 = Xy.angle (Xy.sub (snd route.(length - 1)) (snd route.(0))) in
	  epsilon < abs_float (Xy.sub_angle h1 h2)
	  || fst (Acft.predict a).(1) < fst (Acft.predict a).(0) +. lag) in
      if turned then incr n_turned;
      if not turned then (
	let t = fst (Acft.predict a).(1) in
	Acft.turn a t 0. t;
	Acft.apply a);
      let man = ref [] in
      let cost = ref [] in
      Array.iter (fun alpha ->
	if turned || alpha = 0. then (
	  Acft.turn a t0_min 0. t0_min;
	  cost := 0:: !cost;
	  man := Array.copy (Acft.dev a):: !man);
	if turned then (
	  Array.iter (fun dt ->
	    let t1 = times.(0) +. dt in
	    if t0_min < t1 then (
	      Acft.turn a t1 0. t1;
	      cost := truncate (Acft.delay a):: !cost;
	      man := Array.copy (Acft.dev a):: !man)) dtimes)
	else if alpha <> 0. then (
	  Array.iteri (fun i0 t0 ->
	    if t0_min <= t0 then (
	      Array.iter (fun dt ->
		let t1 = t0 +. dt in
		Acft.turn a t0 alpha t1;
		let delay = truncate (Acft.delay a) in
		if delay = 0 then 
		  Printf.printf "*** delay=0 t0=%.1f t1=%.1f alpha=%.1f\n%!"
		    t0 t1 alpha;
		let cost_t0 = truncate (t0_max -. t0) in
		let c = delay + cost_t0 + man_cost in
		cost := c:: !cost;
		man := Array.copy (Acft.dev a):: !man) dtimes)) times)) angles;
      costs.(i) <- Array.of_list !cost;
      Array.of_list !man) acft in
  Printf.printf "%d acft turning\n%!" !n_turned;
  let cpt = ref 0 in
  let couples =
    Array.init n (fun i -> 
      Array.init i (fun j ->
	Array.map (fun route_i -> 
	  Acft.transform acft.(i) route_i 1.;
	  Array.map (fun route_j -> 
	    incr cpt;
	    Acft.transform acft.(j) route_j 1.;
	    Acft.detect [|acft.(i); acft.(j)|] = [|0.;0.|])
	    routes.(j))
	  routes.(i))) in
  let heur = Array.init n (fun i -> Array.map (fun _ -> 0) routes.(i)) in
  Printf.printf "%d couples\n%!" !cpt;
  {routes=routes; costs=costs; couples=couples; heuristic=heur}

let set_heuristic problem =
  let n = Array.length problem.couples in
  let h = problem.heuristic in
  for i = n - 2 downto 0 do
    Array.iteri (fun ki _ ->
      h.(i).(ki) <- max_int;
      Array.iteri (fun kj heur_j ->
	if heur_j < max_int && problem.couples.(i + 1).(i).(kj).(ki) then
	  h.(i).(ki) <- min h.(i).(ki) (problem.costs.(i + 1).(kj) + heur_j))
	h.(i + 1)) h.(i)
  done

let solve times dtimes angles acft err=
  let ha = Array.map (fun a -> (Xy.angle (snd (Acft.route a).(0)), a)) acft in
  Array.sort compare ha;
  let acft = Array.map snd ha in
  let dev = Array.map (fun ai -> Array.copy (Acft.dev ai)) acft in
  let problem = get_problem times dtimes angles acft in
  set_heuristic problem;
  let n = Array.length problem.couples in
  let feasible node kj =
    let j = Array.length node in
    let rec tst i =
      i = -1 || (problem.couples.(j).(i).(kj).(node.(i)) && tst (i - 1)) in
    tst (j - 1) in
  let terminal (cost, node) = Array.length node = n in
  let heuristic node = 
    match Array.length node with
    | 0 -> 0
    | i -> problem.heuristic.(i - 1).(node.(i - 1)) in
  let arcs node =
    let i = Array.length node in
    let sons = ref [] in
    Array.iteri (fun ki cost ->
      if problem.heuristic.(i).(ki) < max_int && feasible node ki then (
	let new_node = Array.append node [|ki|] in
	sons := (cost, new_node):: !sons)) problem.costs.(i);
    !sons in
  try
    let (cost, sol) = Astar.solve terminal heuristic arcs [||] in
    Printf.printf "cost=%d\n%!" cost;
    Array.iteri (fun i ki ->
      Acft.transform acft.(i) problem.routes.(i).(ki) 1.;
      Acft.set_man err acft.(i);
      Acft.apply acft.(i))
      sol
  with Not_found ->
    Printf.printf "*** No solution ***\n%!";
    Array.iteri (fun i ai -> 
      Acft.transform ai dev.(i) 1.;
      Acft.set_man err ai) acft

      
