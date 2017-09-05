(* ocamlopt.opt -o simu -I +labltk labltk.cmxa unix.cmxa simu.ml *)

let max_n = 5            (* Max number of acft *)
let vector = 60.         (* speed vector size (sec) *)
let lag = 60.

let speedup = 1.         (* Initial time acceleration *)
let scale = 10.          (* Initial scale (pixels / Nm) *)
let size = 600.          (* Initial window size (pixels) *)
let mode = 2           (* Initial mode: Basic, Static, Dynamic *)
let incert= 1
let draw_points = true   (* Draw waypoints ? *)
let draw_plane = false   (* Draw an airplane instead of a comet ? *)
let draw_traj_option = false        (* Draw convex trajectory ? *)
let draw_traj_conf_option = false  (* Draw conflicts of convex trajectory ? *)
let draw_bas_conf = false

let glob_incert = [|0.0; 30.; 30.; 5.|] (* Incertitudes*)

let solver = "AG_TD/agopt"
let solver_pln = "AG_TD/plns.txt"
let solver_sol = "AG_TD/sol.txt"

let background = `Color "#468"         (* Background color *)
let scale_color = `Color "yellow"      (* Scale color *)
let acft_color = `Color "red"        (* Aircraft normal color *)
let cur_color = `Color "red"        (* Seleced aircraft color *)
let conf_color = `Color "red"          (* Highlighted conflicts color *)
let conf_color2 = `Color "red"        (* Other conflicts color *)
let pln_color = `Color "#68a"          (* Route cmolor *)
let edit_color = `Color "black"         (* Highlighted route color *)
let traj_color = `Color "#579"       (* Convex trajectory color *)
let traj_conf_color = `Color "#358"  (* Convex conflicts color *)

let scale_tag = "Scale"
let acft_tag = "Aircraft"
let around_tag = "Around"
let route_tag = "Route"
let pln_tag = "PLN"
let edit_tag = "Edit"
let traj_tag = "Traj"
let traj_conf_tag = "Traj_Conf"
let conf_tag = "Conflict"
let current_tag = "current"

let hour = 3600.

(* 0 ; 10; 20; 30 *)
let randominit = 30;;

let plane =
  let half = [| -11,0; -13,-7; -12,-7; -9,-2; 2,-2;
                -7,-15; -4,-15; 9, -2; 18,-2; 20,-1|] in
  let n = Array.length half in
  Array.map (fun (x, y) -> (float x, float y)) (Array.init (2 * n) (fun i ->
      if i < n then half.(i)
      else let (x, y) = half.(2 * n - i - 1) in (x, -y)))

let round a = truncate (floor (a +.0.5))

let plane_xys (x, y) speed hdg_rad =
  let c = cos hdg_rad and s = sin hdg_rad in
  let z = speed *. hour /. 450. in
  Array.to_list (Array.map (fun (xi, yi) ->
      (x + round (z *. (c *. xi -. s *. yi)),
       y + round (z *. (s *. xi +. c *. yi)))) plane)

let t_bgn a =
  fst (Acft.route a).(0)

let t_cur a =
  fst (Acft.dev a).(0)

let t_end a =
  let route = Acft.route a in
  fst route.(Array.length route - 1)

let get_delay acft =
  let dist route i j = Xy.norm (Xy.sub (snd route.(i)) (snd route.(j))) in
  Array.fold_left (fun delay a ->
      let route = Acft.route a in
      let d = ref 0. in
      for i = 0 to Array.length route - 2 do d := !d +. dist route i (i + 1) done;
      let direct = dist route 0 (Array.length route - 1) in
      delay +. (!d -. direct) /. Acft.speed a) 0. acft


(*~~~Conflicts solver~~~*)

let start_solver acft err=
  Acft.save solver_pln acft err;
  if Sys.file_exists solver_sol then Sys.remove solver_sol;
  let cmd = Printf.sprintf "%s %s %s &" solver solver_pln solver_sol in
  ignore (Sys.command cmd)


(*~~~Simu~~~*)

type mode = Basic | Static | Dynamic
(*type incert = Low | Medium | High *)

type state = {
  cv: Widget.canvas Widget.widget;
  btn : Widget.button Widget.widget array;
  scl : Widget.scale Widget.widget array;
  mutable acft: Acft.t array;   (* Aircraft array *)
  mutable mode: mode;           (* Visu mode *)
  mutable incert: float array;
  mutable cur: int;             (* Mouse current aircraft index or -1 *)
  mutable dev_xy: Xy.xy array;  (* Mouse world coords during drag or [||] *)
  mutable timer: Timer.t;       (* Animation loop timer *) mutable start_t: float;       (* Absolute beginning time in sec. *) mutable speedup: float;       (* Time acceleration *)
  mutable scale: float;         (* pixels per Nm *)
  mutable xy0: Xy.xy;           (* pixels *)
}


let eval args =
  Protocol.tkEval (Array.map (fun w -> Protocol.TkToken w) args)

let time = Unix.gettimeofday

let rtime state = time () -.state.start_t

let state_time state = state.speedup *.rtime state

let is_conflict state =
  Acft.is_conflict state.incert state.acft
;;

let index = Acft.init_index_situation ()
let rep = ref 0;;
let incSpeed = [|0.2; 0.1; 0.0|];;
let mustSave = ref false;;

let new_state state n =
  Printf.printf "New state \n%!";
  Canvas.delete state.cv [`Tag "all"];
  let w = float (Winfo.width state.cv) in
  let h = float (Winfo.height state.cv) in
  state.xy0 <- (w /. 2., h /. 2.);
  if (!rep = 0) then (
    Acft.next_situation index;
    mustSave := is_conflict state);
  glob_incert.(0) <- incSpeed.(!rep);
  if (!rep = 2) then (
    rep := 0)
  else (
    rep := !rep + 1);
  Printf.printf "\n -> %d, %d, %d %d %d %f\n%! " index.(0) index.(1) index.(2) index.(3) index.(4) glob_incert.(0);
  state.acft <- Acft.gen_situation_2a (0.47 *. min h w /. scale) index;
  let t = Array.map t_bgn state.acft in
  Array.sort compare t;
  state.start_t <- time () -.t.(1);
  state.cur <- -1;
  state.dev_xy <- [||];
  state.speedup <- 1.
;;

let get_turn_dev state =
  if state.cur <> -1 && state.dev_xy <> [||] then (
    let a = state.acft.(state.cur) in
    let t = t_cur a in
    Acft.set_time a (max t (t_bgn a) +. lag);
    let (t0, xy) = (Acft.predict a).(0) in
    let hroute = if t < t_bgn a then Acft.route a else Acft.predict a in
    let h = Xy.angle (Xy.sub (snd hroute.(1)) (snd hroute.(0))) in
    let vect = Xy.sub state.dev_xy.(0) xy in
    let alpha = Xy.sub_angle (Xy.angle vect) h in
    let t1 = t0 +. Xy.norm vect /. Acft.speed a in
    Acft.set_time a t;
    Acft.turn a t0 alpha t1)

let get_xy_dev state =
  if state.cur <> -1 && state.dev_xy <> [||] then (
    let a = state.acft.(state.cur) in
    Acft.deviate a (max (t_bgn a) (t_cur a) +. lag) state.dev_xy.(0))

let switch_dev = ref false

let set_man state a =
  Acft.set_man_par a (Array.map (Scale.get) state.scl)

let get_dev state =
  if !switch_dev then get_turn_dev state else get_xy_dev state;
  if state.cur <> -1 then set_man state state.acft.(state.cur)

(* Drawings ---------------------------------------------------------------- *)

let cv_xy state xy =
  let (x, y) = Xy.add state.xy0 (Xy.mul state.scale xy) in
  (round x, round y)

let world_xy state (x, y) =
  Xy.mul (1./.state.scale) (Xy.sub (float x, float y) state.xy0)

let tag_id tag id =
  Printf.sprintf "%s %d" tag id

let get_id = function
  | _::ti::_ -> (try Scanf.sscanf ti "%s %d" (fun tag id -> id) with _ -> -1)
  | _ -> -1

let draw_scale state =
  Canvas.delete state.cv [`Tag scale_tag];
  let x = 10 + round (Acft.sep *. state.scale) in
  ignore (Canvas.create_line ~xys:[(10, x); (10, 10); (x, 10)]
            ~fill:scale_color ~arrow:`Both ~tags:[scale_tag] state.cv);
  ignore (Canvas.create_text ~x:20 ~y:20
            ~text:(Printf.sprintf "%.0fNm" Acft.sep)
            ~fill:scale_color ~anchor:`Nw ~tags:[scale_tag] state.cv)

let draw_pln state id =
  let route = Acft.route state.acft.(id) in
  let xys = Array.to_list (Array.map (fun (_, xy) -> cv_xy state xy) route) in
  let tagi = tag_id around_tag id in
  let tags = [around_tag; tagi] in
  Canvas.delete state.cv [`Tag tagi];
  ignore (Canvas.create_line ~xys:xys
            ~fill:(`Color "") ~width:15 ~tags:tags state.cv);
  let tagi = tag_id pln_tag id in
  let tags = [pln_tag; tagi] in
  Canvas.delete state.cv [`Tag tagi];
  ignore (Canvas.create_line ~xys:xys ~fill:pln_color
            ~arrow:`Last ~tags:tags state.cv);
  if draw_points then List.iter (fun (x, y) ->
      ignore (Canvas.create_oval ~x1:x ~y1:y ~x2:x ~y2:y ~outline:pln_color
                ~width:6 ~tags:tags state.cv)) xys;
  Canvas.lower state.cv (`Tag pln_tag)


let draw_route state id =
  let route = Acft.route state.acft.(id) in
  let xys = Array.to_list (Array.map (fun (_, xy) -> cv_xy state xy) route) in
  let tagi = tag_id around_tag id in
  let tags = [around_tag; tagi] in
  Canvas.delete state.cv [`Tag tagi];
  ignore (Canvas.create_line ~xys:xys
            ~fill:(`Color "") ~width:15 ~tags:tags state.cv);
  let tagi = tag_id route_tag id in
  let tags = [route_tag; tagi] in
  Canvas.delete state.cv [`Tag tagi];
  ignore (Canvas.create_line ~xys:xys ~fill:pln_color
            ~arrow:`Last ~tags:tags state.cv);
  if draw_points then List.iter (fun (x, y) ->
      ignore (Canvas.create_oval ~x1:x ~y1:y ~x2:x ~y2:y ~outline:pln_color
                ~width:6 ~tags:tags state.cv)) xys;
  Canvas.lower state.cv (`Tag route_tag)



let draw_dev state pred =
  Canvas.delete state.cv [`Tag edit_tag];
  if 1 < Array.length pred then (
    let xys = Array.to_list (Array.map (fun (_, xy) -> cv_xy state xy) pred) in
    ignore (Canvas.create_line ~xys:xys
              ~fill:edit_color ~arrow:`Last ~tags:[edit_tag] state.cv);
    if draw_points then List.iter (fun (x, y) ->
        ignore (Canvas.create_oval ~x1:x ~y1:y ~x2:x ~y2:y ~outline:edit_color
                  ~width:6 ~tags:[edit_tag] state.cv)) xys;
    Canvas.raise state.cv (`Tag conf_tag))


let draw_traj state =
  Canvas.delete state.cv [`Tag traj_tag; `Tag traj_conf_tag];
  if draw_traj_option && state.cur <> -1 &&
     Array.length (Acft.dev state.acft.(state.cur)) > 2 then (
    let traj = Acft.get_traj state.acft.(state.cur) in
    let conf =
      if draw_traj_conf_option then Acft.detect_traj state.incert state.acft state.cur
      else Array.map (fun _ -> false) traj in
    Array.iteri (fun t (xyzs, _, _) ->
        let xys = List.map (fun (x, y, _) -> cv_xy state (x, y)) xyzs in
        let color = if conf.(t) then traj_conf_color else traj_color in
        let tag = if conf.(t) then traj_conf_tag else traj_tag in
        let w = if conf.(t) then 3 else 1 in
        ignore (Canvas.create_polygon ~xys:xys
                  ~outline:color ~fill:(`Color "") ~width:w
                  ~tags:[tag; tag_id tag state.cur] state.cv))
      traj;
    Canvas.raise state.cv (`Tag traj_conf_tag);
    Canvas.raise state.cv (`Tag acft_tag))


let draw_acft state =
  Canvas.delete state.cv [`Tag acft_tag];
  let conf = Acft.pos_detect state.acft in
  Array.iteri (fun i ai ->
      let tags = [acft_tag; tag_id acft_tag i] in
      let pos = snd (Acft.predict ai).(0) in
      let next_pos = Acft.vector ai in
      let hdg = Xy.angle (Xy.sub next_pos pos) in
      let comet = Acft.comet ai in
      let color =
        if conf.(i) then conf_color
        else if i = state.cur then cur_color
        else acft_color in
      if draw_plane then (
        let xy = cv_xy state pos in
        ignore (Canvas.create_polygon ~xys:(plane_xys xy (Acft.speed ai) hdg)
                  ~fill:color ~outline:color ~tags:tags state.cv))
      else (
        (* Plot *)
        let (x, y as xy) = cv_xy state pos and d = 5 in
        ignore (Canvas.create_rectangle ~x1:(x-d) ~y1:(y-d) ~x2:(x+d) ~y2:(y+d)
                  ~outline:color ~tags:tags state.cv);
        (* Speed vector *)
        ignore (Canvas.create_line ~xys:[xy; cv_xy state next_pos]
                  ~fill:color ~width:2 ~tags:tags state.cv);
        (* Trace *)
        Array.iteri (fun j xyj ->
            let (x, y) = cv_xy state xyj and d = 4 - j in
            ignore (Canvas.create_oval ~x1:(x-d) ~y1:(y-d) ~x2:(x+d) ~y2:(y+d)
                      ~outline:color ~tags:tags state.cv)) comet))
    state.acft;
  draw_traj state

let draw_conf state =
  Canvas.delete state.cv [`Tag conf_tag];
  let funcignore tagi j xy1 xy2=
    ignore (Canvas.create_line ~xys:[cv_xy state xy1; cv_xy state xy2]
              ~fill:conf_color2 ~width:2
              ~tags:[conf_tag; tagi; tag_id conf_tag j] state.cv) in
  if draw_traj_conf_option then
    begin
      let lines=Acft.detect_traj_all state.incert  state.acft in
      let rec drawline tagi j d p l =
        match l with
          [] -> funcignore tagi j p d
        | hd::tl ->
          funcignore tagi j p hd;
          drawline tagi j d hd tl in
      Array.iteri (fun i lines ->
          let tagi = tag_id conf_tag i in
          List.iter (
            fun (j, l) ->
              match l with
                [] -> ()
              | hd::tl ->  drawline tagi j hd hd tl
          )lines) lines;
      if state.cur <> -1 then (
        let items = `Tag (tag_id conf_tag state.cur) in
        Canvas.configure_line ~fill:conf_color state.cv items;
        Canvas.raise state.cv items)
    end
  else
    begin
      Array.iteri (fun i lines ->
          let tagi = tag_id conf_tag i in
          List.iter (fun (j, (xy1, xy2)) -> funcignore tagi j xy1 xy2)lines)
        (Acft.detect_lines state.acft);
      if state.cur <> -1 then (
        let items = `Tag (tag_id conf_tag state.cur) in
        Canvas.configure_line ~fill:conf_color state.cv items;
        Canvas.raise state.cv items)
    end

let draw_all_originale state =
  if Canvas.gettags state.cv (`Tag scale_tag) = [] then draw_scale state;
  if state.mode = Basic then Canvas.delete state.cv [`Tag conf_tag];
  let t = state_time state in
  let errs = Array.map (Scale.get) state.scl in
  Array.iteri (fun i x -> Printf.printf "err(%d)=%f\n" i x) errs;
  Array.iteri (fun i ai ->
      let old_t = fst (Acft.predict ai).(0) in
      Acft.set_time ai t;
      if state.cur = i && state.dev_xy <> [||] then get_dev state
      else if floor (t /. Acft.step) <> floor (old_t /. Acft.step) then
        Acft.set_traj_par ai errs;
      if Canvas.gettags state.cv (`Tag (tag_id pln_tag i)) = [] then
        draw_pln state i;
      if Canvas.gettags state.cv (`Tag (tag_id route_tag i)) = [] then
        draw_route state i;
    ) state.acft;
  draw_acft state;
  if state.cur <> -1 && state.dev_xy <> [||] then (
    let a = state.acft.(state.cur) in
    draw_dev state (Acft.dev a);
    if state.mode = Static then Acft.set_time a (t_cur a));
  if state.mode <> Basic then draw_conf state

(*  Fonction modifiée *)
let draw_all state =
  (* if Canvas.gettags state.cv (`Tag scale_tag) = [] then draw_scale state; *)
  if state.mode = Basic then Canvas.delete state.cv [`Tag conf_tag];
  let t = state_time state in
  let errs = Array.map (Scale.get) state.scl in
  (* Array.iteri (fun i x -> Printf.printf "err(%d)=%f\n" i x) errs; *)
  Array.iteri (fun i ai ->
      let old_t = fst (Acft.predict ai).(0) in
      Acft.set_time ai t;
      if state.cur = i && state.dev_xy <> [||] then get_dev state
      else if floor (t /. Acft.step) <> floor (old_t /. Acft.step) then
        Acft.set_traj_par ai errs;
      if Canvas.gettags state.cv (`Tag (tag_id pln_tag i)) = [] then
        draw_pln state i;
      if Canvas.gettags state.cv (`Tag (tag_id route_tag i)) = [] then
        draw_route state i;
    ) state.acft;
  draw_acft state;
  if state.cur <> -1 && state.dev_xy <> [||] then (
    let a = state.acft.(state.cur) in
    draw_dev state (Acft.dev a);
    if state.mode = Static then Acft.set_time a (t_cur a))
(* if state.mode <> Basic then draw_conf state *)



let show_solution state old_pred next_action =
  let tail route = Array.map snd (Array.sub route 1 (Array.length route - 1)) in
  let predict = Array.map (fun a -> Array.copy (Acft.predict a)) state.acft in
  let mode = state.mode in
  let info = Array.mapi (fun i a ->
      let changed = tail predict.(i) <> tail old_pred.(i) in
      if Array.length predict.(i) = 2 && Array.length old_pred.(i) = 2 && changed
      then
        (* let (x1, y1) = snd predict.(i).(1) and (x2, y2) = snd old_pred.(i).(1) in *)
        (* Printf.printf "*** show_solution %f,%f %f,%f\n%!" x1 y1 x2 y2); *)
        if changed then Acft.transform a old_pred.(i) 1.;
      (Xy.angle (snd predict.(i).(0)), changed, i)) state.acft in
  let rec show j step =
    if j < Array.length state.acft then (
      let (_, changed, i) = info.(j) in
      state.cur <- i;
      if changed then (
        Acft.transform state.acft.(i) predict.(i) step;
        set_man state state.acft.(i);
        draw_dev state (Acft.dev state.acft.(i));
        draw_traj state;
        if mode <> Basic then
          draw_conf state;
        if step < 1. then
          Timer.set 50 (fun () -> show j (min 1. (step +.0.05)))
        else (
          Canvas.delete state.cv [`Tag edit_tag];
          draw_pln state i;
          draw_route state i;
          Timer.set 100 (fun () -> show (j + 1) 0.1)))
      else show (j + 1) 0.1)
    else (
      state.mode <- mode;
      state.cur <- -1;
      draw_all state;
      Printf.printf "delay = %.0f\n%!" (get_delay state.acft);
      next_action ()) in
  Array.sort compare info;
  Canvas.configure_line ~fill:pln_color state.cv (`Tag pln_tag);
  Canvas.configure_line ~fill:pln_color state.cv (`Tag route_tag);
  Array.iteri (fun i ai -> draw_pln state i;draw_route state i) state.acft;
  show 0 0.1

(*  ajout *)
let show_all state old_pred =
  let tail route = Array.map snd (Array.sub route 1 (Array.length route - 1)) in
  let predict = Array.map (fun a -> Array.copy (Acft.predict a)) state.acft in
  let mode = state.mode in
  let info = Array.mapi (fun i a ->
      let changed = tail predict.(i) <> tail old_pred.(i) in
      if Array.length predict.(i) = 2 && Array.length old_pred.(i) = 2 && changed
      then (
        let (x1, y1) = snd predict.(i).(1) and (x2, y2) = snd old_pred.(i).(1) in
        Printf.printf "*** show_solution %f,%f %f,%f\n%!" x1 y1 x2 y2);
      if changed then Acft.transform a old_pred.(i) 1.;
      (Xy.angle (snd predict.(i).(0)), changed, i)) state.acft in
  let rec show j step =
    if j < Array.length state.acft then (
      let (_, changed, i) = info.(j) in
      state.cur <- i;
      if changed then (
        Acft.transform state.acft.(i) predict.(i) step;
        set_man state state.acft.(i);
        if (draw_bas_conf) then (draw_dev state (Acft.dev state.acft.(i)));
        draw_traj state;
        if mode <> Basic && (draw_bas_conf) then
          draw_conf state;
        if step < 1. then
          Timer.set 50 (fun () -> show j 1.)
        else (
          Canvas.delete state.cv [`Tag edit_tag]; (* utilité ? *)
          draw_pln state i;
          draw_route state i;
          Timer.set 100 (fun () -> show (j + 1) 1.)))
      else show (j + 1) 1.)
    else (
      state.mode <- mode;
      state.cur <- -1;
      draw_all state;
      Printf.printf "delay = %.0f\n%!" (get_delay state.acft)) in
  Array.sort compare info;
  Canvas.configure_line ~fill:pln_color state.cv (`Tag pln_tag);
  Canvas.configure_line ~fill:pln_color state.cv (`Tag route_tag);
  Array.iteri (fun i ai -> draw_pln state i;draw_route state i) state.acft;
  show 0 1.

(* Interactions ----------------------------------------------------------- *)

let highlight_current state evnt =
  let cur = get_id (Canvas.gettags state.cv (`Tag current_tag)) in

  if state.cur <> cur(* && cur <> -1 *) then (

    if state.cur <> -1 then (draw_pln state state.cur;draw_route state state.cur);

    state.cur <- cur;
    draw_traj state;

    Canvas.configure_line ~fill:pln_color state.cv (`Tag pln_tag);
    Canvas.configure_line ~fill:pln_color state.cv (`Tag route_tag);
    Canvas.configure_line ~fill:conf_color2 state.cv (`Tag conf_tag);
    Canvas.configure_line ~fill:acft_color state.cv (`Tag acft_tag);
    let tag = `Tag (tag_id around_tag state.cur) in
    Canvas.raise state.cv ~above:(`Tag around_tag) tag;
    let tag = `Tag (tag_id route_tag state.cur) in
    Canvas.configure_line ~fill:edit_color state.cv tag;
    draw_acft state;
    if state.mode <> Basic && draw_bas_conf then draw_conf state)


let drag_edit state evnt =
  if state.cur <> -1 then (
    let a = state.acft.(state.cur) in
    let t = t_cur a in
    if t < t_end a -.lag then (
      if state.dev_xy = [||] then (
        Canvas.configure_line ~fill:pln_color state.cv (`Tag pln_tag);
        Canvas.configure_line ~fill:pln_color state.cv (`Tag route_tag);
      );
      state.dev_xy <- [|world_xy state (evnt.Tk.ev_MouseX, evnt.Tk.ev_MouseY)|];
      get_dev state;
      draw_dev state (Acft.dev a);
      draw_traj state;
      if state.mode = Dynamic then draw_conf state)
    else (
      state.dev_xy <- [||];
      Canvas.delete state.cv [`Tag edit_tag];
      Acft.set_time a (t_cur a);
      Acft.set_traj_par a (Array.map Scale.get state.scl);
      if state.mode = Dynamic then draw_conf state))

let apply_edit state evnt =
  if state.cur <> -1 && state.dev_xy <> [||] then (
    let a = state.acft.(state.cur) in
    Acft.apply a;
    state.dev_xy <- [||];
    Printf.printf "delay = %.0f\n%!" (get_delay state.acft);
    let tag = `Tag (tag_id pln_tag state.cur) in
    Canvas.delete state.cv [tag; `Tag edit_tag];
    let tag = `Tag (tag_id route_tag state.cur) in
    Canvas.delete state.cv [tag; `Tag edit_tag];
    draw_all state;
    Canvas.configure_line ~fill:edit_color state.cv tag)

let cancel_edit state =
  Canvas.delete state.cv [`Tag edit_tag; `Tag traj_tag; `Tag traj_conf_tag];
  Canvas.configure_line ~fill:pln_color state.cv (`Tag pln_tag);
  Canvas.configure_line ~fill:pln_color state.cv (`Tag route_tag);
  let a = state.acft.(state.cur) in
  Acft.set_time a (t_cur a);
  set_man state a;
  state.cur <- -1;
  state.dev_xy <- [||];
  if state.mode = Dynamic then draw_conf state

let update_traj state =
  if state.cur <> -1 then
    Acft.set_traj_par state.acft.(state.cur) (Array.map Scale.get state.scl);
  draw_traj state
(* draw_conf state *)

let incr_speed state dspeed () =
  let t = time () in
  let old = state.speedup in
  state.speedup <-
    if dspeed = 0. then 1. else (max 1. (state.speedup +.dspeed));
  state.start_t <- t +.old /.state.speedup *.(state.start_t -.t)

let reset state () =
  (* new_state state (2 + Random.int (max_n - 1)); *)
  new_state state 2;
  incr_speed state (speedup -.state.speedup) ();
  draw_all state

let rec loop state next_action () =
  Timer.remove state.timer;
  let ms = round (1000. /. state.speedup) in
  state.timer <- Timer.add ms (loop state next_action);
  let t = state_time state in
  let te = Array.fold_left (fun t a -> max (t_end a) t) 0. state.acft in
  if t < te then (
    if state.cur <> -1 then (
      if t_end state.acft.(state.cur) -.lag < t then
        state.dev_xy <- [||]);
    draw_all state)
  else (
    reset state ();
    (* Printf.printf "CONFLICT : %b \n" (Acft.is_conflict state.acft); *)
    next_action ())

let drop state next_action () =
  Printf.printf "delay = %.0f\n%!" (get_delay state.acft);
  incr_speed state (10.*.250.-.state.speedup) ();
  loop state next_action ()

(* Sauvegarde une image sous le nom file_name de l'état state *)
let save_image file_name state =
  let path = Printf.sprintf "./images/%s.ps" file_name in
  let _ = Canvas.postscript ~file:path state.cv in
  let _ = Sys.command "python3 ps2png.py" in
  let _ = Sys.command "rm images/*.ps" in
  let _ = Sys.command " rm images/*.png" in ()


(*
    Fonction solve modifier pour construire la base de d'entrainement
du CNN.
*)
let rec solve state () =
  let insertSpeed = glob_incert.(0) in
  let angle = Acft.angle state.acft.(1) in
  let tp1 = Acft.time_start state.acft.(0) in
  let tp2 = Acft.time_start state.acft.(1) in
  let speed1 = Acft.speed state.acft.(0) in
  let speed2 = Acft.speed state.acft.(1) in
  let t_max = state_time state +.lag in
  let old_t = state_time state in
  let name_photo = (Printf.sprintf "apres_%f_%f_%f_%f_%f_%f_"
                      insertSpeed angle tp1 tp2 speed1 speed2) in
  let normal = Tk.cget state.btn.(2) `State <> "disabled" in
  let rec get_solution () =
    if state_time state < t_max && not (Sys.file_exists solver_sol) then (
      Timer.set 500 get_solution)
    else (
      if normal then
        List.iter (fun i ->
            Button.configure ~state:`Normal state.btn.(i)) [0; 1; 2];
      if Sys.file_exists solver_sol then (
        let (_,acft) = Acft.load solver_sol in
        Array.iteri (fun i a ->
            Acft.transform a (Acft.dev acft.(i)) 1.) state.acft;
        (*Creer photo*)
        state.start_t <- Unix.gettimeofday () -. old_t/.state.speedup;
        Canvas.delete state.cv [`Tag pln_tag];
        draw_all state;
        Acft.printdev state.acft;
        Canvas.delete state.cv [`Tag (tag_id route_tag 0)];
        if (is_conflict state) then
          let _ = Sys.command "rm images/*.ps" in
          ()
        else (
          save_image name_photo state;
          if (!rep = 0) || (!rep = 1) then mustSave := true);
        drop state (solve state) ())) in
  List.iter (fun i ->
      Button.configure ~state:`Disabled state.btn.(i)) [0; 1; 2];
  state.incert <- glob_incert;
  Array.iter (fun x -> Printf.printf "%f " x) state.incert;
  flush stdout;
  if (is_conflict state) then (
    start_solver state.acft state.incert;
    get_solution ())
  else (
    Printf.printf "DROP DROP DROP";
    if (!mustSave) then save_image name_photo state;
    if (!rep = 2) then mustSave := false;
    drop state (solve state) ())

let redo state () =
  let t = Array.map t_bgn state.acft in
  Array.iteri (fun i a ->
      Acft.set_time a t.(i);
      Acft.turn a t.(i) 0. t.(i);
      set_man state a;
      Acft.apply a) state.acft;
  Array.sort compare t;
  state.start_t <- time () -.t.(1);
  state.cur <- -1;
  state.dev_xy <- [||];
  Canvas.delete state.cv [`Tag "all"];
  draw_all state;
  loop state (reset state) ()

let scroll state evnt =
  state.start_t <- state.start_t -. 5.;
  draw_all state

let back state evnt =
  state.start_t <- state.start_t +. 10.;
  draw_all state

let change_mode state =
  let text = match state.mode with
    | Basic -> state.mode <- Static; "Mode: Static"
    | Static -> state.mode <- Dynamic; "Mode: Dynamic"
    | Dynamic -> state.mode <- Basic; "Mode: Basic" in
  if text <> "" then (
    Button.configure ~text:text state.btn.(0);
    draw_all state)

let test_conflict state () =
  Printf.printf "CONFLIT : %b !\n" (is_conflict state)

let main =
  (* Set current directory *)
  Sys.chdir (Filename.dirname Sys.executable_name);

  let top = Tk.openTk() in
  Wm.title_set top "Control Assistance Tool";
  let frame = Frame.create top in
  let btn = Array.map (fun text -> Button.create ~text:text frame)
      [| "Redo";
         "Solve";
         "Next";  |] in
  let scale_frame = Frame.create top in
  let scl = Array.map (fun (label, minv, maxv, dv) ->
      let scl = Scale.create ~label:label	~min:minv ~max:maxv ~resolution:dv
          ~orient:`Horizontal scale_frame in
      Scale.set scl ((minv +. maxv) /. 2.);
      scl)
      [|("Speed", 0., 0.2, 0.01);
        ("T0", 0., 60., 1.);
        ("T1", 0., 60., 1.);
        ("Heading", 0., 10., 1.)|] in
  let width = round size in
  let state = {
    cv = Canvas.create ~width:width ~height:width top;
    btn = btn;
    scl = scl;
    acft = [||];
    mode = [|Basic; Static; Dynamic|].(mode);
    incert = glob_incert;
    cur = -1;
    dev_xy = [||];
    timer = Timer.add 0 (fun () -> ());
    start_t = time ();
    speedup = speedup;
    scale = scale;
    xy0 = Xy.mul 0.5 (size, size);
  } in

  Tk.pack ~expand:true ~fill:`Both [state.cv];
  Tk.pack ~side:`Left (Array.to_list state.btn);
  Tk.pack ~side:`Left ~ipadx:50 (Array.to_list state.scl);
  Tk.pack [frame; scale_frame];

  Tk.update();
  new_state state 2;
  Random.init randominit;
  let nil () = () in
  Button.configure ~command:(redo state) state.btn.(0);
  Button.configure ~command:(fun () -> solve state ()) state.btn.(1);
  Button.configure ~command:(drop state nil) state.btn.(2);
  Array.iter (fun scl ->
      Scale.configure ~command:(fun _ -> update_traj state) scl) state.scl;

  Canvas.configure ~background:background state.cv;
  let bind w events action =
    Tk.bind ~events:events ~fields:[`MouseX; `MouseY] ~action:action w in
  bind state.cv [`KeyPressDetail "q"] (fun _ -> Tk.closeTk ());
  bind state.cv [`KeyPressDetail "b"] (fun _ -> back state ());
  bind state.cv [`KeyPressDetail "d"] (fun _ -> switch_dev := not !switch_dev);
  bind state.cv [`Motion] (highlight_current state);
  bind state.cv [`ButtonPressDetail 1] (drag_edit state);
  bind state.cv [`Modified ([`Button1], `Motion)] (drag_edit state);
  bind state.cv [`ButtonReleaseDetail 1] (apply_edit state);
  bind state.cv [`ButtonPressDetail 3] (fun _ -> cancel_edit state);
  bind top [`ButtonPressDetail 5] (scroll state);
  Focus.set state.cv;
  draw_all state;
  loop state (reset state) ();
  Tk.mainLoop ()
