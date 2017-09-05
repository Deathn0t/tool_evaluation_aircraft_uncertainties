let draw_points = true   (* Draw waypoints ? *)
let draw_plane = false   (* Draw an airplane instead of a comet ? *)

let background = `Color "#468"     (* Background color *)
let scale_color = `Color "yellow"  (* Scale color *)
let acft_color = `Color "white"    (* Aircraft normal color *)
let cur_color = `Color "yellow"    (* Seleced aircraft color *)
let conf_color = `Color "red"      (* Highlighted conflicts color *)
let conf_color2 = `Color "#434"    (* Other conflicts color *)
let route_color = `Color "#68a"      (* Route color *)
let edit_color = `Color "cyan"     (* Highlighted route color *)

let sep_tag = "Sep"
let acft_tag = "Aircraft"
let around_tag = "Around"
let route_tag = "Route"
let edit_tag = "Edit"
let conf_tag = "Conflict"
let current_tag = "current"

let plane =
  let half = [|
    -11,0; -13,-7; -12,-7; -9,-2; 2,-2; -7,-15; -4,-15; 9,-2; 18,-2; 20,-1|] in
  let float_xy (x, y) = (float x, float y) in
  let sym (x, y) = (x, -y) in
  let nh = Array.length half in
  let n = 2 * nh in
  Array.init n (fun i ->
    if i < nh then float_xy half.(i) else float_xy (sym (half.(n - i - 1))))

type xy = float * float

type map = {
  cv: Widget.canvas Widget.widget;
  mutable xy0: Xy.xy;           (* pixels *)
  mutable scale: float;         (* pixels per Nm *)
}

let create cv width height scale =
  Canvas.configure ~width:width ~height:height ~background:background cv;
  let x = float (min width height) in
  {cv=cv; xy0=(x, x); scale=scale}

let eval args =
  Protocol.tkEval (Array.map (fun w -> Protocol.TkToken w) args)

let cv_xy map xy =
  let (x, y) = Xy.add map.xy0 (Xy.mul map.scale xy) in
  (truncate (floor (x +.0.5)), truncate (floor (y +. 0.5)))

let world_xy map (x, y) =
  Xy.mul (1. /. map.scale) (Xy.sub (float x, float y) map.xy0)

let tag_id tag id =
  Printf.sprintf "%s %d" tag id

let get_id = function
  | _::ti::_ -> (try Scanf.sscanf ti "%s %d" (fun tag id -> id) with _ -> -1)
  | _ -> -1

let draw_sep map =
  Canvas.delete map.cv [`Tag sep_tag];
  let x = 10 + truncate (Acft.sep *. map.scale) in
  ignore (Canvas.create_line ~xys:[(10, x); (10, 10); (x, 10)]
	    ~fill:scale_color ~arrow:`Both ~tags:[sep_tag] map.cv);
  ignore (Canvas.create_text ~x:20 ~y:20 
	    ~text:(Printf.sprintf "%.0fNm" Acft.sep)
	    ~fill:scale_color ~anchor:`Nw ~tags:[sep_tag] map.cv)
    
let draw_route map route id =
  let xys = Array.to_list (Array.map (fun (_, xy) -> cv_xy map xy) route) in
  let around_tagi = tag_id around_tag id and route_tagi = tag_id route_tag id in
  Canvas.delete map.cv [`Tag around_tagi; `Tag route_tagi];
  ignore (Canvas.create_line ~xys:xys ~fill:(`Color "") ~width:15 
	    ~tags:[around_tag; around_tagi] map.cv);
  let tags = [route_tag; route_tagi] in
  ignore (Canvas.create_line ~xys:xys ~fill:route_color ~arrow:`Last 
	    ~tags:tags map.cv);
  if draw_points then List.iter (fun (x, y) ->
    ignore (Canvas.create_oval ~x1:x ~y1:y ~x2:x ~y2:y ~outline:route_color
	      ~width:6 ~tags:tags map.cv)) xys;
  Canvas.lower map.cv (`Tag route_tag)

let draw_dev map dev id =
  Canvas.delete map.cv [`Tag edit_tag];
  if 1 < Array.length dev then (
    let xys = Array.to_list (Array.map (fun (_, xy) -> cv_xy map xy) dev) in
    ignore (Canvas.create_line ~xys:xys
	      ~fill:edit_color ~arrow:`Last ~tags:[edit_tag] map.cv);
    if draw_points then List.iter (fun (x, y) ->
      ignore (Canvas.create_oval ~x1:x ~y1:y ~x2:x ~y2:y ~outline:edit_color
		~width:6 ~tags:[edit_tag] map.cv)) xys;
    Canvas.raise map.cv (`Tag conf_tag))

let plane_xys (x, y) speed heading =
  let (c, s) = Xy.polar heading in
  let size = speed *. 8. in
  Array.to_list (Array.map (fun (xi, yi) ->
    (x + truncate (floor (size *. (c *. xi -. s *. yi)) +. 0.5), 
     y + truncate (floor (size *. (s *. xi +. c *. yi)) +. 0.5))) plane)

let draw_acft map acft curr_id =
  Canvas.delete map.cv [`Tag acft_tag];
  let conf = Acft.pos_detect acft in
  Array.iteri (fun i ai ->
    let tags = [acft_tag; tag_id acft_tag i] in
    let pos = snd (Acft.predict ai).(0) in
    let next_pos = Acft.vector ai in
    let hdg = Xy.angle (Xy.sub next_pos pos) in
    let comet = Acft.comet ai in
    let color = 
      if conf.(i) then conf_color 
      else if i = curr_id then cur_color 
      else acft_color in
    if draw_plane then (
      let xy = cv_xy map pos in
      ignore (Canvas.create_polygon ~xys:(plane_xys xy (Acft.speed ai) hdg)
		~fill:color ~outline:color ~tags:tags map.cv))
    else (
      (* Plot *)
      let (x, y as xy) = cv_xy map pos and d = 5 in
      ignore (Canvas.create_rectangle ~x1:(x-d) ~y1:(y-d) ~x2:(x+d) ~y2:(y+d)
		~outline:color ~tags:tags map.cv);
      (* Speed vector *)
      ignore (Canvas.create_line ~xys:[xy; cv_xy map next_pos]
		~fill:color ~width:2 ~tags:tags map.cv);
      (* Trace *)
      Array.iteri (fun j xyj ->
	let (x, y) = cv_xy map xyj and d = 4 - j in
	ignore (Canvas.create_oval ~x1:(x-d) ~y1:(y-d) ~x2:(x+d) ~y2:(y+d)
		  ~outline:color ~tags:tags map.cv)) comet))
    acft

let draw_conflict_lines map lines curr_id =
  Canvas.delete map.cv [`Tag conf_tag];
  Array.iteri (fun i lines_i ->
    let tagi = tag_id conf_tag i in
    List.iter (fun (j, (xy1, xy2)) ->
      let color = 
	if i = curr_id || j = curr_id then conf_color else conf_color2 in
      ignore (Canvas.create_line ~xys:[cv_xy map xy1; cv_xy map xy2]
		~fill:color ~width:2 
		~tags:[conf_tag; tagi; tag_id conf_tag j] map.cv)) 
      lines_i)
    lines;
  if curr_id <> -1 then Canvas.raise map.cv (`Tag (tag_id conf_tag id))
