let config_filename = ref "AG_TD/general.cfg"

let fileout = ref stdout

let bool_of_string =
  function
    "true" -> true |
       _   -> false

module Map_strings = Map.Make(struct type t=string 
                             let compare x y = compare x y end);;

let map = 
let m = ref Map_strings.empty in
let inch = open_in !config_filename in
try (
  while true do
    let st = (input_line inch) in
    let sttab = Array.of_list (Str.split (Str.regexp "[ \t]+") st) in
    if (Array.length sttab) >=2 then
      m:= Map_strings.add sttab.(0) sttab.(1) !m;
done;
!m)
with
  End_of_file -> close_in inch;!m
| x -> raise x
;;

let find_val s = 
  try (
    let res=Map_strings.find s map in
    (* Printf.fprintf !fileout "%s=%s\n" s res;flush stdout; *) res)
  with x -> Printf.fprintf !fileout "erreur:%s\n" s;flush stdout;raise x;;


let numgen  = ref 0
let nbgens    = int_of_string (find_val "nbgens");;
let nbelems   = ref (int_of_string (find_val "nbelems"));;
let seed =  int_of_string (find_val "seed");;
let _ = Random.init seed

let read_config () =
let pcross    = float_of_string (find_val "pcross")
and pmut      = float_of_string (find_val "pmut")
and scaling   = int_of_string (find_val "scaling")
and elitist   = bool_of_string (find_val "elitist")
and sharing   = float_of_string (find_val "sharing")
and complex_sharing = float_of_string (find_val "complex_sharing")
and evolutive = bool_of_string (find_val "evolutive")
and simp_alpha = float_of_string (find_val "simp_alpha")
and simp_precision = float_of_string (find_val "simp_precision")
and simp_iterations = int_of_string (find_val "simp_iterations") in
(pcross,pmut,scaling,elitist,sharing,complex_sharing,evolutive,simp_alpha,
simp_precision,simp_iterations);;

(* EOM *)

