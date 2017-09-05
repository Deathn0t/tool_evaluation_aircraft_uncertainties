type t                   (* Aircraft *)
type time = float        (* time (s) *)
type speed = float       (* speed (Nm/s) *)
type xy = float * float  (* Nm, Nm *)
type route = (time * xy) array
(*
type pln = xy array
*)
val sep: float           (* separation distance (NM) *)
val step: time           (* trajectory prediction step (s) *)

(* Aircraft information *)
val time_start: t -> time
val speed: t -> speed     (* Nominal speed *)
val route: t -> route     (* Real trajectory, from entry to exit point *)
(* val pln: t -> pln      Real trajectory, from entry to exit point *)
val predict: t -> route   (* Trajectory prediction, from current time *)
val dev: t -> route       (* Deviation prediction, from current time *)
val vector: t -> xy       (* Predicted position 1 minute later *)
val comet: t -> xy array  (* Five last positions [|(Nm, Nm); ...|]*)
val delay: t -> time      (* Current delay *)
val angle: t -> float
(* Aircraft operations *)

val set_time: t -> time -> unit
(** set_time a t: build the trajectory prediction of a at t *)

val turn: t -> time -> float -> time -> unit
(** turn a t0 alpha t1: deviate a of alpha (°) from t0 until t1 *)

val deviate: t -> time -> xy -> unit
(** deviate a t xy: deviate a at t towards xy *)

val apply: t -> unit
(** apply a: apply the current deviation to the real route of a *)

val transform: t -> route -> float -> unit
(** transform a route ratio: set partial deviation to a ratio of route *)

(* Conflicts detection *)

val pos_detect: t array -> bool array
(** pos_detect [|a1; ...|] return [|bool;...|]: conflicts at current time *)

val detect: t array -> time array
(** detect [|a1; ...|] return [|conflict duration, ...|] *)

val is_conflict : float array -> t array -> bool

val detect_lines: t array -> (int * (xy * xy)) list array
(** detect_lines [|a1; ...|] return [|[(i, seg11); ...], ...|] *)

(* Random traffic situation *)

val random: float -> int -> t array
(** random size(Nm) n return acft: random traffic situation with n aircraft *)

val enum_rnd_2a: float -> t array

val enum_all_2a: float -> int -> int -> int -> t array

val gen_situation_2a: float -> int array -> t array

val init_index_situation: unit -> int array

val next_situation: int array -> unit

(* Save aircraft files. *)
val save: string -> t array -> float array -> unit

(* Load aircraft files. *)
val load: string -> (float array * t array)

(* Access to funcnico *)

    (*
val set_man: t -> int -> unit
val set_traj: t -> int -> unit
val get_traj: t -> ((float * float * float) list * float * float) array
val detect_traj: t array -> int -> int -> bool array
     *)

val printdev: t array-> unit
val modifyfornico: float array -> t array -> (float*float*float) array -> int array
val copyacft: t -> t
val set_man: float array -> t -> unit
val set_man_par: t -> float array -> unit
val set_traj: t -> float array -> unit
val set_traj_par: t -> float array -> unit
val get_traj: t -> ((float * float * float) list * float * float) array
val detect_traj: float array -> t array -> int -> bool array
val detect_traj_glob: float array -> t array -> (float array*float)
val detect_traj_mat: float array -> t array -> ((float array) array*float array*float)
val detect_traj_all: float array -> t array -> (int * ((float * float) list)) list array
(*
val print_pln: t array -> unit
*)
