(* A* with int costs and no node memorization *)

type 'node terminal = int * 'node -> bool
type 'node heuristic = 'node -> int
type 'node arcs = 'node -> (int * 'node) list

val solve : 'a terminal -> 'a heuristic -> 'a arcs -> 'a -> int * 'a
