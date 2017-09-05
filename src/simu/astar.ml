type 'node terminal = int * 'node -> bool
type 'node heuristic = 'node -> int
type 'node arcs = 'node -> (int * 'node) list

module PriorityQueue(Ord: Map.OrderedType):sig

  type 'a t
  val empty: unit -> 'a t
  val insert: 'a t -> Ord.t * 'a -> unit
  val extract_min: 'a t -> Ord.t * 'a

end = (struct

  module Queue = Map.Make(Ord)

  type 'a t = 'a list ref Queue.t ref
  let empty () = ref Queue.empty

  let insert queue (priority, x) = 
    try
      let elements = Queue.find priority !queue in
      elements := x:: !elements
    with Not_found -> queue := Queue.add priority (ref [x]) !queue

  let extract_min queue =
    match Queue.min_binding !queue with
    | (priority, ({contents = x::tl} as elements)) ->
      if tl = [] then (queue := Queue.remove priority !queue; (priority, x))
      else (elements := tl; (priority, x))
    | _ -> raise Not_found

end)
  
module AStarQueue = PriorityQueue(struct 
  type t = int * int
  let compare (f1, c1) (f2, c2) = 
    match compare f1 f2 with 0 -> compare c2 c1 | cmp -> cmp
end)

let solve terminal heuristic arcs source =
  let queue = AStarQueue.empty () in
  let generate cost (d_cost, son) =
    let son_cost = cost + d_cost in
    let son_h = heuristic son in
    let son_f = if son_h <> max_int then son_cost + son_h else max_int in
    AStarQueue.insert queue ((son_f, son_cost), son) in
  let rec develop () =
    let ((f, cost), node) = AStarQueue.extract_min queue in
    if terminal (cost, node) then (cost, node)
    else develop (List.iter (generate cost) (arcs node)) in
  develop (AStarQueue.insert queue ((heuristic source, 0), source))
