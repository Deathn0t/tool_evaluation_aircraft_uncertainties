type data
val evalData : data -> float
val genData : unit -> data
val cross : data -> data -> data * data
val mutate : data -> data
val print_data : data -> unit
val print_best : data -> unit
val dataDistance : data -> data -> float
val dataBarycenter : data -> int -> data -> int -> data
val calcNew : data -> data -> float -> data
val calcNext : data -> int -> float -> data
val dim : int
val endlocal : data -> unit
val endlocalerror : unit -> unit
