type 'a problem = {
  generate : unit -> 'a;
  eval : 'a -> float;
  mutate : 'a -> 'a;
  cross : 'a -> 'a -> 'a;
  continue : int -> ('a * float) array -> bool;
}

val solve :
  ?nb_elems:int -> ?nb_gen:int -> ?pmut:float -> ?pcross:float -> 
  'a problem -> ('a * float) array
