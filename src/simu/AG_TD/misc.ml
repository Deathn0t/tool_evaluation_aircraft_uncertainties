let array_sort cmp a =
  Array.blit (Array.of_list (Sort.list cmp (Array.to_list a))) 0 a 0 (Array.length a)


let getnfirst l n =
  let rec getnfirst_2 l n =
  if n=0 then 
    [] 
  else
    (List.hd l)::(getnfirst_2 (List.tl l) (n-1))
  in
  if n>= List.length l then 
    l 
  else
    (getnfirst_2 l n);;
     
