type 'a problem = {
  generate: unit -> 'a;
  eval: 'a -> float;
  mutate: 'a -> 'a;
  cross: 'a -> 'a -> 'a;
  continue: int -> ('a * float) array -> bool;
}

let solve ?(nb_elems=1000) ?(nb_gen=100) ?(pmut=0.4) ?(pcross=0.4) problem =
  let nmut = truncate (pmut *. float nb_elems) in
  let nsel = nb_elems - nmut - truncate (pcross *. float nb_elems) in
  if nsel < 2 then failwith "Ga.solve: pmut + pcross is too large";

  (* First generation *)
  let eval a = (a, problem.eval a) in
  let pop = Array.init nb_elems (fun _ -> eval (problem.generate ())) in
  Array.fast_sort (fun (_, (f1:float)) (_, f2) -> compare f2 f1) pop;

  (* Evolution *)
  let numgen = ref 1 in
  while !numgen <= nb_gen && problem.continue !numgen pop do

    (* Mutations *)
    for i = nsel to nsel + nmut - 1 do
      pop.(i) <- eval (problem.mutate (fst pop.(Random.int nsel)));
    done;

    (* Crossovers *)
    for i = nsel + nmut to nb_elems - 1 do
      let i1 = Random.int nsel in
      let i2 = (i1 + Random.int (nsel - 1)) mod nsel in
      pop.(i) <- eval (problem.cross (fst pop.(i1)) (fst pop.(i2)))
    done;

    (* Next generation *)
    Array.fast_sort (fun (_, (f1:float)) (_, f2) -> compare f2 f1) pop;
    incr numgen;
  done;
  pop
