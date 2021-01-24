
(* Przelewanka *)

(* gcd *)
let rec gcd x y =
  if y = 0 then x
  else gcd y (x mod y)
;;

exception Result of int;;

let my_przelewanka cups final_state =

  let n = Array.length cups in
  let visited = Hashtbl.create (n * 1000 + 1) in
  let q = Queue.create () in

  (* adding the initial state of the cups *)
  Hashtbl.add visited (Array.make n 0) 0;
  Queue.push (Array.make n 0) q;

  (* action of spilling out water from one of the cups *)
  (* state - array, id - index of cup, new_state - array of new state *)
  (* this function returns the new state based on the given state *)
  let spill state id =
    let new_state = Array.copy state in
    new_state.(id) <- 0;
    new_state
  in

  (* action of filling out water from one of the cups *)
  (* state - array, id - index of cup, new_state - array of new state *)
  (* this function returns the new state based on the given state *)
  let fill state id =
    let new_state = Array.copy state in
    new_state.(id) <- cups.(id);
    new_state
  in

  (* action of pouring water from one cup to another *)
  (* state - array, id1 - index of cup from which we pour the water,
     id2 - index of cup to which we pour the water to, new_state - array of new state *)
  (* this function returns the new state based on the given state *)
  let pour state id1 id2 =
    let new_state = Array.copy state in
    if state.(id1) + state.(id2) <= cups.(id2) then
      begin
        new_state.(id1) <- 0;
        new_state.(id2) <- state.(id1) + state.(id2)
      end
    else
      begin
        new_state.(id1) <- state.(id1) - (cups.(id2) - state.(id2));
        new_state.(id2) <- cups.(id2)
      end;
    new_state
  in

  while not (Queue.is_empty q) do
    let my_state = Queue.take q in
    let my_dist = Hashtbl.find visited my_state in

    if my_state = final_state then
      raise (Result my_dist);

    (* function checking of a state was considered before
       - if not it is added to the queue *)
    let add_if_not_visited state dist =
      if state = final_state then raise (Result dist);
      if not (Hashtbl.mem visited state) then
        begin
          Hashtbl.add visited state dist;
          Queue.push state q
        end
    in

    (* adding all the states of spilling out water form each cup *)
    for i = 0 to n - 1 do
      add_if_not_visited (spill my_state i) (my_dist + 1)
    done;

    (* adding all the states of filling each cup *)
    for i = 0 to n - 1 do
      add_if_not_visited (fill my_state i) (my_dist + 1)
    done;

    (* adding all the states of puoring water from one cup to another *)
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j <> i then
          add_if_not_visited (pour my_state i j) (my_dist + 1);
      done;
    done;
  done;
  (-1)
;;


let przelewanka cups_array =
  let cups_list = Array.to_list cups_array in
  let (cups, final_state) = List.split cups_list in
  let cups = Array.of_list cups
  and final_state = Array.of_list final_state in
  (* condition for the final  *)
  let my_gcd = Array.fold_left (fun el (x, _) -> gcd el x) 0 cups_array in
  if my_gcd = 0 then 0 else
    if Array.for_all (fun (_, y) -> (y mod my_gcd) = 0) cups_array &&
        Array.exists (fun (x, y) -> (x = y) || y = 0) cups_array then
      begin
        try
          my_przelewanka cups final_state
        with
        | Result i -> i
      end
    else
      (-1)
;;
