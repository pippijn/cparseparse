let check_reachable dfa actions =
  let reachable = Array.make (Array.length actions) false in

  Dfa.Fsm.iter (fun state outgoing ->
    List.iter (fun (func, target) ->
      match func with
      | Dfa.Transition.Accept action ->
          reachable.(action) <- true
      | Dfa.Transition.Chr _ ->
          ()
    ) outgoing
  ) dfa;

  Array.iteri (fun action reachable ->
    if not reachable then
      let open Lexing in
      let t, s, e = actions.(action) in
      Printf.printf "%s:%d: rule will never be matched\n" s.pos_fname s.pos_lnum
  ) reachable
