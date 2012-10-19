include Format

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |       Print list of printer with interleaved printing separator       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let pp_list : 'a .
    (formatter -> 'a -> unit)
    -> (Format.formatter -> unit -> unit)
    -> formatter
    -> 'a list
    -> unit
  = fun f sep ppf lst ->
    let first = ref true in
    List.iter (fun printer ->
      begin
        if !first
        then fprintf ppf "%a" f printer
        else fprintf ppf "%a%a" sep () f printer
      end;
      first := false
    ) lst

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                        Common list separators                         | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let pp_break_sep ppf () = pp_print_break ppf 0 0
let pp_comma_sep ppf () = fprintf ppf ",@ "
let pp_newline_sep ppf () = fprintf ppf "@\n"
let pp_space_sep ppf () = fprintf ppf "@ "
let pp_newline_bar_sep ppf () = fprintf ppf "@;|@ "
let pp_space_bar_sep ppf () = fprintf ppf "@ |@ "
