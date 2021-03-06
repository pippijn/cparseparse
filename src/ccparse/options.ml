let _xc = ref false
let _wchar_t = ref false


let is_c_source s =
  let length = String.length s in
  if length < 3 then
    false
  else
    s.[length - 2] = '.' &&
    s.[length - 1] = 'c'


let () =
  Cmdline.register "parser" Arg.([
    "-xc",		Set _xc,		" parse code as C, not as C++ (implicit if any input file name ends with .c)";
    "-wchar_t",		Set _wchar_t,		" treat wchar_t as built-in type";
  ]) ~action:(fun inputs ->
    if List.filter is_c_source inputs <> [] then
      _xc := true;
  )


let _xc () = !_xc
let _wchar_t () = !_wchar_t
