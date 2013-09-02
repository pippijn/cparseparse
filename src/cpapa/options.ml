let _rt		= ref false


let () =
  Cmdline.register "cpapa" Arg.([
    "-rt",		Set _rt,		" set real-time scheduling policy with highest priority";
  ])


let _rt		() = !_rt
