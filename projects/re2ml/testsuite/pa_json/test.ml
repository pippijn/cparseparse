match <:json<["hey", 1.0, 1 .. $args$]>> with
| <:json<{"hey": [1 .. $args$]}>> -> ()
| <:json<{"hey": [1 ..]}>> -> ()
| <:json<{"b":0,"a":1}>> -> ()
| <:json<[$str:a$ .. $tl$]>> -> ()
