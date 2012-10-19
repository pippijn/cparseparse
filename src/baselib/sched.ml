type pid_t = int

type policy =
  | OTHER
  | BATCH
  | IDLE
  | FIFO
  | RR

type param = {
  priority : int;
}

(* setscheduler pid policy param *)
external setscheduler : pid_t -> policy -> param -> unit = "ml_Sched_setscheduler"
