#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <sched.h>
#include <string.h>


static int
policy_of_value (value policy_val)
{
  switch (Int_val (policy_val))
    {
    case 0: return SCHED_OTHER;
#ifdef SCHED_BATCH
    case 1: return SCHED_BATCH;
#endif
#ifdef SCHED_IDLE
    case 2: return SCHED_IDLE;
#endif
    case 3: return SCHED_FIFO;
    case 4: return SCHED_RR;
    default: failwith ("unsupported scheduling policy");
    }
  return 0;
}


CAMLprim value
ml_Sched_setscheduler (value pid_val, value policy_val, value params_val)
{
  CAMLparam1 (params_val);

  int pid = Int_val (pid_val);
  int policy = policy_of_value (policy_val);

  struct sched_param param;
  param.sched_priority = Int_val (Field (params_val, 0));

  if (sched_setscheduler (pid, policy, &param) == -1)
    failwith (strerror (errno));

  CAMLreturn (Val_unit);
}
