extern "C" {
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <valgrind/callgrind.h>
#include <valgrind/memcheck.h>

#include <cstdio>
#include <set>

typedef std::set<value> value_set;

__BEGIN_DECLS

/***************************************
 * :: callgrind
 ***************************************/


CAMLprim value
ml_Valgrind_Callgrind_dump_stats (void)
{
  CALLGRIND_DUMP_STATS;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Callgrind_dump_stats_at (value pos_str)
{
  CALLGRIND_DUMP_STATS_AT (String_val (pos_str));
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Callgrind_zero_stats (void)
{
  CALLGRIND_ZERO_STATS;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Callgrind_toggle_collect (void)
{
  CALLGRIND_TOGGLE_COLLECT;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Callgrind_start_instrumentation (void)
{
  CALLGRIND_START_INSTRUMENTATION;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Callgrind_stop_instrumentation (void)
{
  CALLGRIND_STOP_INSTRUMENTATION;
  return Val_unit;
}


/***************************************
 * :: memcheck
 ***************************************/


CAMLprim value
ml_Valgrind_Memcheck_make_mem_noaccess (value addr, value len)
{
  VALGRIND_MAKE_MEM_NOACCESS (Int_val (addr), Int_val (len));
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_make_mem_undefined (value addr, value len)
{
  VALGRIND_MAKE_MEM_UNDEFINED (Int_val (addr), Int_val (len));
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_make_mem_defined (value addr, value len)
{
  VALGRIND_MAKE_MEM_DEFINED (Int_val (addr), Int_val (len));
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_make_mem_defined_if_addressable (value addr, value len)
{
  VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE (Int_val (addr), Int_val (len));
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_create_block (value addr, value len, value desc)
{
  return Val_int (VALGRIND_CREATE_BLOCK (Int_val (addr), Int_val (len), String_val (desc)));
}

CAMLprim value
ml_Valgrind_Memcheck_discard (value blkindex)
{
  VALGRIND_DISCARD (blkindex);
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_check_mem_is_addressable (value addr, value len)
{
  return Val_int (VALGRIND_CHECK_MEM_IS_ADDRESSABLE (addr, len));
}

CAMLprim value
ml_Valgrind_Memcheck_check_mem_is_defined (value addr, value len)
{
  return Val_int (VALGRIND_CHECK_MEM_IS_DEFINED (addr, len));
}

CAMLprim value
ml_Valgrind_Memcheck_do_leak_check (void)
{
  VALGRIND_DO_LEAK_CHECK;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_do_added_leak_check (void)
{
#ifdef VALGRIND_DO_ADDED_LEAK_CHECK
  VALGRIND_DO_ADDED_LEAK_CHECK;
  return Val_unit;
#else
  failwith ("Not supported");
#endif
}

CAMLprim value
ml_Valgrind_Memcheck_do_changed_leak_check (void)
{
#ifdef VALGRIND_DO_CHANGED_LEAK_CHECK
  VALGRIND_DO_CHANGED_LEAK_CHECK;
  return Val_unit;
#else
  failwith ("Not supported");
#endif
}

CAMLprim value
ml_Valgrind_Memcheck_do_quick_leak_check (void)
{
  VALGRIND_DO_QUICK_LEAK_CHECK;
  return Val_unit;
}

CAMLprim value
ml_Valgrind_Memcheck_count_leaks (void)
{
  CAMLparam0 ();
  CAMLlocal1 (tuple);

  unsigned long leaked, dubious, reachable, suppressed;
  VALGRIND_COUNT_LEAKS (leaked, dubious, reachable, suppressed);

  tuple = caml_alloc (4, 0);
  Store_field (tuple, 0, leaked);
  Store_field (tuple, 1, dubious);
  Store_field (tuple, 2, reachable);
  Store_field (tuple, 3, suppressed);

  CAMLreturn (Val_unit);
}

CAMLprim value
ml_Valgrind_Memcheck_count_leak_blocks (void)
{
  CAMLparam0 ();
  CAMLlocal1 (tuple);

  unsigned long leaked, dubious, reachable, suppressed;
  VALGRIND_COUNT_LEAK_BLOCKS (leaked, dubious, reachable, suppressed);

  tuple = caml_alloc (4, 0);
  Store_field (tuple, 0, leaked);
  Store_field (tuple, 1, dubious);
  Store_field (tuple, 2, reachable);
  Store_field (tuple, 3, suppressed);

  CAMLreturn (Val_unit);
}

CAMLprim value
ml_Valgrind_Memcheck_get_vbits (value addr, value vbits, value nbytes)
{
  return Val_int (VALGRIND_GET_VBITS (addr, vbits, nbytes));
}

CAMLprim value
ml_Valgrind_Memcheck_set_vbits (value addr, value vbits, value nbytes)
{
  return Val_int (VALGRIND_SET_VBITS (addr, vbits, nbytes));
}


/***************************************
 * :: own functions
 ***************************************/

static int
compute_size (value obj, value_set &seen, int level = 0)
{
  if (Is_long (obj))
    return sizeof obj;

  if (seen.find (obj) != seen.end ())
    return sizeof obj;
  seen.insert (obj);

  header_t hd = Hd_val (obj);
  tag_t tag = Tag_hd (hd);
  int size = sizeof hd;

  switch (tag)
    {
    case 0 ... No_scan_tag:
      {
        mlsize_t wosize = Wosize_hd (hd);
        size += wosize * sizeof (value);

        for (int i = 0; i < Wosize_hd (hd); i++)
          size += compute_size (Field (obj, i), seen, level + 1);
        break;
      }

    case String_tag:
      size += caml_string_length (obj);
      break;

    default:
      printf ("unhandled tag: %d\n", tag);
      abort ();
    }

  return size;
}


CAMLprim value
ml_Valgrind_sizeof (value obj)
{
  value_set seen;
  int size = compute_size (obj, seen);
  return Val_int (size);
}


__END_DECLS
