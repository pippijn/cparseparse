extern "C" {
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#define STR(V) STR_(V)
#define STR_(V) #V
#define assert(cond) do { if (!(cond)) failwith ("Assertion failed in " __FILE__ ":" STR(__LINE__) ": " #cond); } while (0)

#include <cstdio>
#include <cstring>
#include <vector>

#ifdef HAS_OLD_CXX
#  include <boost/foreach.hpp>
#endif

typedef unsigned long word;
typedef std::vector<word> bitset;
static size_t const word_size = sizeof (word);


#ifdef HAS_OLD_CXX
#  include "ml_BitSet-raw.h"
#else
#  include "ml_BitSet-custom.h"
#endif

static inline size_t
count_bits (unsigned long n)
{
  size_t c;   // c accumulates the total bits set in v

  for (c = 0; n; c++)
    n &= n - 1;         // clear the least significant bit set
  return c;
}


__BEGIN_DECLS


CAMLprim value
ml_BitSet_create (value size)
{
  return make_bitset (Int_val (size));
}


CAMLprim value
ml_BitSet_cardinal (value self)
{
  int count = 0;
#ifdef HAS_OLD_CXX
  BOOST_FOREACH (word w, *get_bitset (self))
#else
  for (word w : *get_bitset (self))
#endif
    count += count_bits (w);
  return Val_int (count);
}


CAMLprim value
ml_BitSet_add (value bit_val, value self)
{
  bitset &set = *get_bitset (self);
  int bit = Int_val (bit_val);
  assert (set.size () > bit / word_size);
  set[bit / word_size] |= 1 << (bit % word_size);
  return Val_unit;
}


CAMLprim value
ml_BitSet_mem (value bit_val, value self)
{
  bitset &set = *get_bitset (self);
  int bit = Int_val (bit_val);
  if (set.size () <= bit / word_size)
    return Val_bool (false);
  return Val_bool (set[bit / word_size] & 1 << (bit % word_size));
}


__END_DECLS
