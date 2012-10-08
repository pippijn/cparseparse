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
#include <vector>

typedef unsigned long word;
typedef std::vector<word> bitset;
static size_t const word_size = sizeof (word);


#include "ml_BitSet-custom.h"
//#include "ml_BitSet-raw.h"


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
ml_BitSet_copy (value self)
{
  return copy_bitset (*get_bitset (self));
}


CAMLprim value
ml_BitSet_count (value self)
{
  int count = 0;
  for (word w : *get_bitset (self))
    count += count_bits (w);
  return Val_int (count);
}


CAMLprim value
ml_BitSet_set (value self, value bit_val)
{
  bitset &set = *get_bitset (self);
  int bit = Int_val (bit_val);
  assert (set.size () > bit / word_size);
  set[bit / word_size] |= 1 << (bit % word_size);
  return Val_unit;
}


CAMLprim value
ml_BitSet_is_set (value self, value bit_val)
{
  bitset &set = *get_bitset (self);
  int bit = Int_val (bit_val);
  assert (set.size () > bit / word_size);
  return Val_bool (set[bit / word_size] & 1 << (bit % word_size));
}


CAMLprim value
ml_BitSet_unite (value dest, value source)
{
  bitset &dst = *get_bitset (dest);
  bitset &src = *get_bitset (source);
  assert (dst.size () >= src.size ());
  for (size_t i = 0; i < src.size (); i++)
    dst[i] |= src[i];
  return Val_unit;
}


CAMLprim value
ml_BitSet_differentiate (value dest, value source)
{
  bitset &dst = *get_bitset (dest);
  bitset &src = *get_bitset (source);
  assert (dst.size () >= src.size ());
  for (size_t i = 0; i < src.size (); i++)
    dst[i] &= ~src[i];
  return Val_unit;
}


CAMLprim value
ml_BitSet_merge (value dest, value source)
{
  bool changed = false;
  bitset &dst = *get_bitset (dest);
  bitset &src = *get_bitset (source);
  assert (dst.size () >= src.size ());
  for (size_t i = 0; i < src.size (); i++)
    if ((dst[i] | src[i]) != dst[i])
      {
        dst[i] |= src[i];
        changed = true;
      }
  return Val_bool (changed);
}


CAMLprim value
ml_BitSet_clear (value self)
{
  for (word &w : *get_bitset (self))
    w = 0;
  return Val_unit;
}


CAMLprim value
ml_BitSet_assign (value dest, value source)
{
  bitset &dst = *get_bitset (dest);
  bitset &src = *get_bitset (source);
  assert (dst.size () == src.size ());
  dst = src;
  return Val_unit;
}


__END_DECLS
