#define POOL_BITSETS 1

#if POOL_BITSETS
#include <map>
static std::map<int, std::vector<bitset>> pools;
#endif


static inline void
finalize_bitset (bitset *set)
{
#if POOL_BITSETS
  pools[set->size ()].emplace_back (move (*set));
#endif
  set->~bitset ();
}

static inline bitset *
get_bitset (value self)
{
  return static_cast<bitset *> (Data_custom_val (self));
}


static struct custom_operations bitset_custom_ops = {
  // identifier
  (char *)"C++ bitset",

  // finalize
  [](value ptr) {
    finalize_bitset (get_bitset (ptr));
  },

  // compare
  [](value a, value b) {
    bitset &ba = *get_bitset (a);
    bitset &bb = *get_bitset (b);
    if (ba < bb)
      return -1;
    if (ba > bb)
      return 1;
    return 0;
  },

  // hash
  [](value ptr) {
    bitset &b = *get_bitset (ptr);
    intnat hash = 0;
    for (word w : b)
      hash ^= w;
    return hash;
  },

  // serialize
  [](value v, unsigned long *wsize_32, unsigned long *wsize_64) {
    bitset &set = *get_bitset (v);
    caml_serialize_int_4 (set.size ());
    if (word_size == 4)
      for (word w : set)
        caml_serialize_int_4 (w);
    else if (word_size == 8)
      for (word w : set)
        caml_serialize_int_8 (w);
    else
      failwith ("Unknown word size");

    *wsize_32 = 4 + set.size () * 4;
    *wsize_64 = 4 + set.size () * 8;
  },

  // deserialize
  [](void *dst) {
    uint32 words = caml_deserialize_uint_4 ();

    bitset &set = *new (dst) bitset (words);
    if (word_size == 4)
      {
        for (uint32 i = 0; i < words; i++)
          set[i] = caml_deserialize_uint_4 ();
        return 4ul + words * 4;
      }
    else if (word_size == 8)
      {
        for (uint32 i = 0; i < words; i++)
          set[i] = caml_deserialize_uint_8 ();
        return 4ul + words * 8;
      }

    failwith ("Unknown word size");
    return 0ul;
  },
};


static inline value
make_bitset (int size)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  int words = size / word_size + !!(size % word_size);
  v = caml_alloc_custom (&bitset_custom_ops, sizeof (bitset), 0, 1);

#if POOL_BITSETS
  std::vector<bitset> &pool = pools[words];
  if (!pool.empty ())
    {
      new (Data_custom_val (v)) bitset (move (pool.back ()));
      pool.pop_back ();
    }
  else
#endif
    new (Data_custom_val (v)) bitset (words);

  CAMLreturn (v);
}

static inline value
copy_bitset (bitset &other)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  v = caml_alloc_custom (&bitset_custom_ops, sizeof (bitset), 0, 1);

#if POOL_BITSETS
  std::vector<bitset> &pool = pools[other.size ()];
  if (!pool.empty ())
    {
      *new (Data_custom_val (v)) bitset (move (pool.back ())) = other;
      pool.pop_back ();
    }
  else
#endif
    new (Data_custom_val (v)) bitset (other);

  CAMLreturn (v);
}
