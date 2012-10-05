static inline bitset *
get_bitset (value self)
{
  return reinterpret_cast<bitset *> (Bp_val (self));
}

static inline value
make_bitset (int size)
{
  return Val_bp (new bitset (size / word_size + !!(size % word_size)));
}

static inline value
copy_bitset (bitset &other)
{
  return Val_bp (new bitset (other));
}
