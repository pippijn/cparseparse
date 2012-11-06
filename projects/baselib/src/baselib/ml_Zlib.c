#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <string.h>
#include <zlib.h>


static value
make_string (char const *s, size_t l)
{
  CAMLparam0 ();
  CAMLlocal1 (r);
  r = caml_alloc_string (l);
  memcpy (String_val (r), s, l);

  CAMLreturn (r);
}


CAMLprim value
ml_Zlib_compress (value s)
{
  CAMLparam1 (s);

  unsigned int sourceSize = caml_string_length (s);
  char const *source = String_val (s);

  uLongf dsize = sourceSize + (sourceSize * 0.1f) + 16;
  char destination[dsize];

  int result = compress ((unsigned char *)destination, &dsize, (unsigned char const *)source, sourceSize);

  if (result != Z_OK)
    failwith ("compress error occured");

  CAMLreturn (make_string (destination, dsize));
}

CAMLprim value
ml_Zlib_uncompress (value s, value length)
{
  CAMLparam1 (s);
  uLongf len = Int_val (length);

  unsigned int sourceSize = caml_string_length (s);
  char const *source = String_val (s);

  char destination[len];

  int result = uncompress ((unsigned char *)destination, &len, (const unsigned char *)source, sourceSize);

  if (result != Z_OK)
    failwith ("uncompress error occured");

  CAMLreturn (make_string (destination, len));
}
