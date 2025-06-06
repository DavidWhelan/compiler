/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Structured input, compact format */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"

/* Item on the stack with defined operation */
struct intern_item {
  volatile value * dest;
  intnat arg;
  enum {
    OReadItems, /* read arg items and store them in dest[0], dest[1], ... */
    OFreshOID,  /* generate a fresh OID and store it in *dest */
    OShift      /* offset *dest by arg */
  } op;
};

/* FIXME: This is duplicated in two other places, with the only difference of
   the type of elements stored in the stack. Possible solution in C would
   be to instantiate stack these function via. C preprocessor macro.
 */

#define INTERN_STACK_INIT_SIZE 256
#define INTERN_STACK_MAX_SIZE (1024*1024*100)

struct caml_intern_state {

  const unsigned char * intern_src;
  /* Reading pointer in block holding input data. */

  unsigned char * intern_input;
  /* Pointer to beginning of block holding input data,
    if non-NULL this pointer will be freed by the cleanup function.

    Allocated using malloc/free instead of stat_alloc/stat_free to
    satisfy the expectations of caml_input_value_from_malloc users.
  */

  asize_t obj_counter;
  /* Count how many objects seen so far */

  value * intern_obj_table;
  /* The pointers to objects already seen */

  struct intern_item intern_stack_init[INTERN_STACK_INIT_SIZE];
  /* The initial intern stack */

  struct intern_item * intern_stack;
  /* Initially points to [intern_stack_init] */

  struct intern_item * intern_stack_limit;

  header_t * intern_dest;
  /* Writing pointer in destination block. Only used when the object fits in
     the minor heap. */

  char compressed;
  /* 1 if the compressed format is in use, 0 otherwise */
};

static void init_intern_stack(struct caml_intern_state* s)
{
  /* (Re)initialize the globals for next time around */
  s->intern_stack = s->intern_stack_init;
  s->intern_stack_limit = s->intern_stack + INTERN_STACK_INIT_SIZE;
}

static struct caml_intern_state* init_intern_state (void)
{
  Caml_check_caml_state();
  struct caml_intern_state* s;

  if (Caml_state->intern_state != NULL)
    return Caml_state->intern_state;

  s = caml_stat_alloc(sizeof(struct caml_intern_state));

  s->intern_src = NULL;
  s->intern_input = NULL;
  s->obj_counter = 0;
  s->intern_obj_table = NULL;
  s->intern_dest = NULL;
  init_intern_stack(s);

  Caml_state->intern_state = s;
  return s;
}

static struct caml_intern_state* get_intern_state (void)
{
  Caml_check_caml_state();

  if (Caml_state->intern_state == NULL)
    caml_fatal_error (
      "intern_state not initialized: it is likely that a caml_deserialize_* "
      "function was called without going through caml_input_*."
      );

  return Caml_state->intern_state;
}

void caml_free_intern_state (void)
{
  if (Caml_state->intern_state != NULL) {
    caml_stat_free(Caml_state->intern_state);
    Caml_state->intern_state = NULL;
  }
}

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset);

CAMLnoret static void intern_bad_code_pointer(unsigned char digest[16]);

Caml_inline unsigned char read8u(struct caml_intern_state* s)
{ return *s->intern_src++; }

Caml_inline signed char read8s(struct caml_intern_state* s)
{ return *s->intern_src++; }

Caml_inline uint16_t read16u(struct caml_intern_state* s)
{
  uint16_t res = (s->intern_src[0] << 8) + s->intern_src[1];
  s->intern_src += 2;
  return res;
}

Caml_inline int16_t read16s(struct caml_intern_state* s)
{
  int16_t res = (s->intern_src[0] << 8) + s->intern_src[1];
  s->intern_src += 2;
  return res;
}

Caml_inline uint32_t read32u(struct caml_intern_state* s)
{
  uint32_t res =
    ((uint32_t)(s->intern_src[0]) << 24) + (s->intern_src[1] << 16)
    + (s->intern_src[2] << 8) + s->intern_src[3];
  s->intern_src += 4;
  return res;
}

Caml_inline int32_t read32s(struct caml_intern_state* s)
{
  int32_t res =
    ((uint32_t)(s->intern_src[0]) << 24) + (s->intern_src[1] << 16)
    + (s->intern_src[2] << 8) + s->intern_src[3];
  s->intern_src += 4;
  return res;
}

#ifdef ARCH_SIXTYFOUR
static uintnat read64u(struct caml_intern_state* s)
{
  uintnat res =
    ((uintnat) (s->intern_src[0]) << 56)
    + ((uintnat) (s->intern_src[1]) << 48)
    + ((uintnat) (s->intern_src[2]) << 40)
    + ((uintnat) (s->intern_src[3]) << 32)
    + ((uintnat) (s->intern_src[4]) << 24)
    + ((uintnat) (s->intern_src[5]) << 16)
    + ((uintnat) (s->intern_src[6]) << 8)
    + (uintnat) (s->intern_src[7]);
  s->intern_src += 8;
  return res;
}
#endif

static int readvlq(struct caml_intern_state* s, /*out*/ uintnat * res)
{
  unsigned char c = read8u(s);
  uintnat n = c & 0x7F;
  int retcode = 0;
  while ((c & 0x80) != 0) {
    c = read8u(s);
    uintnat n7 = n << 7;
    if (n != n7 >> 7) retcode = -1;
    n = n7 | (c & 0x7F);
  }
  if (res) *res = n;
  return retcode;
}

Caml_inline void readblock(struct caml_intern_state* s,
                           void * dest, intnat len)
{
  memcpy(dest, s->intern_src, len);
  s->intern_src += len;
}

static void intern_init(struct caml_intern_state* s, const void * src,
                        void * input)
{
  CAMLassert (s);
  /* This is asserted at the beginning of demarshaling primitives.
     If it fails, it probably means that an exception was raised
     without calling intern_cleanup() during the previous demarshaling. */
  CAMLassert(s->intern_input == NULL);
  CAMLassert(s->intern_obj_table == NULL);
  s->intern_src = src;
  s->intern_input = input;
}

/* Free the recursion stack if needed */
static void intern_free_stack(struct caml_intern_state* s)
{
  if (s->intern_stack != s->intern_stack_init) {
    caml_stat_free(s->intern_stack);
    init_intern_stack(s);
  }
}

static void intern_cleanup(struct caml_intern_state* s)
{
  if (s->intern_input != NULL) {
     free(s->intern_input);
     s->intern_input = NULL;
  }
  if (s->intern_obj_table != NULL) {
    caml_stat_free(s->intern_obj_table);
    s->intern_obj_table = NULL;
  }
  s->intern_dest = NULL;
  /* free the recursion stack */
  intern_free_stack(s);
}

CAMLnoret static void intern_failwith2(const char * fun_name, const char * msg)
{
  caml_failwith_value(caml_alloc_sprintf("%s: %s", fun_name, msg));
}

CAMLnoret static void
intern_cleanup_failwith3(struct caml_intern_state* s, const char * fun_name,
                         const char * msg, const char * arg)
{
  /* Format the message before calling intern_cleanup, as arg may point
     into intern buffers */
  value v = caml_alloc_sprintf("%s: %s %s", fun_name, msg, arg);
  intern_cleanup(s);
  caml_failwith_value(v);
}

static void readfloat(struct caml_intern_state* s,
                      double * dest, unsigned int code)
{
  if (sizeof(double) != 8) {
    intern_cleanup(s);
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock(s, (char *) dest, 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_BIG) Reverse_64(dest, dest);
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_LITTLE) Reverse_64(dest, dest);
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_LITTLE)
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x01234567);
  else
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x76543210);
#endif
}

/* [len] is a number of floats */
static void readfloats(struct caml_intern_state* s,
                       double * dest, mlsize_t len, unsigned int code)
{
  if (sizeof(double) != 8) {
    intern_cleanup(s);
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock(s, (char *) dest, len * 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_ARRAY8_BIG &&
      code != CODE_DOUBLE_ARRAY32_BIG) {
    for (mlsize_t i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_ARRAY8_LITTLE &&
      code != CODE_DOUBLE_ARRAY32_LITTLE) {
    for (mlsize_t i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_ARRAY8_LITTLE ||
      code == CODE_DOUBLE_ARRAY32_LITTLE) {
    for (mlsize_t i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x01234567);
  } else {
    for (mlsize_t i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x76543210);
  }
#endif
}

CAMLnoret static void intern_stack_overflow(struct caml_intern_state* s)
{
  caml_gc_message (0x04, "Stack overflow in un-marshaling value\n");
  intern_cleanup(s);
  caml_raise_out_of_memory();
}

static struct intern_item * intern_resize_stack(struct caml_intern_state* s,
                                                struct intern_item * sp)
{
  asize_t newsize = 2 * (s->intern_stack_limit - s->intern_stack);
  asize_t sp_offset = sp - s->intern_stack;
  struct intern_item * newstack;

  if (newsize >= INTERN_STACK_MAX_SIZE) intern_stack_overflow(s);
  newstack = caml_stat_calloc_noexc(newsize, sizeof(struct intern_item));
  if (newstack == NULL) intern_stack_overflow(s);

  /* Copy items from the old stack to the new stack */
  memcpy(newstack, s->intern_stack,
         sizeof(struct intern_item) * sp_offset);

  /* Free to old stack if it is not the initial stack */
  if (s->intern_stack != s->intern_stack_init)
    caml_stat_free(s->intern_stack);

  s->intern_stack = newstack;
  s->intern_stack_limit = newstack + newsize;
  return newstack + sp_offset;
}

/* Convenience macros for requesting operation on the stack */
#define PushItem(s)                                                     \
  do {                                                                  \
    sp++;                                                               \
    if (sp >= s->intern_stack_limit) sp = intern_resize_stack(s, sp);   \
  } while(0)

#define ReadItems(s,_dest,_n)                                           \
  do {                                                                  \
    if (_n > 0) {                                                       \
      PushItem(s);                                                      \
      sp->op = OReadItems;                                              \
      sp->dest = _dest;                                                 \
      sp->arg = _n;                                                     \
    }                                                                   \
  } while(0)

static void intern_alloc_storage(struct caml_intern_state* s, mlsize_t whsize,
                                 mlsize_t num_objects)
{
  mlsize_t wosize;
  value v;

  if (whsize == 0) {
    CAMLassert (s->intern_obj_table == NULL);
    return;
  }
  wosize = Wosize_whsize(whsize);

  if (wosize <= Max_young_wosize && wosize != 0) {
    /* don't track bulk allocation in minor heap with statmemprof;
     * individual block allocations are tracked instead */
    Alloc_small(v, wosize, String_tag, Alloc_small_enter_GC_no_track);
    s->intern_dest = (header_t *) Hp_val(v);
  } else {
    CAMLassert (s->intern_dest == NULL);
  }
  s->obj_counter = 0;
  if (num_objects > 0) {
    s->intern_obj_table =
      (value *) caml_stat_alloc_noexc(num_objects * sizeof(value));
    if (s->intern_obj_table == NULL) {
      intern_cleanup(s);
      caml_raise_out_of_memory();
    }
  } else {
    CAMLassert(s->intern_obj_table == NULL);
  }

  return;
}

static value intern_alloc_obj(struct caml_intern_state* s, caml_domain_state* d,
                              mlsize_t wosize, tag_t tag)
{
  void* p;

  if (s->intern_dest) {
    CAMLassert ((value*)s->intern_dest >= d->young_start &&
                (value*)s->intern_dest < d->young_end);
    p = s->intern_dest;
    *s->intern_dest = Make_header (wosize, tag, 0);
    caml_memprof_sample_block(Val_hp(p), wosize, 1 + wosize,
                              CAML_MEMPROF_SRC_MARSHAL);
    s->intern_dest += 1 + wosize;
  } else {
    p = caml_shared_try_alloc(d->shared_heap, wosize, tag,
                              0 /* no reserved bits */);
    if (p == NULL) {
      intern_cleanup (s);
      caml_raise_out_of_memory();
    }
    d->allocated_words += Whsize_wosize(wosize);
    d->allocated_words_direct += Whsize_wosize(wosize);
    Hd_hp(p) = Make_header (wosize, tag, caml_global_heap_state.MARKED);
    caml_memprof_sample_block(Val_hp(p), wosize,
                              Whsize_wosize(wosize),
                              CAML_MEMPROF_SRC_MARSHAL);
  }
  return Val_hp(p);
}

static void intern_rec(struct caml_intern_state* s,
                       const char * fun_name,
                       volatile value *dest)
{
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, ofs_ind;
  value v;
  asize_t ofs;
  header_t header;
  unsigned char digest[16];
  struct custom_operations * ops;
  char * codeptr;
  struct intern_item * sp;
  caml_domain_state * d = Caml_state;

  sp = s->intern_stack;

  /* Initially let's try to read the first object from the stream */
  ReadItems(s, dest, 1);

  /* The un-marshaler loop, the recursion is unrolled */
  while(sp != s->intern_stack) {

  /* Interpret next item on the stack */
  dest = sp->dest;
  switch (sp->op) {
  case OFreshOID:
    /* Refresh the object ID */
    /* but do not do it for predefined exception slots */
    if (Long_val(Field((value)dest, 1)) >= 0)
      caml_set_oo_id((value)dest);
    /* Pop item and iterate */
    sp--;
    break;
  case OShift:
    /* Shift value by an offset */
    *dest += sp->arg;
    /* Pop item and iterate */
    sp--;
    break;
  case OReadItems:
    /* Pop item */
    sp->dest++;
    if (--(sp->arg) == 0) sp--;
    /* Read a value and set v to this value */
  code = read8u(s);
  if (code >= PREFIX_SMALL_INT) {
    if (code >= PREFIX_SMALL_BLOCK) {
      /* Small block */
      tag = code & 0xF;
      size = (code >> 4) & 0x7;
    read_block:
      if (size == 0) {
        v = Atom(tag);
      } else {
        v = intern_alloc_obj (s, d, size, tag);
        if (s->intern_obj_table != NULL)
          s->intern_obj_table[s->obj_counter++] = v;
        /* For objects, we need to freshen the oid */
        if (tag == Object_tag) {
          CAMLassert(size >= 2);
          /* Request to read rest of the elements of the block */
          ReadItems(s, &Field(v, 2), size - 2);
          /* Request freshing OID */
          PushItem(s);
          sp->op = OFreshOID;
          sp->dest = (value*) v;
          sp->arg = 1;
          /* Finally read first two block elements: method table and old OID */
          ReadItems(s, &Field(v, 0), 2);
        } else
          /* If it's not an object then read the contents of the block */
          ReadItems(s, &Field(v, 0), size);
      }
    } else {
      /* Small integer */
      v = Val_int(code & 0x3F);
    }
  } else {
    if (code >= PREFIX_SMALL_STRING) {
      /* Small string */
      len = (code & 0x1F);
    read_string:
      size = (len + sizeof(value)) / sizeof(value);
      v = intern_alloc_obj (s, d, size, String_tag);
      if (s->intern_obj_table != NULL)
        s->intern_obj_table[s->obj_counter++] = v;
      Field(v, size - 1) = 0;
      ofs_ind = Bsize_wsize(size) - 1;
      Byte(v, ofs_ind) = ofs_ind - len;
      readblock(s, (char *)String_val(v), len);
    } else {
      switch(code) {
      case CODE_INT8:
        v = Val_long(read8s(s));
        break;
      case CODE_INT16:
        v = Val_long(read16s(s));
        break;
      case CODE_INT32:
        v = Val_long(read32s(s));
        break;
      case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
        v = Val_long((intnat) (read64u(s)));
        break;
#else
        intern_cleanup(s);
        intern_failwith2(fun_name, "integer too large");
        break;
#endif
      case CODE_SHARED8:
        ofs = read8u(s);
      read_shared:
        if (!s->compressed) ofs = s->obj_counter - ofs;
        CAMLassert (ofs < s->obj_counter);
        CAMLassert (s->intern_obj_table != NULL);
        v = s->intern_obj_table[ofs];
        break;
      case CODE_SHARED16:
        ofs = read16u(s);
        goto read_shared;
      case CODE_SHARED32:
        ofs = read32u(s);
        goto read_shared;
#ifdef ARCH_SIXTYFOUR
      case CODE_SHARED64:
        ofs = read64u(s);
        goto read_shared;
#endif
      case CODE_BLOCK32:
        header = (header_t) read32u(s);
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
#ifdef ARCH_SIXTYFOUR
      case CODE_BLOCK64:
        header = (header_t) read64u(s);
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
#endif
      case CODE_STRING8:
        len = read8u(s);
        goto read_string;
      case CODE_STRING32:
        len = read32u(s);
        goto read_string;
#ifdef ARCH_SIXTYFOUR
      case CODE_STRING64:
        len = read64u(s);
        goto read_string;
#endif
      case CODE_DOUBLE_LITTLE:
      case CODE_DOUBLE_BIG:
        v = intern_alloc_obj (s, d, Double_wosize, Double_tag);
        if (s->intern_obj_table != NULL)
          s->intern_obj_table[s->obj_counter++] = v;
        readfloat(s, (double *) v, code);
        break;
      case CODE_DOUBLE_ARRAY8_LITTLE:
      case CODE_DOUBLE_ARRAY8_BIG:
        len = read8u(s);
      read_double_array:
        size = len * Double_wosize;
        v = intern_alloc_obj (s, d, size, Double_array_tag);
        if (s->intern_obj_table != NULL)
          s->intern_obj_table[s->obj_counter++] = v;
        readfloats(s, (double *) v, len, code);
        break;
      case CODE_DOUBLE_ARRAY32_LITTLE:
      case CODE_DOUBLE_ARRAY32_BIG:
        len = read32u(s);
        goto read_double_array;
#ifdef ARCH_SIXTYFOUR
      case CODE_DOUBLE_ARRAY64_LITTLE:
      case CODE_DOUBLE_ARRAY64_BIG:
        len = read64u(s);
        goto read_double_array;
#endif
      case CODE_CODEPOINTER:
        ofs = read32u(s);
        readblock(s, digest, 16);
        codeptr = intern_resolve_code_pointer(digest, ofs);
        if (codeptr != NULL) {
          v = (value) codeptr;
        } else {
          const value * function_placeholder =
            caml_named_value ("Debugger.function_placeholder");
          if (function_placeholder != NULL) {
            /* Use the code pointer from the "placeholder" function */
            v = (value) Code_val(*function_placeholder);
          } else {
            intern_cleanup(s);
            intern_bad_code_pointer(digest);
          }
        }
        break;
      case CODE_INFIXPOINTER:
        ofs = read32u(s);
        /* Read a value to *dest, then offset *dest by ofs */
        PushItem(s);
        sp->dest = dest;
        sp->op = OShift;
        sp->arg = ofs;
        ReadItems(s, dest, 1);
        continue;  /* with next iteration of main loop, skipping *dest = v */
      case OLD_CODE_CUSTOM:
        intern_cleanup(s);
        intern_failwith2(fun_name, "custom blocks serialized with "
                         "OCaml 4.08.0 (or prior) are no longer supported");
        break;
      case CODE_CUSTOM_LEN:
      case CODE_CUSTOM_FIXED: {
        uintnat expected_size, temp_size;
        const char * name = (const char *) s->intern_src;
        ops = caml_find_custom_operations(name);
        if (ops == NULL) {
          intern_cleanup_failwith3
            (s, fun_name, "unknown custom block identifier", name);
        }
        if (code == CODE_CUSTOM_FIXED && ops->fixed_length == NULL) {
          intern_cleanup_failwith3
            (s, fun_name, "expected a fixed-size custom block", name);
        }
        while (*s->intern_src++ != 0) /*nothing*/;  /*skip identifier*/
#ifdef ARCH_SIXTYFOUR
        if (code == CODE_CUSTOM_FIXED) {
          expected_size = ops->fixed_length->bsize_64;
        } else {
          s->intern_src += 4;
          expected_size = read64u(s);
        }
#else
        if (code == CODE_CUSTOM_FIXED) {
          expected_size = ops->fixed_length->bsize_32;
        } else {
          expected_size = read32u(s);
          s->intern_src += 8;
        }
#endif
        temp_size = 1 + (expected_size + sizeof(value) - 1) / sizeof(value);
        v = intern_alloc_obj(s, d, temp_size, Custom_tag);
        Custom_ops_val(v) = ops;
        size = ops->deserialize(Data_custom_val(v));
        if (size != expected_size) {
          intern_cleanup_failwith3
            (s, fun_name, "error while deserializing custom block", name);
        }
        if (s->intern_obj_table != NULL)
          s->intern_obj_table[s->obj_counter++] = v;
        if (ops->finalize != NULL && Is_young(v)) {
          /* Remember that the block has a finalizer. */
          add_to_custom_table (&d->minor_tables->custom, v, 0, 1);
        }
        break;
      }
      default:
        intern_cleanup(s);
        intern_failwith2(fun_name, "ill-formed message");
      }
    }
  }
  /* end of case OReadItems */
  /* The following direct-assignment to [*dest] rather than [caml_modify] is
     safe since either it is the case that

       1. [dest] points within the minor heap of the current domain or
       2. [dest] is a freshly-allocated major heap block, but not yet visible
          to the GC, and if [v] is a block, then it is also in the major heap.
          So no major to minor heap references are created.

     Moreover, since [*dest] is uninitialised, using `caml_modify` is
     incorrect; the deletion barrier will mark the old uninitialised value and
     may crash. */
  *dest = v;
  break;
  }
  }
  /* We are done. Cleanup the stack and leave the function */
  intern_free_stack(s);
}

static value intern_end(struct caml_intern_state* s, value res)
{
  CAMLparam1(res);
  /* Free everything */
  intern_cleanup(s);

  /* Give gc a chance to run, and run memprof callbacks */
  caml_process_pending_actions();

  CAMLreturn(res);
}

/* Parsing the header */

struct marshal_header {
  uint32_t magic;
  int header_len;
  uintnat data_len;
  uintnat uncompressed_data_len;
  uintnat num_objects;
  uintnat whsize;
  int compressed;
};

static void caml_parse_header(struct caml_intern_state* s,
                              const char * fun_name,
                              /*out*/ struct marshal_header * h)
{
  h->magic = read32u(s);
  switch(h->magic) {
  case Intext_magic_number_small:
    h->header_len = 20;
    h->compressed = 0;
    h->data_len = h->uncompressed_data_len = read32u(s);
    h->num_objects = read32u(s);
#ifdef ARCH_SIXTYFOUR
    read32u(s);
    h->whsize = read32u(s);
#else
    h->whsize = read32u(s);
    read32u(s);
#endif
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    h->header_len = 32;
    h->compressed = 0;
    read32u(s);
    h->data_len = h->uncompressed_data_len = read64u(s);
    h->num_objects = read64u(s);
    h->whsize = read64u(s);
#else
    intern_failwith2
      (fun_name, "object too large to be read back on a 32-bit platform");
#endif
    break;
  case Intext_magic_number_compressed:
    h->header_len = read8u(s) & 0x3F;
    h->compressed = 1;
    int overflow = 0;
    overflow |= readvlq(s, &h->data_len);
    overflow |= readvlq(s, &h->uncompressed_data_len);
    overflow |= readvlq(s, &h->num_objects);
#ifdef ARCH_SIXTYFOUR
    (void) readvlq(s, NULL);
    overflow |= readvlq(s, &h->whsize);
#else
    overflow |= readvlq(s, &h->whsize);
    (void) readvlq(s, NULL);
#endif
    if (overflow) {
      intern_failwith2
        (fun_name, "object too large to be read back on this platform");
    }
    break;
  default:
    intern_failwith2(fun_name, "bad object");
  }
}

/* Decompress the input if needed.
   Must be called after [intern_init].
   Should preferably be called before [intern_alloc_storage]
   when the memory block for the compressed input can be freed
   before more memory is allocated. */

size_t (*caml_intern_decompress_input)(unsigned char *,
                                       uintnat,
                                       const unsigned char *,
                                       uintnat) = NULL;

static void intern_decompress_input(struct caml_intern_state * s,
                                    const char * fun_name,
                                    struct marshal_header * h)
{
  s->compressed = h->compressed;
  if (! h->compressed) return;
  if (caml_intern_decompress_input != NULL) {
    unsigned char * blk = malloc(h->uncompressed_data_len);
    if (blk == NULL) {
      intern_cleanup(s);
      caml_raise_out_of_memory();
    }
    size_t res =
      caml_intern_decompress_input(blk,
                                   h->uncompressed_data_len,
                                   s->intern_src,
                                   h->data_len);
    if (res != h->uncompressed_data_len) {
      free(blk);
      intern_cleanup(s);
      intern_failwith2(fun_name, "decompression error");
    }
    if (s->intern_input != NULL) free(s->intern_input);
    s->intern_input = blk;  /* to be freed at end of demarshaling */
    s->intern_src = blk;
  } else {
    intern_cleanup(s);
    intern_failwith2(fun_name, "compressed object, cannot decompress");
  }
}

/* Reading from a channel */

value caml_input_val(struct channel *chan)
{
  intnat r;
  char header[MAX_INTEXT_HEADER_SIZE];
  struct marshal_header h;
  char * block;
  value res;
  struct caml_intern_state* s = init_intern_state ();

  if (! caml_channel_binary_mode(chan))
    caml_failwith("input_value: not a binary channel");
  /* Read the magic number and determine the size of the header */
  r = caml_really_getblock(chan, header, 5);
  if (r == 0)
    caml_raise_end_of_file();
  else if (r < 5)
    caml_failwith("input_value: truncated object");
  s->intern_src = (unsigned char *) header;
  int hlen;
  switch (read32u(s)) {
  case Intext_magic_number_big:
    hlen = 32; break;
  case Intext_magic_number_compressed:
    hlen = read8u(s) & 0x3F; break;
  default:
    hlen = 20; break;
  }
  /* Read the remainder of the header */
  CAMLassert (hlen > 5);
  if (caml_really_getblock(chan, header + 5, hlen - 5) < hlen - 5)
    caml_failwith("input_value: truncated object");
  /* Parse the full header */
  s->intern_src = (unsigned char *) header;
  caml_parse_header(s, "input_value", &h);
  /* Read block from channel */
  /* During channel I/O, concurrent [caml_input_val] operations
     can take place (via context switching in systhreads),
     and the context [s] may change.  So, wait until all I/O is over
     before using the context [s] again. */
  block = malloc(h.data_len);
  if (block == NULL) {
    caml_raise_out_of_memory();
  }
  if (caml_really_getblock(chan, block, h.data_len) < h.data_len) {
    free(block);
    caml_failwith("input_value: truncated object");
  }
  /* Initialize global state */
  intern_init(s, block, block);
  intern_decompress_input(s, "input_value", &h);
  intern_alloc_storage(s, h.whsize, h.num_objects);
  /* Fill it in */
  intern_rec(s, "input_value", &res);
  return intern_end(s, res);
}

CAMLprim value caml_input_value(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  caml_channel_lock(chan);
  res = caml_input_val(chan);
  caml_channel_unlock(chan);
  CAMLreturn (res);
}

/* Reading from memory-resident blocks */

/* XXX KC: Unused primitive. Remove with bootstrap. */
CAMLprim value caml_input_value_to_outside_heap(value vchan)
{
  return caml_input_value(vchan);
}

CAMLexport value caml_input_val_from_bytes(value str, intnat ofs)
{
  CAMLparam1 (str);
  CAMLlocal1 (obj);
  struct marshal_header h;
  struct caml_intern_state* s = init_intern_state ();

  /* Initialize global state */
  intern_init(s, &Byte_u(str, ofs), NULL);
  caml_parse_header(s, "input_val_from_string", &h);
  if (ofs + h.header_len + h.data_len > caml_string_length(str))
    caml_failwith("input_val_from_string: bad length");
  /* Allocate result */
  intern_alloc_storage(s, h.whsize, h.num_objects);
  s->intern_src = &Byte_u(str, ofs + h.header_len); /* If a GC occurred */
  /* Decompress if needed */
  intern_decompress_input(s, "input_val_from_string", &h);
  /* Fill it in */
  intern_rec(s, "input_val_from_string", &obj);
  CAMLreturn (intern_end(s, obj));
}

CAMLprim value caml_input_value_from_bytes(value str, value ofs)
{
  return caml_input_val_from_bytes(str, Long_val(ofs));
}

static value input_val_from_block(struct caml_intern_state* s,
                                  struct marshal_header * h)
{
  value obj;
  /* Decompress if needed */
  intern_decompress_input(s, "input_val_from_block", h);
  /* Allocate result */
  intern_alloc_storage(s, h->whsize, h->num_objects);
  /* Fill it in */
  intern_rec(s, "input_val_from_block", &obj);
  return (intern_end(s, obj));
}

CAMLexport value caml_input_value_from_malloc(char * data, intnat ofs)
{
  struct marshal_header h;
  struct caml_intern_state* s = init_intern_state ();

  intern_init(s, data + ofs, data);
  caml_parse_header(s, "input_value_from_malloc", &h);
  return input_val_from_block(s, &h);
}

/* [len] is a number of bytes */
CAMLexport value caml_input_value_from_block(const char * data, intnat len)
{
  struct marshal_header h;
  struct caml_intern_state* s = init_intern_state ();

  /* Initialize global state */
  intern_init(s, data, NULL);
  caml_parse_header(s, "input_value_from_block", &h);
  if (h.header_len + h.data_len > len)
    caml_failwith("input_val_from_block: bad length");
  return input_val_from_block(s, &h);
}

/* [ofs] is a [value] that represents a number of bytes
   result is a [value] that represents a number of bytes
   To handle all marshaling formats,
   we assume 16 bytes are available at [buff + ofs],
   and we return the data size + the length of the part of the header
   that remains to be read.
   16 bytes are necessary and sufficient because:
   - for the "small" model: the length is at positions 4 to 7
   - for the "big" model: the length is at positions 8 to 15
   - for the "compressed" model: the length is at positions 5 to at most 14
   16 bytes is not too much because the smallest marshalled object
   is 20 bytes long (a small integer in the "compressed" model),
   so we're not reading past the end of the data.
 */

CAMLprim value caml_marshal_data_size(value buff, value ofs)
{
  uint32_t magic;
  int header_len;
  uintnat data_len;
  struct caml_intern_state *s = init_intern_state ();

  s->intern_src = &Byte_u(buff, Long_val(ofs));
  magic = read32u(s);
  switch(magic) {
  case Intext_magic_number_small:
    header_len = 20;
    data_len = read32u(s);
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    header_len = 32;
    read32u(s);
    data_len = read64u(s);
#else
    caml_failwith("Marshal.data_size: "
                  "object too large to be read back on a 32-bit platform");
#endif
    break;
  case Intext_magic_number_compressed:
    header_len = read8u(s) & 0x3F;
    if (readvlq(s, &data_len) != 0)
      caml_failwith("Marshal.data_size: "
                    "object too large to be read back on this platform");
    break;
  default:
    caml_failwith("Marshal.data_size: bad object");
  }
  return Val_long((header_len - 16) + data_len);
}

/* Resolution of code pointers */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset)
{
  struct code_fragment * cf = caml_find_code_fragment_by_digest(digest);
  if (cf != NULL && cf->code_start + offset < cf->code_end)
    return cf->code_start + offset;
  else
    return NULL;
}

static void intern_bad_code_pointer(unsigned char digest[16])
{
  char msg[256];
  snprintf(msg, sizeof(msg),
               "input_value: unknown code module "
               "%02X%02X%02X%02X%02X%02X%02X%02X"
               "%02X%02X%02X%02X%02X%02X%02X%02X",
          digest[0], digest[1], digest[2], digest[3],
          digest[4], digest[5], digest[6], digest[7],
          digest[8], digest[9], digest[10], digest[11],
          digest[12], digest[13], digest[14], digest[15]);
  caml_failwith(msg);
}

/* Functions for writing user-defined marshallers */

CAMLexport int caml_deserialize_uint_1(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read8u(s);
}

CAMLexport int caml_deserialize_sint_1(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read8s(s);
}

CAMLexport int caml_deserialize_uint_2(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read16u(s);
}

CAMLexport int caml_deserialize_sint_2(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read16s(s);
}

CAMLexport uint32_t caml_deserialize_uint_4(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read32u(s);
}

CAMLexport int32_t caml_deserialize_sint_4(void)
{
  struct caml_intern_state* s = get_intern_state ();
  return read32s(s);
}

CAMLexport uint64_t caml_deserialize_uint_8(void)
{
  uint64_t i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport int64_t caml_deserialize_sint_8(void)
{
  int64_t i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport float caml_deserialize_float_4(void)
{
  float f;
  caml_deserialize_block_4(&f, 1);
  return f;
}

CAMLexport double caml_deserialize_float_8(void)
{
  double f;
  caml_deserialize_block_float_8(&f, 1);
  return f;
}

CAMLexport void caml_deserialize_block_1(void * data, intnat len)
{
  struct caml_intern_state* s = get_intern_state ();
  memcpy(data, s->intern_src, len);
  s->intern_src += len;
}

CAMLexport void caml_deserialize_block_2(void * data, intnat len)
{
  struct caml_intern_state* s = get_intern_state ();
#ifndef ARCH_BIG_ENDIAN
  const unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 2, q += 2)
    Reverse_16(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 2);
  s->intern_src += len * 2;
#endif
}

CAMLexport void caml_deserialize_block_4(void * data, intnat len)
{
  struct caml_intern_state* s = get_intern_state ();
#ifndef ARCH_BIG_ENDIAN
  const unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 4, q += 4)
    Reverse_32(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 4);
  s->intern_src += len * 4;
#endif
}

CAMLexport void caml_deserialize_block_8(void * data, intnat len)
{
  struct caml_intern_state* s = get_intern_state ();
#ifndef ARCH_BIG_ENDIAN
  const unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 8);
  s->intern_src += len * 8;
#endif
}

CAMLexport void caml_deserialize_block_float_8(void * data, intnat len)
{
  struct caml_intern_state* s = get_intern_state ();
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memcpy(data, s->intern_src, len * 8);
  s->intern_src += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  const unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  s->intern_src = p;
#else
  const unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Permute_64(q, ARCH_FLOAT_ENDIANNESS, p, 0x01234567);
  s->intern_src = p;
#endif
}

CAMLexport void caml_deserialize_error(char * msg)
{
  struct caml_intern_state* s = get_intern_state ();
  intern_cleanup(s);
  caml_failwith(msg);
}
