/* memcpy -- copy memory to memory until the specified number of bytes
   has been copied.  Overlap is NOT handled correctly.
   Copyright (C) 1991 Free Software Foundation, Inc.
   Contributed by Torbjorn Granlund (tege@sics.se).

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*
  Removed the word-boundary optimizations.  Include <misc.h> and "mem.h"
  for declarations.
  
  mgd@santafe.edu 1998-07-08. */

#include <misc.h>
#include "mem.h"

/* Copy exactly NBYTES bytes from SRC_BP to DST_BP,
   without any assumptions about alignment of the pointers.  */
#define BYTE_COPY_FWD(dst_bp, src_bp, nbytes)                                 \
  do                                                                          \
    {                                                                         \
      size_t __nbytes = (nbytes);                                             \
      while (__nbytes > 0)                                                    \
        {                                                                     \
          byte __x = ((byte *) src_bp)[0];                                    \
          src_bp += 1;                                                        \
          __nbytes -= 1;                                                      \
          ((byte *) dst_bp)[0] = __x;                                         \
          dst_bp += 1;                                                        \
        }                                                                     \
    } while (0) 

void *
memcpy (void *dstpp, const void *srcpp, size_t len)
{
  unsigned long int dstp = (long int) dstpp;
  unsigned long int srcp = (long int) srcpp;

  BYTE_COPY_FWD (dstp, srcp, len);

  return dstpp;
}
