/* Copyright (C) 1991, 1996, 1997 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* Modified for garbage collecting malloc.
   marcus@sysc.pdx.edu 1997-09-02 */

#include "misc.h"

/* Duplicate S, returning an identical malloc'd string.  */
char *
strdup (const char *s)
{
  size_t len = strlen (s) + 1;
  void *new = xmalloc_atomic (len);

  if (new == NULL)
    return NULL;

  return (char *) memcpy (new, s, len);
}
