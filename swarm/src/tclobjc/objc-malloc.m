/* Memory allocation support for Objective-C: easy garbage collection.
   Copyright (C) 1993 Free Software Foundation, Inc.

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   This file is part of the GNU Objective-C Collection library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

#ifdef NeXT
#include <stdio.h>

volatile void objc_fatal(const char *msg)
{
  write(stderr, msg, (size_t)strlen((char*)msg));
  abort();
}

#endif

#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

void*
__objc_malloc(size_t size)
{
  void* res = (void*) malloc(size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}
 
void*
__objc_realloc(void* mem, size_t size)
{
  void* res = (void*) realloc(mem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}
 
void*
__objc_calloc(size_t nelem, size_t size)
{
  void* res = (void*)calloc(nelem, size);
  if(!res)
    objc_fatal("Virtual memory exhausted\n");
  return res;
}

void
__objc_free (void* obj)
{
  free (obj);
}

/* I do this to make substituting Boehm's Garbage Collector easy. */
void *(*objc_malloc)(size_t size) = __objc_malloc;
void *(*objc_atomic_malloc)(size_t) = __objc_malloc;
void *(*objc_realloc)(void *optr, size_t size) = __objc_realloc;
void *(*objc_calloc)(size_t nelem, size_t size) = __objc_calloc;
void (*objc_free)(void *optr) = __objc_free;
