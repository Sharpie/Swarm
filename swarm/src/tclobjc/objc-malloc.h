/* Memory allocation definitions for Objective-C, easy garbage collection.
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

#ifndef __objc_malloc_h_INCLUDE_GNU
#define __objc_malloc_h_INCLUDE_GNU

/* I do this to make substituting Boehm's Garbage Collection easy. */
extern void *(*objc_malloc)(size_t);
extern void *(*objc_atomic_malloc)(size_t);
extern void *(*objc_realloc)(void *, size_t);
extern void *(*objc_calloc)(size_t, size_t);
extern void (*objc_free)(void *);

#define OBJC_MALLOC(VAR, TYPE, NUM) \
   ((VAR) = (TYPE *) (*objc_malloc)((unsigned)(NUM)*sizeof(TYPE))) 
#define OBJC_ATOMIC_MALLOC(VAR, TYPE, NUM) \
   ((VAR) = (TYPE *) (*objc_atomic_malloc)((unsigned)(NUM)*sizeof(TYPE))) 
#define OBJC_REALLOC(VAR, TYPE, NUM) \
   ((VAR) = (TYPE *) (*objc_realloc)((VAR), (unsigned)(NUM)*sizeof(TYPE)))
#define OBJC_CALLOC(VAR, TYPE, NUM) \
   ((VAR) = (TYPE *) (*objc_calloc)((unsigned)(NUM), sizeof(TYPE)))
#define OBJC_FREE(PTR) (*objc_free)((PTR))

#endif /* __objc_malloc_h_INCLUDE_GNU */
