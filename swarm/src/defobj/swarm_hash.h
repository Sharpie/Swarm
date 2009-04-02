// Swarm library. Copyright © 2008 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/* Hash tables for Objective C method dispatch.
   Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


/*
Name:         swarm_hash.h
Description:  hash table functionality
Library:      defobj
*/


#ifndef __hash_INCLUDE_SWARM
#define __hash_INCLUDE_SWARM

#include <stddef.h>
#include <string.h> // strcmp
#import <Swarm/swarm-objc-api.h> // BOOL

/*
 * This data structure is used to hold items
 *  stored in a hash table.  Each node holds 
 *  a key/value pair.
 *
 * Items in the cache are really of type void *.
 */
typedef struct swarm_cache_node
{
  struct swarm_cache_node *next; /* Pointer to next entry on the list.
				   NULL indicates end of list. */
  const void *key;		/* Key used to locate the value.  Used
				   to locate value when more than one
				   key computes the same hash
				   value. */
  void *value;			/* Value stored for the key. */
} *swarm_node_ptr;


/*
 * This data type is the function that computes a hash code given a key.
 * Therefore, the key can be a pointer to anything and the function specific
 * to the key type. 
 *
 * Unfortunately there is a mutual data structure reference problem with this
 * typedef.  Therefore, to remove compiler warnings the functions passed to
 * hash_new will have to be casted to this type. 
 */
typedef unsigned int (*swarm_hash_func_type)(void *, const void *);

/*
 * This data type is the function that compares two hash keys and returns an
 * integer greater than, equal to, or less than 0, according as the first
 * parameter is lexicographically greater than, equal to, or less than the
 * second. 
 */

typedef int (*swarm_compare_func_type)(const void *, const void *);


/*
 * This data structure is the cache.
 *
 * It must be passed to all of the hashing routines
 *   (except for new).
 */
typedef struct
{
  /* Variables used to implement the hash itself.  */
  swarm_node_ptr *node_table; /* Pointer to an array of hash nodes.  */
  /* Variables used to track the size of the hash table so to determine
    when to resize it.  */
  unsigned int size; /* Number of buckets allocated for the hash table
			(number of array entries allocated for
			"node_table").  Must be a power of two.  */
  unsigned int used; /* Current number of entries in the hash table.  */
  unsigned int mask; /* Precomputed mask.  */

  /* Variables used to implement indexing through the hash table.  */

  unsigned int last_bucket; /* Tracks which entry in the array where
			       the last value was returned.  */
  /* Function used to compute a hash code given a key. 
     This function is specified when the hash table is created.  */
  swarm_hash_func_type    hash_func;
  /* Function used to compare two hash keys to see if they are equal.  */
  swarm_compare_func_type compare_func;
} swarm_cache_t;

typedef swarm_cache_t *swarm_cache_ptr;

/* Allocate and initialize a hash table.  */ 

swarm_cache_ptr swarm_hash_new (unsigned int size,
				swarm_hash_func_type hash_func,
				swarm_compare_func_type compare_func);
                       
/* Deallocate all of the hash nodes and the cache itself.  */

void swarm_hash_delete (swarm_cache_ptr cache);

/* Add the key/value pair to the hash table.  If the
   hash table reaches a level of fullness then it will be resized. 
                                                   
   assert if the key is already in the hash.  */

void swarm_hash_add (swarm_cache_ptr *cachep, const void *key, void *value);
     
/* Remove the key/value pair from the hash table.  
   assert if the key isn't in the table.  */

void swarm_hash_remove (swarm_cache_ptr cache, const void *key);

/* Used to index through the hash table.  Start with NULL
   to get the first entry.
                                                  
   Successive calls pass the value returned previously.
   ** Don't modify the hash during this operation *** 
                                                  
   Cache nodes are returned such that key or value can
   be extracted.  */

swarm_node_ptr swarm_hash_next (swarm_cache_ptr cache, swarm_node_ptr node);

/* Used to return a value from a hash table using a given key.  */

void *swarm_hash_value_for_key (swarm_cache_ptr cache, const void *key);

/* Used to determine if the given key exists in the hash table */

BOOL swarm_hash_is_key_in_hash (swarm_cache_ptr cache, const void *key);

/************************************************

        Useful hashing functions.  
        
        Declared inline for your pleasure.
        
************************************************/

/* Calculate a hash code by performing some 
   manipulation of the key pointer.  (Use the lowest bits
   except for those likely to be 0 due to alignment.)  */

static inline unsigned int
swarm_hash_ptr (swarm_cache_ptr cache, const void *key)
{
  return ((size_t)key / sizeof (void *)) & cache->mask;
}

#ifndef __cplusplus

/* Calculate a hash code by iterating over a NULL 
   terminate string.  */
static inline unsigned int 
swarm_hash_string (swarm_cache_ptr cache, const void *key)
{
  unsigned int ret = 0;
  unsigned int ctr = 0;
        
        
  while (*(char*)key) {
    ret ^= *(char*)key++ << ctr;
    ctr = (ctr + 1) % sizeof (void *);
  }

  return ret & cache->mask;
}


/* Compare two pointers for equality.  */
static inline int 
swarm_compare_ptrs (const void *k1, const void *k2)
{
  return !(k1 - k2);
}


/* Compare two strings.  */
static inline int 
swarm_compare_strings (const void *k1, const void *k2)
{
  if (k1 == k2)
    return 1;
  else if (k1 == 0 || k2 == 0)
    return 0;
  else
    return !strcmp (k1, k2);
}
#endif

#endif /* not __hash_INCLUDE_SWARM */
