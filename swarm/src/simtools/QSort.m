// Swarm library. Copyright © 1997-2000 Swarm Development Group.
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

#import <simtools/QSort.h>
#import <collections.h>

#include <misc.h> // quicksort

#include <swarmconfig.h> // PTRINT

@implementation QSort

static id *flat;
static size_t size;
static SEL comp_selector;

PHASE(Creating)
PHASE(Using)

+ (void)_flatten_: aCollection
{
  id index;  //atOffset would cause repetitive traversal in lists etc.
  size_t i;

  size = [aCollection getCount];
  if (size)
    {
      flat = [scratchZone alloc: sizeof (id) * size];
      
      index = [aCollection begin: scratchZone];
      
      for (i = 0; i < size; i++)
        flat[i] = [index next];
      
      [index drop];
    }
}

+ (void)_unFlatten_: aCollection
{
  id index; // atOffset would cause repetitive traversal in lists etc.
  size_t i;
  
  index = [aCollection begin: scratchZone];
  for (i = 0; i < size; i++)
    {
      [index next];
      [index put: flat[i]]; 
    }
  
  [index drop];
  [scratchZone free: flat];
}

static int
defaultCmpObjs (id *a, id *b)
{
  return [*a compare: *b];
}

static int
cmpInts (PTRINT *a, PTRINT *b)
{
  if (*a > *b)
    return 1;
  
  if (*a == *b)
    return 0;
  
  return -1;
}

static int
cmpObjs (id *a, id *b)
{
  return (int) (PTRINT) [*a perform: comp_selector with: *b];
}

+ (void)sortObjectsIn: aCollection
{
  [self _flatten_: aCollection];

  if (size)
    {
      quicksort (flat, size, sizeof (id),
                 (quicksort_compar_fn_t) defaultCmpObjs);
      [self _unFlatten_: aCollection];
    }
}

+ (void)sortNumbersIn: aCollection
{
  [self _flatten_: aCollection];

  if (size)
    {
      quicksort (flat, size, sizeof (PTRINT),
                 (quicksort_compar_fn_t) cmpInts);
      [self _unFlatten_: aCollection];
    }
}

+ (void)sortObjectsIn: aCollection using: (SEL)aSelector
{
  [self _flatten_: aCollection];
  
  if (size)
    {
      comp_selector = aSelector;
      quicksort (flat, size, sizeof (id),
                 (quicksort_compar_fn_t) cmpObjs);
      [self _unFlatten_: aCollection];
    }
}

+ (void)sortNumbersIn: aCollection
                using: (int (*) (const void *, const void *)) comp_fun
{
  [self _flatten_: aCollection];

  if (size)
    {
      quicksort (flat, size, sizeof (PTRINT),
                 (quicksort_compar_fn_t) comp_fun);
      [self _unFlatten_: aCollection];
    }
}

+ (void)reverseOrderOf: aCollection
{
  id index;  // atOffset would cause repetitive traversal in lists etc.
  size_t i;
  
  // Do `flatten':
  
  size = [aCollection getCount];
  
  if (size)
    {
      flat = [scratchZone alloc: sizeof (id) * size];
      
      index = [aCollection begin: scratchZone];
      
      for (i = 0; i < size; i++)
        flat[i] = [index next];
      
      // Now do a modified `unflatten':
      
      [index setLoc: Start]; // no need to re-create the index
      
      for (i = 0; i < size; i++)
        {
          [index next];
          [index put: flat[size - 1 - i]];
        }
      
      [index drop];
      [scratchZone free: flat];
    }
}

@end
