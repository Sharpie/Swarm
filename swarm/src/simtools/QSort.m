// Swarm library. Copyright (C) 1997-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/QSort.h>
#import <collections.h>

#include <misc.h> // xmalloc, xfree
#include <stdlib.h> // qsort

@implementation QSort

static id *flat;
static int size;
static SEL comp_selector;

+ (void)_flatten_: aCollection
{
  id index;  //atOffset would cause repetitive traversal in lists etc.
  int i;

  size = [aCollection getCount];
  if (size)
    {
      flat = xmalloc(sizeof(int)*size);
      
      index = [aCollection begin: scratchZone];
      
      for(i = 0; i < size; i++)
        flat[i] = [index next];
      
      [index drop];
    }
}

+ (void)_unFlatten_: aCollection
{
  id index; //atOffset would cause repetitive traversal in lists etc.
  int i;
  
  index = [aCollection begin: scratchZone];
  for(i = 0; i < size; i++)
    {
      [index next];
      [index put: flat[i]]; 
    }
  
  [index drop];
  xfree (flat);
}

static int
defaultCmpObjs (id *a, id *b)
{
  return [*a compare: *b];
}

static int
cmpInts (int *a, int *b)
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
  return (int) [*a perform: comp_selector with: *b];
}

+ (void)sortObjectsIn: aCollection
{
  [self _flatten_: aCollection];

  if (size)
    {
      qsort (flat,size,sizeof(id),
             (int (*)(const void *, const void *)) defaultCmpObjs);
      [self _unFlatten_: aCollection];
    }
}

+ (void)sortNumbersIn: aCollection
{
  [self _flatten_: aCollection];

  if (size)
    {
      qsort (flat, size, sizeof(int),
             (int (*)(const void *, const void *)) cmpInts);
      [self _unFlatten_: aCollection];
    }
}

+ (void)sortObjectsIn: aCollection using: (SEL)aSelector
{
  [self _flatten_: aCollection];
  
  if (size)
    {
      comp_selector = aSelector;
      qsort (flat, size, sizeof (id),
             (int (*)(const void *, const void *)) cmpObjs);
      [self _unFlatten_: aCollection];
    }
}

+(void) sortNumbersIn: aCollection
                using: (int(*)(const void*,const void*)) comp_fun
{
  [self _flatten_: aCollection];

  if (size)
    {
      qsort (flat, size, sizeof(int),
             (int (*)(const void *, const void *)) comp_fun);
      [self _unFlatten_: aCollection];
    }
}

+ (void)reverseOrderOf: aCollection
{
  id index;  // atOffset would cause repetitive traversal in lists etc.
  int i;
  
  // Do `flatten':
  
  size = [aCollection getCount];
  
  if (size)
    {
      flat = xmalloc (sizeof (int) * size);
      
      index = [aCollection begin: scratchZone];
      
      for (i = 0; i < size; i++)
        flat[i] = [index next];
      
      // Now do a modified `unflatten':
      
      [index setLoc: Start]; // no need to re-create the index
      
      for (i = 0; i < size; i++)
        {
          [index next];
          [index put: flat[size-1-i]];
        }
      
      [index drop];
      xfree (flat);
    }
}

@end
