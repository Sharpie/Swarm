// Swarm library. Copyright (C) 1997-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         QSort.m
Description:  Sort Routine
Library:      simtools
*/

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdlib.h>
#import <collections.h>
#import <simtools/QSort.h>

//S: A class to sort a collection.
//D: QSort is simply a "wrapper" for the C native "qsort" function, as applied
//D: to a Swarm collection. The values will appear in ascending order by
//D: default. Reversing the order of a collection can be made by calling 
//D: reverseOrderOf. All these methods modify the underlying collection, so
//D: any indexes should always be regenerated. 
@implementation QSort

static id *flat ;
static int size ;
static SEL comp_selector ;

+(void) _flatten_: aCollection {
  id index ;  //atOffset would cause repetitive traversal in lists etc.
  int i ;

  size = [aCollection getCount] ;
  if(size){
    flat = malloc(sizeof(int)*size) ;

    index = [aCollection begin: scratchZone] ;

    for(i = 0 ; i < size ; i++)
      flat[i] = [index next] ;

    [index drop] ;
  }
}

+(void) _unFlatten_: aCollection {
  id index ; //atOffset would cause repetitive traversal in lists etc.
  int i ;

  index = [aCollection begin: scratchZone] ;
  for(i = 0 ; i < size ; i++){
    [index next] ;
    [index put: flat[i]] ; 
  }

  [index drop] ;
  free(flat) ;
}

int defaultCmpObjs(id *a,id *b){
  return [*a compare: *b] ;
}

int cmpInts(int *a,int *b){

  if (*a > *b)
    return 1 ;

  if (*a == *b)
    return 0 ;

  return -1 ;
}

int cmpObjs(id *a,id *b){
  return (int) [*a perform: comp_selector with: *b] ;
}

//M: The sortObjectsIn: method will sort the objects in the collection with the
//M: "compare' function for the object.  If the objects don't provide a
//M: compare function, sortObjectsIn uses the default from the defobj library. 
+(void) sortObjectsIn: aCollection {

  [self _flatten_: aCollection] ;

  if(size){
    qsort(flat,size,sizeof(id),
          (int (*)(const void *, const void *)) defaultCmpObjs) ;
    [self _unFlatten_: aCollection] ;
  }
}

//M: The sortNumbersIn: method performs integer comparisons on the objects in
//M: the collection using the default "compare" function.  The default assumes
//M: that the numbers should be monotonically increasing.
+(void) sortNumbersIn: aCollection {

  [self _flatten_: aCollection] ;

  if(size){
    qsort(flat,size,sizeof(int),
          (int (*)(const void *, const void *)) cmpInts) ;
    [self _unFlatten_: aCollection] ;
  }
}

//M: The sortObjectsIn:using: method will sort the objects in the collection 
//M: with the specified comparison function for the object.
+(void) sortObjectsIn: aCollection using: (SEL) aSelector {

  [self _flatten_: aCollection] ;

  if(size){
    comp_selector = aSelector ;
    qsort(flat,size,sizeof(id),
          (int (*)(const void *, const void *)) cmpObjs) ;
    [self _unFlatten_: aCollection] ;
  }

}

//M: The sortNumbersIn:using: method performs integer comparisons on the 
//M: objects in the collection with the specified comparison function for the
//M: object.
+(void) sortNumbersIn: aCollection
                using: (int(*)(const void*,const void*)) comp_fun {

  [self _flatten_: aCollection] ;

  if(size){
    qsort(flat,size,sizeof(int),
          (int (*)(const void *, const void *)) comp_fun) ;
    [self _unFlatten_: aCollection] ;
  }

}

//M: The reverseOrderOf: method reverses the current order of a collection.
//M: To make a "reversed" sort, simply call one of the appropriate "sort"
//M: methods on a collection then call this function on the same collection.
+(void) reverseOrderOf: aCollection {
  id index ;  // atOffset would cause repetitive traversal in lists etc.
  int i ;

// Do 'flatten':

  size = [aCollection getCount] ;

  if(size){

    flat = malloc(sizeof(int)*size) ;

    index = [aCollection begin: scratchZone] ;

    for(i = 0 ; i < size ; i++)
      flat[i] = [index next] ;

// Now do a modified 'unflatten':

    [index setLoc: Start];	// no need to re-create the index

    for(i = 0 ; i < size ; i++){
      [index next] ;
      [index put: flat[size-1-i]] ;
    }

    [index drop] ;
    free(flat) ;

  }
}

@end
