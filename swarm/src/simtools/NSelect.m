// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         NSelect.m
Description:  Selection Routine
Library:      simtools
*/

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdlib.h>
#import <simtools.h>

@implementation NSelect

+(void) select: (int) n from: aCollection into: bCollection {
  id a, b ;
  int N ; // total number of items in aCollection
  int t ; // items seen
  int m ; // items selected
  float r ;

  if(!n)
    return ;

  t = m = 0 ;

  N = [aCollection getCount] ;

  if(N < n){
    fprintf(stderr,"NSelect: attempted to select %d elements from a collection containing only %d elements!!!\n",n,N) ;
    exit(-1) ;
  }

  a = [aCollection begin: scratchZone] ;
  b = [bCollection begin: scratchZone] ;

  while(m < n){
    r = [uniformRandom rFloat] ;    
    
    if( ( ((float)(N - t)) * r) >= ((float)(n - m)) ){
      [a next] ;
    } else {
      m++ ;
      [b next] ;
      [b put: [a next]] ;
    }

    t++ ;
  }

  [a drop] ;
  [b drop] ;
}

@end
