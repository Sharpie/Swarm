// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/MessageProbe.h>

// Average object: calculates a few basic statistics given a collection of 
// objects to poll and a selector with which to poll them.

@interface Averager : MessageProbe {
  double total;				    // total
  double max, min;			    // maximum, minimum
  int count;				    // total number
  id collection;  		            // collection to average over
}

-setCollection: (id) aCollection;
-createEnd;		

-update;					  // update the average.
-(double) getAverage;
-(double) getTotal;
-(double) getMax;
-(double) getMin;
-(int) getCount;
@end
