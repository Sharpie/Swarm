// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/MessageProbe.h>

// Average object: some day, will be developed into a generalized data
// collection facility. This currently works in limited circumstances:
// if the list is homogeneous, and if the selector returns an int or
// a double.

@interface Averager : MessageProbe {
  double total;					  // total
  double max, min;				  // maximum, minimum
  int count;					  // total number
  id list;					  // list to average over
}

-setList: (id) list;
-createEnd;					  // set only one type.

-update;					  // update the average.
-(double) getAverage;
-(double) getTotal;
-(double) getMax;
-(double) getMin;
-(int) getCount;
@end
