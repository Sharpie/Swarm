// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // Averager
#import <objectbase/MessageProbe.h>

// Average object: calculates a few basic statistics given a collection of 
// objects to poll and a selector with which to poll them.

@interface Averager: MessageProbe <Averager>
{
  double total, totalSquared; 
  double max, min;
  unsigned count;
  id collection;      // collection to average over
}

- setCollection: aCollection;
- createEnd;		

- update;					  // update the average.
- (double)getAverage;
- (double)getVariance;
- (double)getStdDev;
- (double)getTotal;
- (double)getMax;
- (double)getMin;
- (unsigned)getCount;
@end
