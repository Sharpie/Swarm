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
  unsigned count, totalCount;
  id target;
  BOOL isList;

  unsigned maWidth;
  double maTotal;
  double maTotalSquared;
  double *maData;

  id (*nextImp) (id, SEL);
  id (*getLocImp) (id, SEL);
  double (*callImp) (id, SEL, id);
  void (*addImp) (id, SEL, double);
}

- setCollection: aCollection;
- setWidth: (unsigned)width;
- createEnd;		

- (void)update;					  // update the average.
- (double)getAverage;
- (double)getMovingAverage;
- (double)getVariance;
- (double)getMovingVariance;
- (double)getStdDev;
- (double)getMovingStdDev;
- (double)getTotal;
- (double)getMax;
- (double)getMin;
- (unsigned)getCount;
@end

