// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>
#import <gui.h>
#import <collections.h>

@interface ScheduleItem: CompositeItem
{
  id <Schedule> schedule;
  unsigned step;
  id <Zone> zone;
  int x, y;
}

- _createItem_;
- createBindings;
- setSchedule: schedule;
- setStep: (unsigned)step;
- setX: (int)x Y: (int)y;
- createItem;
- update;
- (void)drop;
@end
