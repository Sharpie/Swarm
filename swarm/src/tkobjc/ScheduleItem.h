// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>
#import <gui.h>
#import <collections.h>

@interface ScheduleItem: CompositeItem <ScheduleItem>
{
  id <Schedule> schedule;
  unsigned step;
  id <Zone> zone;
  id <List> pendingEvents;
  int xoffset, yoffset;
  timeval_t min;
  id <Line> line;
  id <TextItem> minTextItem;
  id <TextItem> maxTextItem;
  unsigned sleepTime;
  BOOL processingUpdate;
  BOOL pendingDrop;
}

+ createBegin: aZone;
- _createItem_;
- createBindings;
- setSchedule: schedule;
- setStep: (unsigned)step;
- setX: (int)x Y: (int)y;
- at: (timeval_t)tval owner: owner widget: widget x: (int)sourceX y: (int)sourceY;
- createItem;
- update;
- (int)getXForBar;
- (int)getYForTime: (timeval_t)tval;
- trigger: widget X: (int)x Y: (int)y;
- (unsigned)getSleepTime;
- (void)_drop_;
- (void)drop;
@end
