// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
- (void)_createItem_;
- (void)createBindings;
- setSchedule: schedule;
- setStep: (unsigned)step;
- setX: (int)x Y: (int)y;
- (void)createItem;

- (void)at: (timeval_t)tval owner: owner widget: widget x: (int)sourceX y: (int)sourceY;
- (void)update;
- (int)getXForBar;
- (int)getYForTime: (timeval_t)tval;
- (void)trigger: widget X: (int)x Y: (int)y;
- (unsigned)getSleepTime;
- (void)_drop_;
- (void)drop;
@end
