// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
