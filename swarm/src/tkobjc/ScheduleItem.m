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

#import <tkobjc/ScheduleItem.h>
#include <tkobjc/global.h>

#import <defobj/Create.h>
#import <activity/Schedule.h>
#include "internal.h"

#import <tkobjc/CanvasItem.h>
#import <defobj/defalloc.h> // getZone

#import <defobj/swarm-objc-api.h>
#include <misc.h>

#define BARSIZE 10
#define BAROFFSET 30
#define ENTRYOFFSET 200
#define DESCOFFSET 50

#define ANIMATEOFFSET 300

static void
canvasItemDestroyNotify (id obj, id reallocAddress, void *canvas)
{
  const char *item = ((CanvasItem *) obj)->item;
  
  if (item)
    [globalTkInterp eval: "%s delete %s",
		    [(id) canvas getWidgetName], item];
}

static void
canvasLabelDestroyNotify (id obj, id reallocAddress, void *canvas)
{
  [(id)canvas removeWidget: obj];
  [globalTkInterp eval: "destroy %s", [obj getWidgetName]];
}


@interface PendingEvent: CreateDrop
{
  id scheduleItem;
  timeval_t t;
  id owner; 
  id widget;
  int x, y;
}
- setScheduleItem: scheduleItem;
- setTime: (timeval_t)tval;
- setOwner: owner;
- setWidget: widget;
- setX: (int)x;
- setY: (int)y;
- (void)showEvent;
@end

@implementation PendingEvent
- setScheduleItem: theScheduleItem
{
  scheduleItem = theScheduleItem;
  return self;
}

- setTime: (timeval_t)tval
{
  t = tval;
  return self;
}

- setOwner: theOwner
{
  owner = theOwner;
  return self;
}

- setWidget: theWidget
{
  widget = theWidget;
  return self;
}

- setX: (int)theX
{
  x = theX;
  return self;
}

- setY: (int)theY
{
  y = theY;
  return self;
}

- (void)showEvent
{
  int wx, wy, bx, by;
  unsigned zoomFactor;
  id <Canvas> canvas = [scheduleItem getCanvas];

  zoomFactor = ([widget respondsTo: @selector(getZoomFactor)]
                ? [widget getZoomFactor]
                : 1);

  wx = x * zoomFactor;
  wy = y * zoomFactor;

  bx = [scheduleItem getXForBar] + ANIMATEOFFSET;
  by = [scheduleItem getYForTime: t];

  tkobjc_animate_message (widget, canvas, wx, wy, bx, by, NO,
			  [scheduleItem getSleepTime]);
}

@end

@implementation ScheduleItem

PHASE(Creating)

+ createBegin: aZone
{
  ScheduleItem *obj = [super createBegin: aZone];

  obj->step = 20;
  obj->xoffset = 0;
  obj->yoffset = 0;
  obj->pendingEvents = [List create: aZone];  

  return obj;
}

- setStep: (unsigned)theStep
{
  step = theStep;
  return self;
}

- (void)createItem
{
  [self _createItem_];
}

- (void)createBindings
{
}

- setX: (int)theX Y: (int)theY
{
  xoffset = theX;
  yoffset = theY;

  return self;
}

PHASE(Setting)

- setSchedule: theSchedule
{
  schedule = theSchedule;
  return self;
}


PHASE(Using)

- (void)update
{
  processingUpdate = YES;

  if (zone)
    [zone drop];
  
  [self _createItem_];
  while (GUI_EVENT_ASYNC ()) {}  
  {
    id <Index> li = [pendingEvents begin: getZone (self)];
    id event;

    while ((event = [li next]) != nil)
      {
        if (pendingDrop)
          break;
        [event showEvent];
        [event drop];
        [li remove];
      }
    [li drop];
  }
  processingUpdate = NO;
  if (pendingDrop)
    [self _drop_];
}

- (int)getXForBar
{
  return xoffset + BAROFFSET;
}

- (int)getYForTime: (timeval_t)tval
{
  return yoffset + step * (tval - min);
}

- (void)_createItem_
{
  id <MapIndex> mi;
  int xbarpos, ymaxpos;
  timeval_t key;
  timeval_t max = 0;

  if (schedule == nil)
    return;
  
  zone = [SwarmZone create: getZone (self)];
  
  mi = [schedule mapBegin: zone];
  if ([mi next: (id *)&key])
    {
      min = max = key;
      while ([mi next: (id *)&key])
        {
          if (key < min)
            min = key;
          if (key > max)
            max = key;
        }
    }
  [mi drop];
  line = [Line createBegin: zone];
  [line setCanvas: canvas];
  ymaxpos = [self getYForTime: max];
  xbarpos = [self getXForBar];
  [line setTX: xbarpos TY: yoffset LX: xbarpos LY: ymaxpos];
  line = [line createEnd];
  
  {
    char buf[20];
    id <TextItem> text;
    
    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: xoffset Y: yoffset];
    sprintf (buf, "%lu", min);
    [text setText: buf];
    minTextItem = [text createEnd];
    [minTextItem addRef: canvasItemDestroyNotify withArgument: canvas];

    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: xoffset Y: ymaxpos];
    sprintf (buf, "%lu", max);
    [text setText: buf];
    maxTextItem = [text createEnd];
    [maxTextItem addRef: canvasItemDestroyNotify withArgument: canvas];
  }

  {
    timeval_t key;
    id <MapIndex> mi;
    id action;
    
    mi = [schedule mapBegin: zone];
    
    while ((action = [mi next: (id *)&key]))
      {
        int ypos = [self getYForTime: key];

        {
          id <Line> bar;
          
          bar = [Line createBegin: zone];
          [bar setCanvas: canvas];
          [bar setTX: xbarpos - BARSIZE/2 TY: ypos
               LX: xbarpos + BARSIZE/2 LY: ypos];
          bar = [bar createEnd];
          [bar addRef: canvasItemDestroyNotify withArgument: canvas];
        }
        {
          id <TextItem> text;
          
          text = [TextItem createBegin: zone];
          [text setCanvas: canvas];
          [text setCenterFlag: NO];
          [text setX: xoffset + DESCOFFSET Y: ypos];
          {
            char *buf;
            if ([action conformsTo: @protocol (ActionConcurrent)])
              {
                buf = [zone alloc: 10];
                sprintf (buf, "%u",
                         [((ActionConcurrent_c *)
                           action)->concurrentGroup getCount]);
                [text setText: buf];
              }
            else
              {
                const char *targetName = [[action getTarget] name];
                const char *selName =
                  sel_get_name ([action getMessageSelector]);
                unsigned len = 
                  2 + strlen (targetName) + 1 + strlen (selName) + 2;

                buf = [zone alloc: len + 1];
                sprintf (buf, "\\[%s %s\\]", targetName, selName);
                [text setText: buf];
              }
            text = [text createEnd];
            [zone free: buf];
          }
          [text addRef: canvasItemDestroyNotify withArgument: canvas];
        }
        {
          id <CompleteProbeDisplayLabel> label;
          
          label = [CompleteProbeDisplayLabel createBegin: zone];
          [label setProbedObject: action];
          [label setParent: canvas];
          [label setTargetWidget: self];
          label = [label createEnd];
          [label setText: [action getDisplayName]];
          [canvas addWidget: label
                  X: xoffset + ENTRYOFFSET
                  Y: ypos
                  centerFlag: YES];
          [label addRef: canvasLabelDestroyNotify withArgument: canvas];
        }
     }
    [mi drop];
  }
}

- (void)at: (timeval_t)tval owner: owner widget: widget x: (int)sourceX y: (int)sourceY
{
  id pendingEvent = [PendingEvent createBegin: getZone (self)];

  [pendingEvent setScheduleItem: self];
  [pendingEvent setTime: tval];
  [pendingEvent setOwner: owner];
  [pendingEvent setWidget: widget];
  [pendingEvent setX: sourceX];
  [pendingEvent setY: sourceY];

  [pendingEvents addLast: [pendingEvent createEnd]];
}

- (void)trigger: widget X: (int)x Y: (int)y
{
  int zoomFactor = ([widget respondsTo: @selector(getZoomFactor)]
                  ? [widget getZoomFactor]
                    : 1);
  tkobjc_animate_message (canvas, widget,
                          [self getXForBar] + ANIMATEOFFSET,
                          [self getYForTime: getCurrentTime ()],
                          x * zoomFactor, y * zoomFactor, YES, sleepTime);
}

- (unsigned)getSleepTime
{
  return sleepTime;
}

- (void)_drop_
{
  [zone drop];
  [pendingEvents deleteAll];
  [pendingEvents drop];
  pendingEvents = nil;
  [super drop];
}

- (void)drop
{
  if (processingUpdate)
    pendingDrop = YES;
  else
    [self _drop_];
}
@end

