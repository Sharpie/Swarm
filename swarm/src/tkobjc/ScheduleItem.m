// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/ScheduleItem.h>
#define BARSIZE 10
#define BAROFFSET 30
#define TEXTOFFSET 45

static void
destroyNotify (id obj, id reallocAddress, void *arg)
{
  [obj drop];
}

@interface PendingEvent: SwarmObject
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
- showEvent;
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

- showEvent
{
  int wx, wy, bx, by;
  unsigned zoomFactor;
  id <Canvas> canvas = [scheduleItem getCanvas];

  zoomFactor = ([widget respondsTo: @selector(getZoomFactor)]
                ? [widget getZoomFactor]
                : 1);

  wx = x * zoomFactor;
  wy = y * zoomFactor;

  bx = [scheduleItem getXForBar];
  by = [scheduleItem getYForTime: t];

  tkobjc_animate_message (widget, canvas, wx, wy, bx, by, NO);
  return self;
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

- createItem
{
  return [self _createItem_];
}

- createBindings
{
  return self;
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

- update
{
  if (zone)
    [zone drop];

  [self _createItem_];
  while (GUI_EVENT_ASYNC ()) {}  
  [pendingEvents forEach: M(showEvent)];
  [pendingEvents deleteAll];

  return self;
}

- (int)getXForBar
{
  return xoffset + BAROFFSET;
}

- (int)getYForTime: (timeval_t)tval
{
  return yoffset + step * (tval - min);
}

- _createItem_
{
  int key;
  id <MapIndex> mi;
  int xbarpos, ymaxpos;
  id <Line> line;
  timeval_t max;

  if (schedule == nil)
    return self;
  
  zone = [Zone create: [self getZone]];
  mi = [schedule begin: zone];
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
  [line addRef: destroyNotify withArgument: NULL];
  
  {
    char buf[20];
    id <TextItem> text;
    
    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: xoffset Y: yoffset];
    sprintf (buf, "%d", min);
    [text setText: buf];
    text = [text createEnd];
    [text addRef: destroyNotify withArgument: NULL];

    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: xoffset Y: ymaxpos];
    sprintf (buf, "%d", max);
    [text setText: buf];
    text = [text createEnd];
    [text addRef: destroyNotify withArgument: NULL];
  }

  {
    timeval_t key;
    id <MapIndex> mi;
    id action;
    
    mi = [schedule begin: zone];
    
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
          [bar addRef: destroyNotify withArgument: NULL];
        }
        {
          id <TextItem> text;

          text = [TextItem createBegin: zone];
          [text setCanvas: canvas];
          [text setCenterFlag: NO];
          [text setX: xoffset + TEXTOFFSET Y: ypos];
          [text setText: [action name]];
          text = [text createEnd];
          [text addRef: destroyNotify withArgument: NULL];
        }
      }
    [mi drop];
  }
  return self;
}

- at: (timeval_t)tval owner: owner widget: widget x: (int)sourceX y: (int)sourceY
{
  id pendingEvent = [PendingEvent createBegin: [self getZone]];

  [pendingEvent setScheduleItem: self];
  [pendingEvent setTime: tval];
  [pendingEvent setOwner: owner];
  [pendingEvent setWidget: widget];
  [pendingEvent setX: sourceX];
  [pendingEvent setY: sourceY];

  [pendingEvents addLast: [pendingEvent createEnd]];
  return self;
}

- trigger: widget X: (int)x Y: (int)y
{
  tkobjc_animate_message (canvas, widget,
                          [self getXForBar],
                          [self getYForTime: getCurrentTime ()],
                          x, y, YES);
  return self;
}


- (void)drop
{
  [zone drop];
  [pendingEvents deleteAll];
  [pendingEvents drop];
  [super drop];
}
@end

