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

@implementation ScheduleItem

PHASE(Creating)

+ createBegin: aZone
{
  ScheduleItem *obj = [super createBegin: aZone];

  obj->step = 20;
  obj->x = 0;
  obj->y = 0;
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
  x = theX;
  y = theY;

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
  
  return [self _createItem_];
}

- _createItem_
{
  int key, min, max;
  id <MapIndex> mi;
  int ymaxpos;
  id <Line> line;

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
  ymaxpos = y + step * (max - min);
  [line setTX: x + BAROFFSET TY: y LX: x + BAROFFSET LY: ymaxpos];
  line = [line createEnd];
  [line addRef: destroyNotify withArgument: NULL];
  
  {
    char buf[20];
    id <TextItem> text;
    
    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: x Y: y];
    sprintf (buf, "%d", min);
    [text setText: buf];
    text = [text createEnd];
    [text addRef: destroyNotify withArgument: NULL];

    text = [TextItem createBegin: zone];
    [text setCanvas: canvas];
    [text setCenterFlag: NO];
    [text setX: x Y: ymaxpos];
    sprintf (buf, "%d", max);
    [text setText: buf];
    text = [text createEnd];
    [text addRef: destroyNotify withArgument: NULL];
  }

  {
    int key;
    id <MapIndex> mi;
    id action;
    
    mi = [schedule begin: zone];
    
    while ((action = [mi next: (id *)&key]))
      {
        int ypos = y + step * (key - min);

        {
          id <Line> bar;
          
          bar = [Line createBegin: zone];
          [bar setCanvas: canvas];
          [bar setTX: x + BAROFFSET - BARSIZE/2 TY: ypos LX: x + BAROFFSET + BARSIZE/2 LY: ypos];
          bar = [bar createEnd];
          [bar addRef: destroyNotify withArgument: NULL];
        }
        {
          id <TextItem> text;

          text = [TextItem createBegin: zone];
          [text setCanvas: canvas];
          [text setCenterFlag: NO];
          [text setX: x + TEXTOFFSET Y: ypos];
          [text setText: [action name]];
          text = [text createEnd];
          [text addRef: destroyNotify withArgument: NULL];
        }
      }
    [mi drop];
  }
  return self;
}

- (void)drop
{
  [zone drop];
  [super drop];
}
@end

