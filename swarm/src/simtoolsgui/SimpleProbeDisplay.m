// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <simtoolsgui/SimpleProbeDisplay.h>
#import <simtoolsgui/VarProbeWidget.h>
#import <simtoolsgui/MessageProbeWidget.h>
#import <simtoolsgui/global.h>

#import <gui.h>

@implementation SimpleProbeDisplay

- setProbeMap: theProbeMap
{
  probeMap = theProbeMap;
  return self;
}

- getProbeMap
{  
  return probeMap;
}

// finalize creation: create widgets, set them up.
- createEnd
{
  int i;
  id index;
  id probe;
  id <SimpleProbeDisplayHideButton> hideB;
  id <Frame> top_top_Frame, raisedFrame;

  numberOfProbes = [probeMap getNumEntries];

  [super createEnd];

  top_top_Frame =  [Frame createParent: topFrame];  

  raisedFrame =  [Frame createBegin: [self getZone]];
  [raisedFrame setParent: top_top_Frame];
  [raisedFrame setReliefFlag: YES];
  raisedFrame = [raisedFrame createEnd];

  myTitle = [CompleteProbeDisplayLabel createBegin: [self getZone]];
  [myTitle setParent: raisedFrame];
  [myTitle setProbeDisplay: self];
  [myTitle setProbeDisplayManager: probeDisplayManager];
  [myTitle setProbedObject: probedObject];
  myTitle = [myTitle createEnd];
  [myTitle setText: [self getId: NULL]];
  
  hideB = [SimpleProbeDisplayHideButton createBegin: [self getZone]];
  [hideB setParent: top_top_Frame];
  [hideB setProbeDisplay: self];
  hideB = [hideB createEnd];
  
  [raisedFrame packBeforeAndFillLeft: hideB expand: NO];

  middleFrame =  [Frame createParent: topFrame];  
  leftFrame =  [Frame createParent: middleFrame];
  rightFrame = [Frame createParent: middleFrame];
  bottomFrame = [Frame createParent: topFrame];

  if (numberOfProbes > 0)
    widgets = (id *)
      [[self getZone] alloc: sizeof(id) * numberOfProbes];
  else
    widgets = 0;

  index = [probeMap begin: globalZone];

  i = 0;
  while ((probe = [index next]) != nil)
    {      
      if ([probe isKindOf: [VarProbe class]])
        {
          widgets[i] =	
            [[VarProbeWidget createBegin: [self getZone]]
              setParent: topFrame];
          [widgets[i] setProbe: probe];
          [widgets[i] setObject: probedObject];
          [widgets[i] setMyLeft:  leftFrame];
          [widgets[i] setMyRight: rightFrame];
          widgets[i] = [widgets[i] createEnd];
          [widgets[i] pack];
          i++;
      }
    }
  
  [index drop];

  index = [probeMap begin: globalZone];

  while ((probe = [index next]) != nil)
    {
      if ([probe isKindOf: [MessageProbe class]])
        {
          widgets[i] =	
            [[MessageProbeWidget createBegin: [self getZone]] 
              setParent: bottomFrame];
          [widgets[i]  setProbe: probe];
          [widgets[i] setObject: probedObject];
          widgets[i] = [widgets[i] createEnd];
          [widgets[i] pack];
          i++;
        }
    }
  
  [index drop];

  // This label is not being garbage collected!!!!!
  if (!i)
    {
      index = [Label createParent: topFrame];
      [index setText: "No Instance Variables or Messages."];
      [index pack];
    }

  [top_top_Frame packFill];
  [leftFrame packFillLeft: NO];
  [rightFrame packFillLeft: YES];

  [middleFrame pack];
  [bottomFrame pack];

  [self install];
  return self;
}

- update
{
  int i;
  
  for (i = 0; i < numberOfProbes; i++)
    if (!markedForDropFlag)
      [widgets[i] update];
    else
      break;

  return self;
}

- (void)drop
{
  int i;
  
  // If drops all go through markForDrop, then the disable there
  // should take care of it.
  // [topLevel disableDestroyNotification];
  [leftFrame drop];
  [rightFrame drop];
  [middleFrame drop];
  [bottomFrame drop];

  for (i = 0; i < numberOfProbes; i++)
    [widgets[i] drop];

  if (numberOfProbes)
    [[self getZone] free: widgets];

  [topLevel drop];

  [probeDisplayManager removeProbeDisplay: self];

  if (removeRef)
    [probedObject removeRef: objectRef];
  
  [super drop];
}

@end
