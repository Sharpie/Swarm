// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <simtoolsgui/SimpleProbeDisplay.h>
#import <simtoolsgui.h>
#import <simtoolsgui/VarProbeWidget.h>
#import <gui.h>
#import <defobj/defalloc.h> // getZone

@implementation SimpleProbeDisplay

- setProbeMap: (id <ProbeMap>)theProbeMap
{
  probeMap = theProbeMap;
  return self;
}

- (id <ProbeMap>)getProbeMap
{  
  return probeMap;
}

// finalize creation: create widgets, set them up.
- createEnd
{
  unsigned i;
  id <Index> index;
  id probe;

  count = [probeMap getCount];

  [super createEnd];

  top_top_Frame =  [Frame createParent: topFrame];  

  raisedFrame =  [Frame createBegin: getZone (self)];
  [raisedFrame setParent: top_top_Frame];
  [raisedFrame setReliefFlag: YES];
  raisedFrame = [raisedFrame createEnd];

  title = [CompleteProbeDisplayLabel createBegin: getZone (self)];
  [title setParent: raisedFrame];
  [title setTargetWidget: self];
  [title setProbedObject: probedObject];
  title = [title createEnd];
  [title setText: [self getId: NULL]];
  
  hideB = [SimpleProbeDisplayHideButton createBegin: getZone (self)];
  [hideB setParent: top_top_Frame];
  [hideB setProbeDisplay: self];
  hideB = [hideB createEnd];
  
  [raisedFrame packBeforeAndFillLeft: hideB expand: NO];

  middleFrame =  [Frame createParent: topFrame];  
  leftFrame =  [Frame createParent: middleFrame];
  rightFrame = [Frame createParent: middleFrame];
  bottomFrame = [Frame createParent: topFrame];

  if (count > 0)
    widgets = (id *) [getZone (self) alloc: sizeof (id) * count];
  else
    widgets = 0;

  index = [probeMap begin: globalZone];

  i = 0;
  while ((probe = [index next]) != nil)
    {      
      if ([probe isKindOf: [VarProbe class]])
        {
          widgets[i] =	
            [[VarProbeWidget createBegin: getZone (self)]
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
            [[MessageProbeWidget createBegin: getZone (self)]
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
      id label = [Label createParent: topFrame];

      [label setText: "No Instance Variables or Messages."];
      [label pack];
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
  unsigned i;
  
  for (i = 0; i < count; i++)
    if (!markedForDropFlag)
      [widgets[i] update];
    else
      break;

  return self;
}

- (void)drop
{
  unsigned i;
  
  // If drops all go through markForDrop, then the disable there
  // should take care of it.
  // [topLevel disableDestroyNotification];

  [hideB drop];
  [title drop];

  [leftFrame drop];
  [rightFrame drop];
  [middleFrame drop];
  [bottomFrame drop];

  for (i = 0; i < count; i++)
    [widgets[i] drop];

  if (count)
    [getZone (self) free: widgets];

  [raisedFrame drop];
  [top_top_Frame drop];

  [probeDisplayManager removeProbeDisplay: self];

  if (removeRef)
    [probedObject removeRef: objectRef];
  
  [super drop];
}

@end
