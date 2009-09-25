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
          [(id)widgets[i] setProbe: probe];
          [(id)widgets[i] setObject: probedObject];
          [(id)widgets[i] setMyLeft:  leftFrame];
          [(id)widgets[i] setMyRight: rightFrame];
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
          [(id)widgets[i]  setProbe: probe];
          [(id)widgets[i] setObject: probedObject];
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

- (void)update
{
  unsigned i;
  
  for (i = 0; i < count; i++)
    if (!markedForDropFlag)
      [(id)widgets[i] update];
    else
      break;
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
