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

#import <simtoolsgui/ClassDisplayWidget.h>
#import <gui.h>
#import <defobj/defalloc.h> // getZone

#include <swarmconfig.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation ClassDisplayWidget

PHASE(Creating)

+ createBegin: aZone
{
  ClassDisplayWidget *obj = [super createBegin: aZone];

  obj->maxLabelWidth = 0;

  return obj;
}

- setOwner: anOwner
{
  owner = anOwner;

  return self;
}

- setMySuperclass: aWidget
{
  mySuperclass = aWidget;

  return self;
}

- setMySubclass: aWidget
{
  mySubclass = aWidget;

  return self;
}

- setProbedObject: anObject
{
  if (SAFEPROBES)
    {
      if (probedObject != 0)
        {
          raiseEvent (InvalidCombination, 
                      "It is an error to reset the object when building a ClassDisplayWidget\n");
          return nil;
        }
    }
  probedObject = anObject;

  return self;
}

- setMaxLabelWidth: (int)width
{
  maxLabelWidth = width;

  return self;
}

- setClassToDisplay: (Class)aClass
{
  theClass = aClass;

  return self;
}

// finalize creation: create widgets, set them up.
- createEnd
{
  int i;
  id index;
  id probe;

#ifndef USE_FRAME
  widgetName = [parent makeWidgetNameFor: self];
  GUI_MAKE_FRAME (self);
#endif
  [super createEnd];

  if (SAFEPROBES)
    {
      if (probedObject == 0 || theClass == 0)
        {
          raiseEvent (InvalidCombination,
                      "ClassDisplayWidget object was not properly initialized\n");
          return nil;
        }
    }

#ifdef USE_FRAME
  [self setReliefFlag: YES];
#endif

  probeMap = [[[ProbeMap createBegin: getZone (self)]
                setProbedClass: theClass]
               createEnd];

  count = [probeMap getCount];
  
  topRow = [Frame createParent: self];

  myTitle = [ClassDisplayLabel createParent: topRow];
    
  if (([probedObject getClass] == theClass))
    [myTitle setText: [probedObject getName]];
#ifdef HAVE_JDK
  else if ([theClass respondsTo: M(isJavaProxy)])
    [myTitle setText: java_get_class_name (SD_JAVA_FIND_CLASS_JAVA (theClass))];
#endif
  else
    [myTitle setText: swarm_class_getName(theClass)];

  GUI_DRAG_AND_DROP (myTitle, self);
  
  hideB = [ClassDisplayHideButton createBegin: getZone (self)];
  [hideB setParent: topRow];
  [hideB setUser: self];
  [hideB setOwner: owner];
  [hideB setSubWidget: mySubclass];
  hideB = [hideB createEnd];

  superB = [SuperButton createBegin: getZone (self)];
  [superB setParent: topRow];
  [superB setUser: self];
  [superB setOwner: owner];
  [superB setSuperWidget: mySuperclass];
  superB = [superB createEnd];
  
  middleFrame = [Frame createParent: self];
  leftFrame = [Frame createParent: middleFrame];  
  rightFrame = [Frame createParent: middleFrame];
  bottomFrame = [Frame createParent: self];
  
  if (count > 0)
    widgets = (id *)
      [getZone (self) alloc: sizeof (id) * count];
  else
    widgets = 0;
  
  index = [probeMap begin: globalZone];
  
  i = 0;
  while ((probe = [index next]))
    {
      if ([probe isKindOf: [VarProbe class]])
        {
          widgets[i] =	
            [[VarProbeWidget createBegin: getZone (self)] setParent: self];
          [widgets[i] setProbe: probe];
          [widgets[i] setObject: probedObject];
          [widgets[i] setMyLeft:  leftFrame];
          [widgets[i] setMyRight: rightFrame];
          [widgets[i] setMaxLabelWidth: maxLabelWidth];
          
          widgets[i] = [widgets[i] createEnd];
          
          [widgets[i] pack];
          
          i++;
        }
    }
  [index drop];
  
  index = [probeMap begin: globalZone];

  while ((probe = [index next]))
    {
      if ([probe isKindOf: [MessageProbe class]])
        {
          widgets[i] =	
            [[MessageProbeWidget createBegin: getZone (self)]
              setParent: bottomFrame];
          
          [widgets[i] setProbe: probe];
          [widgets[i] setObject: probedObject];
          [widgets[i] setMaxReturnWidth: maxLabelWidth];
          widgets[i] = [widgets[i] createEnd];
          [widgets[i] pack];
          i++;
        }
    }
  
  [index drop];
  
  [superB packToRight: hideB];
  [myTitle packBeforeAndFillLeft: hideB expand: NO];
  
  [topRow pack];
  
  [leftFrame packFillLeft: NO];
  [rightFrame packFillLeft: YES];

  [middleFrame pack];
  [bottomFrame pack];
  
  return self;
}

PHASE(Using)

- getProbedObject
{
  return probedObject;
}

- (id <ProbeMap>)getProbeMap
{
  return probeMap;
}

- (void)armSuperButton
{
  [superB setActiveFlag: YES];
}

- (void)update
{
  unsigned i;
  
  for (i = 0; i < count; i++)
    [widgets[i] update];
}

- (void)drop
{
  unsigned i;
  
  [topRow drop];
  [myTitle drop];
  [hideB drop];
  [superB drop];
  [leftFrame drop];
  [rightFrame drop];
  [middleFrame drop];
  [bottomFrame drop];
  if (count > 0)
    {
      for (i = 0; i < count; i++)
        [widgets[i] drop];
      [getZone (self) free: widgets];
    }
  
  [super drop];
}

- (const char *)package: (const char *)windowName
{
  return [probedObject getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [probedObject getDisplayName];
}

#ifndef USE_FRAME
- setParent: (id <Frame>)theParent
{
  parent = theParent;
  return self;
}

- (const char *)getWidgetName
{
  return widgetName;
}

- (void)pack
{
  GUI_PACK (self);
}

- getTopLevel
{
  return [parent getTopLevel];
}

#endif

@end

