// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/ClassDisplayWidget.h>
#import <gui.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation ClassDisplayWidget

+ createBegin: aZone
{
  id obj = [super createBegin: aZone];
  [obj setMaxLabelWidth: 0];
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
          [InvalidCombination
            raiseEvent:
              "It is an error to reset the object when building a ClassDisplayWidget\n"];
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

- getProbedObject
{
  return probedObject;
}

- setClassToDisplay: (Class)aClass
{
  theClass = aClass;
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

#ifndef USE_FRAME
  widgetName = [parent makeWidgetNameFor: self];
  GUI_MAKE_FRAME (self);
#endif
  [super createEnd];

  if (SAFEPROBES)
    {
      if (probedObject == 0 || theClass == 0)
        {
          [InvalidCombination
            raiseEvent: 
              "ClassDisplayWidget object was not properly initialized\n"];
          return nil;
        }
    }

#ifdef USE_FRAME
  [self setReliefFlag: YES];
#endif

  probeMap = [[[ProbeMap createBegin: [self getZone]] 
                       setProbedClass: theClass]
                       createEnd];

  numberOfProbes = [probeMap getNumEntries];
  
  topRow = [Frame createParent: self];

  myTitle = [ClassDisplayLabel createParent: topRow];
  [myTitle setText: theClass->name];

  GUI_DRAG_AND_DROP (myTitle, self);
  
  hideB = [ClassDisplayHideButton createBegin: [self getZone]];
  [hideB setParent: topRow];
  [hideB setUser: self];
  [hideB setOwner: owner];
  [hideB setSubWidget: mySubclass];
  hideB = [hideB createEnd];

  superB = [SuperButton createBegin: [self getZone]];
  [superB setParent: topRow];
  [superB setUser: self];
  [superB setOwner: owner];
  [superB setSuperWidget: mySuperclass];
  superB = [superB createEnd];
  
  middleFrame =  [Frame createParent: self];  
  leftFrame =  [Frame createParent: middleFrame];  
  rightFrame = [Frame createParent: middleFrame];
  bottomFrame = [Frame createParent: self];
  
  if (numberOfProbes > 0)
    widgets = (id *)
      [[self getZone] alloc: sizeof(id) * numberOfProbes];
  else
    widgets = 0;
  
  index = [probeMap begin: globalZone];
  
  i = 0;
  while ((probe = [index next]))
    {
      if ([probe isKindOf: [VarProbe class]])
        {
          widgets[i] =	
            [[VarProbeWidget createBegin: [self getZone]] setParent: self];
          
          [widgets[i]  setProbe: probe];
          [widgets[i] setObject: probedObject];
          [widgets[i]  setMyLeft:  leftFrame];
          [widgets[i]  setMyRight: rightFrame];
          [widgets[i]  setMaxLabelWidth: maxLabelWidth ];
          
          widgets[i] = [widgets[i] createEnd];
          
          [widgets[i] pack];
          
          i++;
        }
    }
  [index drop];
  
  index = [probeMap begin: globalZone];

  while ((probe = [index next]))
    {
      
      if ([probe isKindOf: [MessageProbe class]]){
        widgets[i] =	
          [[MessageProbeWidget createBegin: [self getZone]] 
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

- armSuperButton
{
  [superB setActiveFlag: YES];
  return self;
}

- update
{
  int i;
  
  for (i = 0; i < numberOfProbes; i++)
    [widgets[i] update];
  
  return self;
}

- (void)drop
{
  int i;
  
  [topRow drop];
  [myTitle drop];
  [hideB drop];
  [superB drop];
  [leftFrame drop];
  [rightFrame drop];
  [middleFrame drop];
  [bottomFrame drop];
  if (numberOfProbes > 0)
    {
      for (i = 0; i < numberOfProbes; i++)
        [widgets[i] drop];
      [[self getZone] free: widgets];
    }
  
  [super drop];
}

- (const char *)package
{
  return [probedObject getObjectName];
}

- (const char *)getId
{
  return [probedObject getIdName];
}

#ifndef USE_FRAME
- setParent: theParent
{
  parent = theParent;
  return self;
}

- (const char *)getWidgetName
{
  return widgetName;
}

- pack
{
  GUI_PACK (self);
  return self;
}

#endif

@end

