// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdlib.h>
#import <objc/objc.h>
#import <string.h>
#import <tkobjc.h>
#import <simtools/ClassDisplayWidget.h>
#import <tkobjc/control.h>

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

- setMySuperClass: aWidget
{
  mySuperClass = aWidget;
  return self;
}

- setMySubClass: aWidget
{
  mySubClass = aWidget;
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
  
  tkobjc_setRelief (self);

  probeMap = [[[ProbeMap createBegin: [self getZone]] 
                       setProbedClass: theClass]
                       createEnd];

  numberOfProbes = [probeMap getNumEntries];
	
  topRow = [Frame createParent: self];

  myTitle  = [Label createParent: topRow];
  [myTitle setText: (char *)theClass->name];

  tkobjc_setAnchorWest (myTitle);
  tkobjc_setColorBlue (myTitle);

  dragAndDrop (myTitle, self);

  hideB = [Button createParent: topRow];
  if (mySubClass != nil)
    {
      tkobjc_packForgetArmSuperAndResize (hideB, self, mySubClass, owner);
      tkobjc_configureHideBitmap (hideB);
    } 
  else
    {
      tkobjc_configureWidgetToDrop (hideB, owner);
      tkobjc_configureSpecialBitmap (hideB);
    }
  
  superB = [Button createParent: topRow];
  tkobjc_configureSuperBitmap (superB);
  
  if (mySuperClass != nil) 
    tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize 
      (superB,
       mySuperClass,
       self,
       owner);
  else
    {
      tkobjc_configureWidgetToBeep (superB);
      tkobjc_disabledState (superB);
    }
  
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
  
  //Will 'rewind' index when I find out how...
  index = [probeMap begin: globalZone];

  while ((probe = [index next]))
    {
      
      if ([probe isKindOf: [MessageProbe class]]){
        widgets[i] =	
          [[MessageProbeWidget createBegin: [self getZone]] 
            setParent: bottomFrame];
        
        [widgets[i] setProbe: probe];
        [widgets[i] setObject: probedObject];
        [widgets[i] setMaxReturnWidth: maxLabelWidth ];
        widgets[i] = [widgets[i] createEnd];
        [widgets[i] pack];
        i++;
      }
    }
  
  [index drop];
  
  tkobjc_packToRight (superB, hideB);
  tkobjc_packBeforeAndFillLeft (myTitle, hideB, 0);
  
  [topRow pack];
  
  tkobjc_packFillLeft (leftFrame, 0);
  tkobjc_packFillLeft (rightFrame, 1);

  [middleFrame pack];
  [bottomFrame pack];
  
  return self;
}

- armSuperButton
{
  tkobjc_normalState (superB);
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
  return tclObjc_objectToName (probedObject);
}

- (const char *)getId
{
  if ([probedObject respondsTo: @selector (getInstanceName)])
    return [probedObject getInstanceName];
  else
    return [probedObject name];
}

@end
