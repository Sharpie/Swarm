// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/VarProbeWidget.h>
#import <simtools/global.h>
#import <objc/objc-api.h>
#import <tkobjc/control.h>
#import <simtools.h>

@implementation VarProbeWidget

+ createBegin: aZone
{
  id obj;
  
  obj = [super createBegin: aZone];
  [obj setMaxLabelWidth: 0];
  
  return obj;
}

- setObject: obj
{
  myObject = obj;
  return self;
}

- setProbe: (Probe *) the_probe
{
  myProbe = (VarProbe *) the_probe;
  return self;
}

- setMyLeft: obj
{
  myLeft = obj;
  return self;
}

- setMyRight: obj
{
  myRight = obj;
  return self;
}

- setMaxLabelWidth: (int) width
{
  maxLabelWidth = width;
  return self;
}

- createEnd
{
  char theType;
  
  [super createEnd];

  myLabel  = [Label  createParent: myLeft];
  [myLabel setText: [myProbe getProbedVariable]];
  
  tkobjc_setAnchorEast (myLabel);
  if (maxLabelWidth)
    tkobjc_setWidth (myLabel, maxLabelWidth);
    
  myEntry  = [Entry  createParent: myRight];
  theType = ([myProbe getProbedType])[0];
  
  if ([myProbe isInteractive])
    {
      tkobjc_bindReturnToSetValue (myEntry, self);
      tkobjc_bindKeyReleaseReturnToResetColorAndUpdate (myEntry);
      tkobjc_bindFocusInToSetSelection (myEntry);
      tkobjc_bindFocusOutToClearSelection (myEntry);
      interactive = 1;
    }
  else
    tkobjc_disabledState (myEntry);
  
  if (theType == _C_ID)
    {
      tkobjc_bindButton3ToSpawn (myEntry, self, 0);
      dragAndDropTarget (myEntry, self);
      dragAndDrop (myEntry, self);
    }
  else
    tkobjc_bindButton3ToBeUnhelpful (myEntry, nil);
  
  [self update];
  
  return self;
}

- Spawn
{
  id target = (*(id *)[myProbe probeRaw: myObject]);
  
  if (target)
    createProbeDisplay (target);
  else
    {
      tkobjc_ringBell ();
      tkobjc_update ();
    }
  return self;
}

- pack
{
  [myLabel pack];  
  [myEntry pack];
  
  return self;
}

- setValue
{
  [myProbe setData: myObject ToString: [myEntry getValue]];
  
  return self;
}

- update
{
  char buffer[512];
  
  if (!interactive)
    {
      tkobjc_normalState (myEntry);
      [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
      tkobjc_disabledState (myEntry);
    }
  else
    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
  
  tkobjc_update ();
  
  return self;
}

- (void)drop
{
  [myLabel drop];
  [myEntry drop];
  
  [super drop];
}

- idReceive
{
  id resObj = tkobjc_gimme_drag_and_drop_object ();

  [myProbe setData: myObject To: &resObj]; 
  tkobjc_focus (myEntry);
  [self update];
  return self;
}

- (const char *)package
{
  id *content = [myProbe probeRaw: myObject];

  if (*content == nil)
    {
      tkobjc_ringBell ();
      tkobjc_update ();
      return "";
    }
  return tclObjc_objectToName (*content);
}

- (const char *)getId
{
  return [myEntry getValue];
}

@end
