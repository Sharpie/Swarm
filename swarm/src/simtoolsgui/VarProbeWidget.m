// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/VarProbeWidget.h>
#import <simtoolsgui.h>
#import <gui.h>

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
  [super createEnd];

  myLabel = [VarProbeLabel createParent: myLeft];
  [myLabel setText: [myProbe getProbedVariable]];
  
  if (maxLabelWidth)
    [myLabel setWidth: maxLabelWidth];
    
  myEntry = [VarProbeEntry createBegin: [self getZone]];
  [myEntry setOwner: self];
  [myEntry setParent: myRight];
  interactiveFlag = [myProbe getInteractiveFlag];
  [myEntry setInteractiveFlag: interactiveFlag];
  [myEntry setProbeType: ([myProbe getProbedType])[0]];
  myEntry = [myEntry createEnd];
  
  [self update];
  
  return self;
}

- Spawn: (const char *)widgetName
{
  id target = (*(id *)[myProbe probeRaw: myObject]);

  if (target)
    CREATE_PROBE_DISPLAY (target);
  else
    {
      GUI_BEEP ();
      GUI_UPDATE ();
    }
  return self;
}

- pack
{
  [myLabel pack];  
  [myEntry pack];
  
  return self;
}

- setVariableValue: (const char *)windowName
{
  [myProbe setData: myObject ToString: [myEntry getValue]];
  
  return self;
}

- update
{
  char buffer[512];
  
  if (interactiveFlag)
    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
  else
    {
      [myEntry setActiveFlag: YES];
      [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
      [myEntry setActiveFlag: NO];
    }
    
  GUI_UPDATE ();
  
  return self;
}

#ifndef USE_WIDGET
- setParent: theParent
{
  parent = theParent;
  return self;
}
#endif

- (void)drop
{
  [myLabel drop];
  [myEntry drop];
  
  [super drop];
}

- idReceive
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();
  
  [myProbe setData: myObject To: &resObj]; 
  GUI_FOCUS (self);
  [self update];
  return self;
}

- (const char *)package: (const char *)windowName
{
  id *content = [myProbe probeRaw: myObject];

  if (*content == nil)
    {
      GUI_BEEP ();
      GUI_UPDATE ();
      return "";
    }
  return [*content getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [myEntry getValue];
}

@end
