// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/VarProbeWidget.h>
#import <simtools/global.h>
#import <simtools.h>
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

  myLabel = [Label createParent: myLeft];
  [myLabel setText: [myProbe getProbedVariable]];
  [myLabel anchorEast];

  if (maxLabelWidth)
    [myLabel setWidth: maxLabelWidth];
    
  myEntry = [VarProbeEntry createBegin: [self getZone]];
  [myEntry setOwner: self];
  [myEntry setParent: myRight];
  interactive = [myProbe isInteractive];
  [myEntry setInteractiveFlag: interactive];
  [myEntry setProbeType: ([myProbe getProbedType])[0]];
  myEntry = [myEntry createEnd];
  
  [self update];
  
  return self;
}

- Spawn
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
      [myEntry setActiveFlag: YES];
      [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
      [myEntry setActiveFlag: NO];
    }
  else
    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
  
  GUI_UPDATE ();
  
  return self;
}

#ifndef USE_WIDGET
- focus
{
  GUI_FOCUS (self);
  return self;
}

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
  [self focus];
  [self update];
  return self;
}

- (const char *)package
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

- (const char *)getId
{
  return [myEntry getValue];
}

@end
