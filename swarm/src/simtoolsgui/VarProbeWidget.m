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

#import <simtoolsgui/VarProbeWidget.h>
#import <simtoolsgui.h>
#import <gui.h>

@implementation VarProbeWidget

+ createBegin: aZone
{
  id obj = [super createBegin: aZone];

  [obj setMaxLabelWidth: 0];
  
  return obj;
}

- setObject: obj
{
  myObject = obj;

  return self;
}

- setProbe: aProbe
{
  myProbe = aProbe;

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

- setMaxLabelWidth: (int)width
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
  [myEntry setVarProbe: myProbe];
  myEntry = [myEntry createEnd];
  
  [self update];
  
  return self;
}

- Spawn: (const char *)widgetName
{
  id target = [myProbe probeObject: myObject];

  if (target)
    CREATE_PROBE_DISPLAY (target);
  else
    {
      GUI_BEEP ();
      GUI_UPDATE ();
    }
  return self;
}

- (void)pack
{
  [myLabel pack];  
  [myEntry pack];
}

- setVariableValue: (const char *)windowName
{
  [myProbe setData: myObject ToString: [myEntry getValue]];
  
  return self;
}

- (void)update
{
  char buffer[5120];
  
  if (interactiveFlag)
    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
  else
    {
      [myEntry setActiveFlag: YES];
      [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]];
      [myEntry setActiveFlag: NO];
    }
    
  GUI_UPDATE ();
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

- idReceive: (const char *)windowName
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();
  
  [myProbe setData: myObject To: &resObj]; 
  GUI_FOCUS (self);
  [self update];
  return self;
}

- (const char *)package: (const char *)windowName
{
  id content = [myProbe probeObject: myObject];

  if (content == nil)
    {
      GUI_BEEP ();
      GUI_UPDATE ();
      return "";
    }
  return [content getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [myEntry getValue];
}

@end
