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

#import <tkobjc/CompleteProbeDisplayLabel.h>
#import <tkobjc/common.h>
#import <tkobjc/global.h>

#include <misc.h> // strcpy

extern id probeDisplayManager;

static void
tkobjc_bindButton3ForCompleteProbeDisplay (id widget,
                                           id probedObject)
{
  const char *pdmName = [probeDisplayManager getObjectName];
  char pdmNameCopy[strlen (pdmName) + 1];

  strcpy (pdmNameCopy, pdmName);    
  
  [globalTkInterp 
    eval: 
      "bind %s <ButtonPress-3> {%s createCompleteProbeDisplayFor: %s}",
    [widget getWidgetName],
    pdmNameCopy,
    [probedObject getObjectName]];
}

@implementation CompleteProbeDisplayLabel

PHASE(Creating)

- setProbedObject: theProbedObject
{
  probedObject = theProbedObject;
  return self;
}

- setTargetWidget: theTargetWidget
{
  targetWidget = theTargetWidget;
  return self;
}

- createEnd
{
  [super createEnd];
  
  if (probedObject != nil)
    {
      tkobjc_dragAndDrop (self, targetWidget);
      tkobjc_bindButton3ForCompleteProbeDisplay (self,
                                                 probedObject);
    }
  
  [globalTkInterp eval: "bind %s <Enter> {%s configure -fg CornFlowerBlue}",
                  widgetName, widgetName];
  [globalTkInterp eval: "bind %s <Leave> {%s configure -fg blue}",
                  widgetName, widgetName];
  
  [self pack];
  
  return self;
}

PHASE(Using)

@end

