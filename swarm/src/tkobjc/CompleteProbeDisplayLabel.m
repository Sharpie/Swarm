// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompleteProbeDisplayLabel.h>
#import <tkobjc/common.h>
#import <tkobjc/global.h>

static void
tkobjc_bindButton3ForCompleteProbeDisplay (id widget,
                                           id probedObject,
                                           id theProbeDisplayManager)
{
  // have to make a private copy of the return for objectToName.
  const char *pdmName = [theProbeDisplayManager getObjectName];
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

- setProbedObject: theProbedObject
{
  probedObject = theProbedObject;
  return self;
}

- setProbeDisplay: theProbeDisplay
{
  probeDisplay = theProbeDisplay;
  return self;
}

- setProbeDisplayManager: theProbeDisplayManager
{
  probeDisplayManager = theProbeDisplayManager;
  return self;
}

- createEnd
{
  [super createEnd];
  
  tkobjc_dragAndDrop (self, probeDisplay);
  tkobjc_bindButton3ForCompleteProbeDisplay (self,
                                             probedObject,
                                             probeDisplayManager);
  
  [globalTkInterp eval: "bind %s <Enter> {%s configure -fg CornFlowerBlue}",
                  widgetName, widgetName];
  [globalTkInterp eval: "bind %s <Leave> {%s configure -fg blue}",
                  widgetName, widgetName];
  
  [self pack];
  
  return self;
}

@end

