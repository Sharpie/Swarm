// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompleteProbeDisplayLabel.h>
#import <tkobjc/common.h>
#import <tkobjc/global.h>

extern id probeDisplayManager;

static void
tkobjc_bindButton3ForCompleteProbeDisplay (id widget,
                                           id probedObject)
{
  // have to make a private copy of the return for objectToName.
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
      tkobjc_dragAndDrop (self, probeDisplay);
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

@end

