// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/Form.h>
#import <tkobjc/Label.h>
#import <tkobjc/Entry.h>
#import <tkobjc/CheckButton.h>

@implementation Form

// a Form is itself just a frame (are we creating an extra frame than we need?)
- createEnd
{
  [super createEnd];
  [globalTkInterp eval: "frame %s", widgetName];
  return self;
}

- setEntryWidth: (int)ew
{
  entryWidth = ew;
  return self;
}

// this is atrocious - we should maintain a collection of the entries
- addLineName: (const char *)n Variable: (void *)p Type: (int)type
{
  Label *l;
  InputWidget *w;
  
  l = [Label createParent: parent];
  [l setText: n];
  [globalTkInterp 
    eval:
      "table %s %s %d,0 -anchor e -fill none",
    [parent getWidgetName],
    [l getWidgetName],
    numEntries];
  
  switch (type)
    {
    case TCL_LINK_BOOLEAN:
      w = [CheckButton createParent: parent];
      break;
    default:
      w = [Entry createParent: parent];
      [w setWidth: entryWidth Height: 1];
      break;
    }
  [w linkVariable: p Type: type];
  [globalTkInterp eval: "table %s %s %d,1 -anchor w -fill x",
		  [parent getWidgetName],
                  [w getWidgetName],
                  numEntries];
  
  // only have to call this once, not once per item, but oh well.
  [globalTkInterp eval: "table configure %s c0 -resize none",
		  [parent getWidgetName]];
  numEntries++;
  return self;
}

@end
