// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"

#import <tkobjc/Form.h>
#import <tkobjc/Label.h>
#import <tkobjc/Entry.h>
#import <tkobjc/CheckButton.h>
#import <tkobjc/global.h>
#include <misc.h> // abort

@implementation Form

PHASE(Creating)

// a Form is itself just a frame (are we creating an extra frame than we need?)
- createEnd
{
  [super createEnd];
  [globalTkInterp eval: "frame %s", widgetName];
  return self;
}

PHASE(Using)

- (void)setEntryWidth: (unsigned)ew
{
  entryWidth = ew;
}

- (void)_addLineName_: (const char *)n Variable: (void *)p Type: (int)type
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
    entryCount];

  if (type == TCL_LINK_BOOLEAN)
    {
      w = [CheckButton createParent: parent];
      [w linkVariableBoolean: p];
    }
  else 
    {
      w = [Entry createParent: parent];
      [w setWidth: entryWidth];

      if (type == TCL_LINK_INT)
        [w linkVariableInt: p];
      else if (type == TCL_LINK_DOUBLE)
        [w linkVariableDouble: p];
      else 
        abort ();
    }
  [globalTkInterp eval: "table %s %s %d,1 -anchor w -fill x",
		  [parent getWidgetName],
                  [w getWidgetName],
                  entryCount];
  
  // only have to call this once, not once per item, but oh well.
  [globalTkInterp eval: "table configure %s c0 -resize none",
		  [parent getWidgetName]];
  entryCount++;
}

// this is atrocious - we should maintain a collection of the entries
- (void)addLineName: (const char *)n Boolean: (unsigned *)p
{
  [self _addLineName_: n Variable: p Type: TCL_LINK_BOOLEAN];
}

- (void)addLineName: (const char *)n Int: (int *)p
{
  [self _addLineName_: n Variable: p Type: TCL_LINK_INT];
}

- (void)addLineName: (const char *)n Double: (double *)p
{
  [self _addLineName_: n Variable: p Type: TCL_LINK_DOUBLE];
}

@end
