// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Entry.h>
#import <tkobjc/global.h>
#include <misc.h> // abort

@implementation Entry

PHASE(Creating)

- createEnd
{
  [super createEnd];
  
  // create the Entry
  [globalTkInterp eval: "entry %s; %s configure -width 10 -relief sunken;",
                  widgetName, widgetName];
  [globalTkInterp eval: "%s configure -textvariable %s;",
                  widgetName, variableName];
  return self;
}

PHASE(Using)

- setValue: (const char *)t
{
  [globalTkInterp eval: "%s delete 0 end; %s insert 0 \"%s\"; %s xview 0",
		  widgetName, widgetName, t, widgetName];
  return self;
}

- setHeight: (unsigned)h
{
  abort ();
}

@end

