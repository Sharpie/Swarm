// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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
  [globalTkInterp eval: "%s delete 0 end; %s insert 0 {%s}; %s xview 0",
		  widgetName, widgetName, t, widgetName];
  return self;
}

- (const char *)getValue
{
  [globalTkInterp eval: "%s get", widgetName];

  return [globalTkInterp result];
}

- setHeight: (unsigned)h
{
  abort ();
}

@end

