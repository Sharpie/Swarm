// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Button.h>
#include <misc.h> // strcpy, stpcpy

@implementation Button

- createEnd
{
  [super createEnd];

  // create the button
  [globalTkInterp eval: "button %s", widgetName];
  
  return self;
}

- setText: (const char *)text
{
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, text];
  return self;
}

- setButtonTarget: target method: (SEL)sel
{
  char bcmd[1024], *p;
  
  p = stpcpy (bcmd, [target getObjectName]);
  strcpy (p, " dynamic");
  [globalTkInterp eval: "%s configure -command \"%s\"", widgetName, command];
  
  return self;
}

@end
