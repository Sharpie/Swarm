// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <TkInterp.h>
#import <tkobjc/Button.h>

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

-setCommand: (const char *)command
{
  [globalTkInterp eval: "%s configure -command \"%s\"", widgetName, command];
  return self;
}

@end

