// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Button.h>

#include <objc/objc-api.h>
#include <misc.h> // strcpy, stpcpy

@implementation Button

PHASE(Creating)

- createEnd
{
  [super createEnd];

  [globalTkInterp eval: "button %s", widgetName];
  
  return self;
}

PHASE(Using)

- setText: (const char *)text
{
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, text];
  return self;
}

- setButtonTarget: target method: (SEL)sel
{
  char bcmd[1024], *p;
  
  p = stpcpy (bcmd, [target getObjectName]);
  p = stpcpy (p, " ");
  strcpy (p, sel_get_name (sel));
  [globalTkInterp eval: "%s configure -command \"%s\"", widgetName, bcmd];
  
  return self;
}

@end
