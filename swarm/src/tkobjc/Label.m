// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Label.h>

@implementation Label

PHASE(Creating)
- createEnd
{
  [super createEnd];

  // create the label
  [globalTkInterp eval: "label %s", widgetName];
  
  return self;
}

PHASE(Using)
- setText: (const char *)text
{
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, text];
  return self;
}

@end

