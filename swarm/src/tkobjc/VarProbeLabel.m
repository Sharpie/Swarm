// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/VarProbeLabel.h>

@implementation VarProbeLabel

- createEnd
{
  [super createEnd];

  [globalTkInterp eval: "%s configure -anchor e", widgetName];

  return self;
}

@end

