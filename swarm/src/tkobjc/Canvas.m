// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <tkobjc/global.h>
#import <tkobjc/Canvas.h>

@implementation Canvas

- createEnd
{
  [super createEnd];
  [globalTkInterp eval: "canvas %s", widgetName];
  return self;
}

@end
