// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/SimpleProbeDisplayHideButton.h>

@implementation SimpleProbeDisplayHideButton

- setFrame: theFrame
{
  frame = theFrame;
  return self;
}

- setProbeDisplay: theProbeDisplay
{
  probeDisplay = theProbeDisplay;
  return self;
}

- createEnd
{
  [super createEnd];

  printf ("SimpleProbeDisplayHideButton\n");
  return self;
}

@end

