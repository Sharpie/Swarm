// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/CommonProbeDisplay.h>

@interface CompleteProbeDisplay : CommonProbeDisplay
{
  id widgets;
}

- createEnd;
- update;
- (void)drop;

@end
