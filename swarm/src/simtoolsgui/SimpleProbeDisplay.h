// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/SingleProbeDisplay.h>
#import <gui.h>

@interface SimpleProbeDisplay: SingleProbeDisplay
{
  id probeMap;
  id <Frame> leftFrame, rightFrame, middleFrame, bottomFrame;
  id <CompleteProbeDisplayLabel> myTitle;
  int numberOfProbes;
  id <Widget> *widgets;
}

- setProbeMap: probeMap;
- createEnd;
- getProbeMap;
- update;
- (void)drop;
@end
