// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Canvas.h>
#import <gui.h>

@interface ProbeCanvas: Canvas <ProbeCanvas>
{
  BOOL horizontalScrollbarFlag;
}

- setHorizontalScrollbarFlag: (BOOL)horizontalScorllbarFlag;
- createEnd;
@end
