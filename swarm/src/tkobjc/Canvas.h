// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@interface Canvas: ArchivedGeometryWidget <Canvas>
{
}

- createEnd;
- addWidget: widget X: (int)x Y: (int)y centerFlag: (BOOL)centerFlag;
- removeWidget: widget;
- checkGeometry: frame;
@end
