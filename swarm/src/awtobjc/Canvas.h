// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/Widget.h>

@interface Canvas: Widget
{
  jmethodID _placeObj, _moveObj, _drawLine, _setColor;
}

- createEnd;

- placeObj: (id)obj X: (int)x Y: (int)y;
- moveObj: (id)obj X: (int)x Y: (int)y;
- drawLineFX: (int)fx FY: (int)fy TX: (int)tx TY: (int)ty;
- setColor: (const char *)color;

@end
