// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <gui.h>

extern id <Error> WindowGeometryRecordError;

@interface WindowGeometryRecord: SwarmObject <_WindowGeometryRecord>
{
  BOOL positionFlag, sizeFlag;
  unsigned width, height;
  int x, y;
}
- setX: (int)x Y: (int)y;
- setWidth: (unsigned)w Height: (unsigned)h;
- (BOOL)getSizeFlag;
- (BOOL)getPositionFlag;
- (int)getX;
- (int)getY;
- (unsigned)getWidth;
- (unsigned)getHeight;

- in: expr;
+ in: aZone expr: expr;
- out: outputCharStream;
@end
