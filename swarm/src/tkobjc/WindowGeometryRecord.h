// Swarm library. Copyright © 1996-2000 Swarm Development Group.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <gui.h>
#import <defobj.h> // Serialization

@interface WindowGeometryRecord: CreateDrop <WindowGeometryRecord, Serialization>
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

- lispIn: expr;
- lispOutShallow: outputCharStream;
- lispOutDeep: outputCharStream;
@end
