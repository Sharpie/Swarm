// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface WindowGeometryRecord : SwarmObject
{
  id windowGeometryString;
}
- (const char *)getWindowGeometry;
- setWindowGeometry : (const char *)theWindowGeometryString;
- (void)describe : outputCharStream;

- in : expr;
+ in : aZone expr: expr;
- out : outputCharStream;
@end
