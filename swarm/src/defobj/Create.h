// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Create.h
Description:  superclasses to implement object life cycle
Library:      defobj
*/

#import <defobj/Customize.h>

//
// CreateDrop_s -- superclass to create object with retained zone for drop
//
@interface CreateDrop_s : Customize_s // <Drop>
{
@public
  id <Zone>  zone;  // zone in which object allocated
}
/*** methods implemented in .m file ***/
+ create: aZone;
+ createBegin: aZone;
- createEnd;
- getZone;
- (void) drop;
@end

@interface CreateDrop : CreateDrop_s
- createEnd;
@end

//
// Drop_s -- superclass containing only the Using phase of CreateDrop_s
//
@interface Drop_s : Object_s
{
@public
  id <Zone>  zone;  // zone in which object allocated
}
/*** methods implemented in .m file ***/
- (void) dropFrom: aZone;
- getZone;
- (void) drop;
@end
