// Swarm library. Copyright (C) 1996-1997-1997 Santa Fe Institute.
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
@interface CreateDrop_s : Customize_s
/*** methods in CreateDrop_s (inserted from .m file by m2h) ***/
+ create: aZone;
+ createBegin: aZone;
- createEnd;
@end

@interface CreateDrop : CreateDrop_s
/*** methods in CreateDrop (inserted from .m file by m2h) ***/
- createEnd;
@end
