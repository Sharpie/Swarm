// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// QSort -> QuickSorts a collection!
//
// The values will appear in ascending order.

#import <swarmobject/SwarmObject.h>

@interface QSort : SwarmObject {
}

+(void) sortObjectsIn: aCollection ;
+(void) sortNumbersIn: aCollection ;

@end
