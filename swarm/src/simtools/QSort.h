// Swarm library. Copyright (C) 1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// QSort -> QuickSorts a collection!
//
// The values will appear in ascending order by default.
// Reverse order can be obtained by calling reverseOrderOf

#import <swarmobject/SwarmObject.h>

@interface QSort : SwarmObject {
}

+(void) sortObjectsIn: aCollection ;
+(void) sortObjectsIn: aCollection using: (SEL) aSelector ;
+(void) sortNumbersIn: aCollection ;
+(void) sortNumbersIn: aCollection
                using: (int(*)(const void*,const void*)) comp_fun ;
+(void) reverseOrderOf: aCollection ;

@end
