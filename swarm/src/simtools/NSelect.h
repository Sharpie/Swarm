// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// NSelect -> select exactly N elements from a collection 
// without repetition! The target collection must be provided.

#import <objectbase/SwarmObject.h>

@interface NSelect : SwarmObject {
}

+(void) select: (int) n from: aCollection into: bCollection ;

@end
