// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// NSelect -> select exactly N elements from a collection 
// without repetition! The target collection must be provided.

#import <simtools.h> // NSelect
#import <objectbase/SwarmObject.h>

@interface NSelect: SwarmObject <NSelect>
{
}

+ (void)select: (int) n from: aCollection into: bCollection;

@end

