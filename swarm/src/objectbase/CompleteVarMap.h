// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// A map containing all the variables of a given class but none of the 
// messages!

#import <objectbase/ProbeMap.h>

@interface CompleteVarMap: ProbeMap <CompleteVarMap>
{
}

- createEnd;
@end
