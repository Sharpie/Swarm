// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// quickie interface for a label

#import <objc/Object.h>
#import <tkobjc/Widget.h>

@interface Label : Widget {
}

-setText: (char *) t;				  // initialize the entry

@end
