// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// quickie interface for a control panel of buttons

#import <tkobjc/Widget.h>

@interface Button : Widget {
}

-setText: (char *) t;				  // give the button a name
-setCommand: (char *) c;			  // give the button a cmd

@end
