// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Many thanks to JJ Merelo for helping with this widget.

#import <awtobjc/InputWidget.h>

@interface CheckButton : InputWidget {
}

-(BOOL) getBoolValue;
-setBoolValue: (BOOL) v;
@end
