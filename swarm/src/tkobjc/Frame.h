// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C encapsulation of toplevels and frames, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@interface Frame: ArchivedGeometryWidget <Frame>
{
  unsigned borderWidth;
  BOOL reliefFlag;
}

- setBorderWidth: (unsigned)borderWidth;
- setReliefFlag: (BOOL)reliefFlag;
- createEnd; // we override this.
- (void)withdraw;
- (void)deiconify;
@end
