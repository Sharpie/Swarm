// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C encapsulation of toplevels and frames, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@interface Frame: ArchivedGeometryWidget <Frame>
{
  int borderWidth;
  BOOL reliefFlag;
}

- createEnd; // we override this.
- withdraw;
- deiconify;
- setBorderWidth: (int)borderWidth;
- setReliefFlag: (BOOL)reliefFlag;
@end
