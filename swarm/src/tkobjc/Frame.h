// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C encapsulation of toplevels and frames, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>

@interface Frame: ArchivedGeometryWidget
{
  int borderWidth;
  BOOL reliefFlag;
}

- createEnd; // we override this.
- (void)drop;
- assertGeometry;
- assertPosition;
- withdraw;
- deiconify;
- setBorderWidth: (int)borderWidth;
- setReliefFlag: (BOOL)reliefFlag;
@end
