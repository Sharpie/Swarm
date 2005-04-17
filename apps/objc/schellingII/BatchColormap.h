//Paul Johnson, copyright 2000-2004

// The code is made available under the provisions of the GNU Greater
// Public License (version 2 or newer, at the user's discretion).  That
// license is available with this package in a file called "gpl.txt".
// The code is made available without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.

// Class that can serve up a colormap to BatchRaster when
// the GUI isn't running. Modified from code by Nelson:
// Benedikt Stefansson <benedikt@ucla.edu>. First version 7/23/97
// Modified by Paul Johnson 1999-2003


#import <objectbase.h>
#import <objectbase/Swarm.h>
#import <gui.h> // Needed to make Colormap use Color as type

@class BatchColormap;
@class RGB;

#define NUMNAMES 752
#define MAXCOLORS 256



@interface BatchColormap:Swarm {
  BOOL isSet[MAXCOLORS];
  RGB * xcolors[MAXCOLORS];
}

-(RGB *) rgbValue: (int) c; 
-(BOOL) setColor: (Color) c ToName: (const char *) colorName;
-(BOOL) setColor: (int) c ToGrey: (double) g;
-(BOOL) setColor: (int) c ToRed: (double) r Green: (double) g Blue: (double) b;
-(BOOL) colorIsSet: (int) c;

@end

@interface RGB: Swarm {
  unsigned red;
  unsigned green;
  unsigned blue;
}
-setRed: (unsigned) r Green: (unsigned) g Blue: (unsigned) b;
-(int) getRed;
-(int) getGreen;
-(int) getBlue;

@end

