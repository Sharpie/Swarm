// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <tk.h>
#import <tkobjc/XDrawer.h>

@interface XPixmap : CreateDrop <XDrawer>
{
  const char *filename;
  Display *display;				  // should be global variable
  Pixmap pixmap;				  // the map
  Pixmap mask;					  // clipping mask
  unsigned width, height;
}

- setFile: (const char *)filename;
- createEnd;

- (Pixmap)getPixmap;
- (BOOL)getMasked;
- (Pixmap)getMask;
- (unsigned)getWidth;
- (unsigned)getHeight;

@end
