// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/global.h>
#import <tkobjc/XPixmap.h> // XPixmap

@implementation XPixmap

- setFile: (const char *)s
{
  if (filename)
    {
      [InvalidCombination
        raiseEvent:
          "It is an error to reset the filename\n"];
      return nil;
    }
  else
    filename = s;
  return self;
}

// all this work to get the display.. tclobjc should support this
// directly.
- createEnd
{
  Tk_Window tkwin;
  XpmAttributes xpmattrs;
  int rc;
  
  [super createEnd];

  tkwin = tkobjc_nameToWindow (".");
  display = Tk_Display (tkwin);
  
  xpmattrs.valuemask = 0;			  // no input
  rc = XpmReadFileToPixmap (display,
                            XDefaultRootWindow (display),
                            (char *)filename,
                            &pixmap,
                            &mask,
                            &xpmattrs);
  if (rc != 0)
    {
      char *error = NULL;
      char *warning = NULL;
      
      switch (rc)
        {
        case XpmSuccess:
          break;
        case XpmColorError:
          warning = "Could not parse or alloc requested color";
          break;
        case XpmOpenFailed:
          error = "Cannot open file";
          break;
        case XpmFileInvalid:
          error = "Invalid XPM file";
          break;
        case XpmNoMemory:
          error = "Not enough memory";
          break;
        case XpmColorFailed:
          error = "Failed to parse or alloc some color";
          break;
        }
      if (warning)
        [Warning raiseEvent: "Warning loading pixmap %s: %s\n",
                 filename, warning];
      if (error)
        {
          [WindowCreation raiseEvent: "Error loading pixmap %s: %s\n",
                          filename, error];
        }
    }
  width = xpmattrs.width;
  height = xpmattrs.height;
  return self;
}

- (Pixmap)getPixmap
{
  return pixmap;
}

- (Pixmap)getMask
{
  return mask;
}

// do we have a mask set?
- (BOOL)getMasked
{
  return mask ? 1 : 0;
}

- (unsigned)getWidth
{
  return width;
}

- (unsigned)getHeight
{
  return height;
}

- drawX: (int)x Y: (int)y raster: (Raster *)raster;
{
  GC gc = raster->gc;
  Drawable w = raster->pm;

  if (mask == 0)				  // handle mask?
    XCopyArea (display, pixmap, w, gc, 0, 0, width, height, x, y);
  else
    {					  // doesn't work :-(
      XSetClipMask (display, gc, mask);
      XCopyArea (display, pixmap, w, gc, 0, 0, width, height, x, y);
      XSetClipMask (display, gc, None);		  // should reset.
    }
  return self;
}

@end
