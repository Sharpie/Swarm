// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/XPixmap.h>
#import <tkobjc/TkExtra.h>
#import <X11/xpm.h>

@implementation XPixmap

-setFile: (const char *)s
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
  
  tkwin = Tk_NameToWindow ([globalTkInterp interp],
                           ".",
                           [globalTkInterp mainWindow]);
  display = Tk_Display (tkwin);
  
  xpmattrs.valuemask = 0;			  // no input
  rc = XpmReadFileToPixmap (display,
                            XDefaultRootWindow(display),
                            (char *)filename,
                            &pixmap,
                            &mask,
                            &xpmattrs);
  if (rc != 0)
    [WindowCreation raiseEvent: "Error loading pixmap %s\n", filename];
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

- drawOn: (Drawable)w X: (int)x Y: (int)y GC: (GC)gc Caller: caller
{
  if (mask == 0)				  // handle mask?
    XCopyArea(display, pixmap, w, gc, 0, 0, width, height, x, y);
  else
    {					  // doesn't work :-(
      XSetClipMask(display, gc, mask);
      XCopyArea(display, pixmap, w, gc, 0, 0, width, height, x, y);
      XSetClipMask(display, gc, None);		  // should reset.
    }
  return self;
}

@end
