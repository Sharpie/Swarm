// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/XColormap.h>
#import <TkInterp.h>

@implementation XColormap
// create a new colourmap. Right now we use the Tk widget name
// just to get info about the X server connection. Later, we could
// use it for private colourmap installation, etc. For now, just
// init with the widget ".", that works fine.

- createEnd
{
  int i;
  
  [super createEnd];
  
  tkwin = Tk_NameToWindow ([globalTkInterp interp], 
                           ".",
                           [globalTkInterp mainWindow]);
  display = Tk_Display (tkwin);
  xwin = Tk_WindowId (tkwin);
  white = WhitePixel (display, DefaultScreen (display));
  black = BlackPixel (display, DefaultScreen (display));
  cmap = DefaultColormap (display, DefaultScreen (display));
  
  for (i = 0; i < MAXCOLORS; i++)
    isSet[i] = NO;
  return self;
}

// The colormap is a length MAXCOLORS array of PixelValues. We fill this array
// in as people ask for it. You're welcome to read this array yourself.
- (GUI_PixelValue *)map
{
  return map;
}

// get the particular colour associated with the entry. Error if it's not set.
- (GUI_PixelValue)pixelValue: (GUI_Color)c
{
  if ([self colorIsSet: c])
    return map[c];
  else
    {
      [InvalidArgument raiseEvent: "attempted to access unset color %d\n", c];
      return (GUI_PixelValue)white;
    }
}

// set a new colormap entry to something. Error if it's already set.
// we should do something to see if the colour already has been allocated
// in our colourmap. If it has, then we should somehow persuade the client
// to reuse that entry.
- (BOOL)setColor: (GUI_Color)c ToName: (const char *)colorName
{
  if ([self colorIsSet: c])
    {
      [InvalidArgument raiseEvent: "attempted to set color %d twice\n", c];
      return NO;
    }
  else
    {
      int rc;
      XColor xc, t;				  // ignore t
      
      isSet[c] = YES;
      rc = XAllocNamedColor (display, cmap, colorName, &xc, &t);
      if (rc == 0)
        {
          [ResourceAvailability
            raiseEvent:
              "Problem allocating color %s. Substituting white.\n",
            colorName];
          map[c] = white;
          return NO;
        }
      map[c] = (GUI_Color)xc.pixel;
      return YES;
    }
}

// allocate an RGB combo. We could use XAllocColor directly,
// but that requires rewriting more code.
- (BOOL)setColor: (GUI_Color)c
           ToRed: (double)r
           Green: (double)g
            Blue: (double)b
{
  unsigned ru, gu, bu;
  char colorName[1+2+2+2+1];			  // "#rrggbb\0"

  ru = r * 0xffU;
  gu = g * 0xffU;
  bu = b * 0xffU;
  
  sprintf (colorName, "#%02x%02x%02x", ru, gu, bu);
  return [self setColor: c ToName: colorName];
}

// allocate grey: just a convenience.
- (BOOL)setColor: (GUI_Color)c ToGrey: (double)g
{
  return [self setColor: c ToRed: g Green: g Blue: g];
}

// white and black are basic colours. (Should also be entered in the map,
// reserved. Oh well.)
- (GUI_PixelValue)white
{
  return white;
}

- (GUI_PixelValue)black
{
  return black;
}

// is the colour actually set?
- (BOOL)colorIsSet: (GUI_Color)c
{
  return isSet[c];
}

@end
