// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"
#import <tkobjc/global.h>
#import <tkobjc/Colormap.h>
#import <gui.h>

@implementation Colormap

PHASE(Creating)

- createEnd
{
  int i;
  int screen;
  Display *display;
  
  [super createEnd];
  
  tkwin = tkobjc_nameToWindow (".");
  display = Tk_Display (tkwin);
  screen = DefaultScreen (display);
  white = WhitePixel (display, screen);
  black = BlackPixel (display, screen);
  cmap = DefaultColormap (display, screen);
  
  for (i = 0; i < MAXCOLORS; i++)
    isSet[i] = NO;

  return self;
}

PHASE(Using)

// The colormap is a length MAXCOLORS array of PixelValues. We fill this array
// in as people ask for it. You're welcome to read this array yourself.
- (PixelValue *)map
{
  return map;
}

// get the particular colour associated with the entry. Error if it's not set.
- (PixelValue)pixelValue: (Color)c
{
  if ([self colorIsSet: c])
    return map[c];
  else
    {
      raiseEvent (InvalidArgument,"attempted to access unset color %d\n", c);
      return white;
    }
}

// set a new colormap entry to something. Error if it's already set.
// we should do something to see if the colour already has been allocated
// in our colourmap. If it has, then we should somehow persuade the client
// to reuse that entry.
- (BOOL)setColor: (Color)c ToName: (const char *)colorName
{
  if (isSet[c])
    raiseEvent (InvalidArgument, "attempted to set color %d twice\n", c);
  else
    isSet[c] = tkobjc_setColor (self, colorName, &map[c]);
  return isSet[c];
}

// allocate an RGB combo. We could use XAllocColor directly,
// but that requires rewriting more code.
- (BOOL)setColor: (Color)c
           ToRed: (double)r
           Green: (double)g
            Blue: (double)b
{
  unsigned ru, gu, bu;
  char colorName[4+3+3+3+1];

  ru = r * 256;
  gu = g * 256;
  bu = b * 256;

  if (ru > 255)
    ru = 255;

  if (gu > 255)
    gu = 255;

  if (bu > 255)
    bu = 255;

#if 0
  sprintf (colorName, "rgb:%02x/%02x/%02x", ru, gu, bu);
#else
  sprintf (colorName, "#%02x%02x%02x", ru, gu, bu);
#endif
  return [self setColor: c ToName: colorName];
}

// allocate grey: just a convenience.
- (BOOL)setColor: (Color)c ToGrey: (double)g
{
  return [self setColor: c ToRed: g Green: g Blue: g];
}

// white and black are basic colours. (Should also be entered in the map,
// reserved. Oh well.)
- (PixelValue)white
{
  return white;
}

- (PixelValue)black
{
  return black;
}

// is the colour actually set?
- (BOOL)colorIsSet: (Color)c
{
  return isSet[c];
}

- (Color)nextFreeColor
{
  Color i;

  for (i = MAXCOLORS - 1; i > 0; i--)
    if (isSet[i - 1])
      return i;
  raiseEvent (ResourceAvailability, "No free colors");
  return 0;
}

- unsetColor: (Color)c
{
  isSet[c] = NO;
  return self;
}

@end
