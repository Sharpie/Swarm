// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/Colormap.h>

@implementation Colormap
// create a new colourmap. Right now we use the Tk widget name
// just to get info about the X server connection. Later, we could
// use it for private colourmap installation, etc. For now, just
// init with the widget ".", that works fine.

- createEnd
{
  int i;

  [super createEnd];

  white = 0xffffff;
  black = 0x000000;

  for (i = 0; i < MAXCOLORS; i++)
    isSet[i] = NO;
  return self;
}

// the colourmap is a length MAXCOLORS array of PixelValues. We fill this array
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
      [InvalidArgument raiseEvent: "attempted to access unset color %d\n", c];
      return white;
    }
}

// set a new colormap entry to something. Error if it's already set.
// we should do something to see if the colour already has been allocated
// in our colourmap. If it has, then we should somehow persuade the client
// to reuse that entry.
- (BOOL)setColor: (Color)c ToName: (const char *)colorName
{
  if (strcmp (colorName, "red") == 0)
    map[c] = 0xff0000;
  else if (strcmp (colorName, "green") == 0)
    map[c] = 0x00ff00;
  else if (strcmp (colorName, "blue") == 0)
    map[c] = 0x0000ff;
  else if (strcmp (colorName, "white") == 0)
    map[c] = 0xffffff;
  else
    [InvalidArgument
      raiseEvent: 
        "Unknown color %s.\n", colorName];
  return YES;
}

- (BOOL)setColor: (Color)c R: (unsigned)r G: (unsigned)g B: (unsigned)b
{
  if ([self colorIsSet: c])
    {
      [InvalidArgument raiseEvent: "attempted to set color %d twice\n", c];
      return NO;
    }
  else
    {
      isSet[c] = YES;
      map[c] = ((r & 0xff) << 16) | ((g & 0xff) << 8) | (b & 0xff);
      return YES;
    }
}

// allocate an RGB combo. We could use XAllocColor directly,
// but that requires rewriting more code.
- (BOOL)setColor: (Color)c ToRed: (double)r Green: (double)g Blue: (double)b
{
  unsigned ru, gu, bu;

  ru = r * 0xffU;
  gu = g * 0xffU;
  bu = b * 0xffU;

  return [self setColor: c R: ru G: gu B: bu];
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

@end


