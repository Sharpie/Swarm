// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/Raster.h>

@implementation Raster

- createEnd
{
  [self addInt: 100U];
  [self addInt: 100U];
  [super createEnd];
  [self erase];
  return self;
}

// This widget won't work without this initialized.
- setColormap: (id <Colormap>)c
{
  colormap = c;
  map = [colormap map];				  // cache this, fast access.
  return self;
}

- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  unsigned minWidth, minHeight;

  minWidth = (newWidth < width ? newWidth : width);
  minHeight = (newHeight < height ? newHeight : height);
  
  width = newWidth;
  height = newHeight;
  
  [super setWidth: width Height: height];
  [self erase];

  return [self drawSelf];
}

- erase
{
  if (_erase == 0)
    _erase = [self findMethod: "erase" signature: "()V"];

  [self callVoidMethod: _erase];
  return self;   
}

// draw a point on the window.
- drawPointX: (int)x Y: (int)y Color: (Color)c
{
  if (_drawPoint == 0)
    _drawPoint = [self findMethod: "drawPoint" signature: "(III)V"];
  
  [self callVoidMethod: _drawPoint : x : y : map[c]];
  return self;  
}

// draw a rectangle.
- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  if (_fillRectangle == 0)
    _fillRectangle = [self findMethod: "fillRectangle"
                           signature: "(IIIII)V"];
  
  [self callVoidMethod: _fillRectangle : x0 : y0 : x1 : y1 : map[c]]; 
  return self;
}

// copy the pixmap onto the X window.
- drawSelf
{
  if (_drawSelf == 0)
    _drawSelf = [self findMethod: "drawSelf" signature: "()V"];
  printf ("Raster drawSelf\n");
  [self callVoidMethod: _drawSelf];  
  return self;
}

// if a client is registered, then the specified selector is called with
// the x and y coordinates of a button press.
- setButton: (int)n Client: c Message: (SEL)sel
{
  switch (n)
    {
    case ButtonLeft: button1Client = c; button1Sel = sel; break;
    case ButtonMiddle: button2Client = c; button2Sel = sel; break;
    case ButtonRight: button3Client = c; button3Sel = sel; break;
    default:
      raiseEvent (WarningMessage,
                  "Don't know how to handle button %d, ignoring.\n", n);
    }

  return self;
}

- (void)drop
{
  [super drop];
}

@end
