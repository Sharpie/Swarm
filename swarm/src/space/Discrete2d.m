// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Discrete2d.h>
#import <space.h>
#import <simtools.h> // InFile

// Note - this code assumes that ints can be written in where ids have been
// allocated. It uses casts to do this, and I think is portable to all
// modern architectures.

@implementation Discrete2d

- setSizeX: (int)x Y: (int)y
{
  if (lattice)
    [InvalidArgument
      raiseEvent: "You cannot reset the grid size after creation.\n"];
  xsize = x;
  ysize = y;
  return self;
}

- createEnd
{
  if (xsize <= 0 || ysize <= 0)
    [InvalidCombination
      raiseEvent: "invalid size in creation of Discrete2d\n"];
  lattice = [self allocLattice];
  [self makeOffsets];
  
  return self;
}

- (id *)allocLattice
{
  void *p;

  p = [[self getZone] alloc: xsize * ysize * sizeof (id)];
  memset(p, 0, xsize * ysize * sizeof (id));
  return p;
}

// Part of createEnd, really, but separated out for ease of inheritance.
- makeOffsets
{
  int i;
  
  // precalculate offsets based on the y coordinate. This lets
  // us avoid arbitrary multiplication in array lookup.
  offsets = [[self getZone] alloc: ysize * sizeof(*offsets)];

  for (i = 0; i < ysize; i++)
    offsets[i] = xsize * i;                       // cache this multiplaction
  return self;
}

- (int)getSizeX
{
  return xsize;
}

- (int)getSizeY
{
  return ysize;
}

- getObjectAtX: (int)x Y: (int)y
{
  return *discrete2dSiteAt(lattice, offsets, x, y);
}

- (long)getValueAtX: (int)x Y: (int)y
{
  return (long)*discrete2dSiteAt(lattice, offsets, x, y);
}

- putObject: anObject atX: (int)x Y: (int)y
{
  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;
  return self;
}

- putValue: (long)v atX: (int)x Y: (int)y
{
  *discrete2dSiteAt(lattice, offsets, x, y) = (id) v;
  return self;
}

- fastFillWithValue: (long)aValue
{
  int i, lcount ;
  
  lcount = xsize * ysize ;
  
  for(i = 0 ; i < lcount ; i++)
    lattice[i] = (id) aValue ;

  return self ;
}

- fastFillWithObject: anObj
{
  int i, lcount ;
  
  lcount = xsize * ysize ;

  for(i = 0 ; i < lcount ; i++)
    lattice[i] = anObj ;

  return self ;
}

- fillWithValue: (long)aValue
{
  unsigned x, y;

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      [self putValue: aValue atX: x Y: y];

  return self;
}

- fillWithObject: anObj
{
  unsigned x, y;
  
  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      [self putObject: anObj atX: x Y: y];
  
  return self;
}

- (id *)getLattice
{
  return lattice;
}

- (long *)getOffsets
{
  return offsets;
}

// Utility methods - these should be in the Swarm libraries.
// Read in a file in PGM format and load it into a discrete 2d.
// PGM is a simple image format. It stores grey values for a 2d array.
- (int)setDiscrete2d: a toFile: (const char *)filename 
{
  id <InFile> f;
  char c1, c2;
  int x, y, fileXSize, fileYSize, maxValue;
  
  if (![a conformsTo: @protocol (Discrete2d)])
    [ProtocolViolation
      raiseEvent:
        "Object `%s' does not comply to Discrete2d protocol\n"];

  // open the file
  f = [InFile create: [self getZone] withName: filename];

  // The first two characters should be P2, the PGM ASCII header (not P5 - raw)
  [f getChar: &c1];
  [f getChar: &c2];
  if (c1 != 'P' && c2 != '2')
    [WarningMessage raiseEvent: "File is not in PGM ascii format. Faking it.\n"];

  // Next two entries are the size in pixels
  [f getInt: &fileXSize];
  [f getInt: &fileYSize];
  if (fileXSize != [a getSizeX] || fileYSize != [a getSizeY]) {
    [WarningMessage raiseEvent: "PGM File is not the right size. Faking it.\n"];
  }

  // Finally, the maximum value (typically 255, but sometimes less).
  [f getInt: &maxValue];
  maxValue++;					  // [0, maxValue)

  // Yay! Now we can read in a bunch of integers for the values themselves.
  // This code could be modified to read P5 type PGMs by reading raw bytes.
  for (y = 0; y < ysize; y++)
    {
      for (x = 0; x < xsize; x++)
        {
          int v;
          if ([f getInt: &v] != 1)
            {
            [WarningMessage
              raiseEvent: "Ran out of data reading PGM file. Aborting.\n"];
            goto finishReading;
            }
          [a putValue: v atX: x Y: y];
        }
    }
  
 finishReading:
  [f drop];				  // close the file
  return maxValue;
}

// A similar method should be written to *write* a Discrete2d to a file.

// Copy one Discrete2d's contents to another.
// This could probably use the fast accessor macros.
- copyDiscrete2d: a toDiscrete2d: b
{
  int x, y;

  if (![a conformsTo: @protocol (Discrete2d)])
    [ProtocolViolation
      raiseEvent:
        "Object a `%s' does not comply to Discrete2d protocol\n",
      [a name]];

  if (![b conformsTo: @protocol (Discrete2d)])
    [ProtocolViolation
      raiseEvent:
        "Object b `%s' does not comply to Discrete2d protocol\n",
      [b name]]; 
 
  if ([a getSizeX] != [b getSizeX] || [a getSizeY] != [b getSizeY])
    [InvalidArgument raiseEvent: "Two Discrete2ds aren't the same size."];
  for (x = 0; x < [a getSizeX]; x++)
    for (y = 0; y < [b getSizeY]; y++)
      [b putValue: [a getValueAtX: x Y: y] atX: x Y: y];
  return self;
}

@end
