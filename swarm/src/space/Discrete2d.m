// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Discrete2d.h>
#import <space.h>
#import <simtools.h> // InFile
#import <defobj.h> // ProtocolViolation
#import <defobj/internal.h> // map_ivars
#include <collections/predicates.h> // keywordp, stringp

#include <misc.h> // memset

// Note - this code assumes that ints can be written in where ids have been
// allocated. It uses casts to do this, and I think is portable to all
// modern architectures.

@implementation Discrete2d

PHASE(Creating)

+ createBegin: aZone
{
  Discrete2d *obj = [super createBegin: aZone];

  obj->useObjects = NO;
  obj->useValues = NO;

  return obj;
}

- setSizeX: (unsigned)x Y: (unsigned)y
{
  if (lattice)
    [InvalidArgument
      raiseEvent: "You cannot reset the grid size after creation.\n"];
  xsize = x;
  ysize = y;
  return self;
}

- (id *)allocLattice
{
  void *p;

  p = [[self getZone] alloc: xsize * ysize * sizeof (id)];
  memset (p, 0, xsize * ysize * sizeof (id));

  return p;
}

// Part of createEnd, really, but separated out for ease of inheritance.
- makeOffsets
{
  unsigned i;
  
  // precalculate offsets based on the y coordinate. This lets
  // us avoid arbitrary multiplication in array lookup.
  offsets = [[self getZone] alloc: ysize * sizeof(*offsets)];

  for (i = 0; i < ysize; i++)
    offsets[i] = xsize * i; // cache this multiplaction
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

PHASE(Setting)

- setLattice: (id *)theLattice
{
  lattice = theLattice;
  return self;
}

- setUseObjects
{
  useObjects = YES;
  return self;
}

- setUseValues
{
  useValues = YES;
  return self;
}

PHASE(Using)

- (unsigned)getSizeX
{
  return xsize;
}

- (unsigned)getSizeY
{
  return ysize;
}

- getObjectAtX: (unsigned)x Y: (unsigned)y
{
  return *discrete2dSiteAt(lattice, offsets, x, y);
}

- (long)getValueAtX: (unsigned)x Y: (unsigned)y
{
  return (long)*discrete2dSiteAt(lattice, offsets, x, y);
}

- putObject: anObject atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt(lattice, offsets, x, y) = anObject;
  return self;
}

- putValue: (long)v atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt(lattice, offsets, x, y) = (id) v;
  return self;
}

- fastFillWithValue: (long)aValue
{
  unsigned i, lcount;
  
  lcount = xsize * ysize ;
  
  for (i = 0; i < lcount; i++)
    lattice[i] = (id) aValue;

  return self;
}

- fastFillWithObject: anObj
{
  int i, lcount = xsize * ysize;

  for(i = 0; i < lcount; i++)
    lattice[i] = anObj;

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

- lispInCreate: expr
{
  // call the superclass
  [super lispIn: expr];
  return self;
}

- _lispInAttr_: obj
{
  // frob the created object here...
  return obj;
}

- _lispInAttrAsValues_: site
{
  unsigned x, y;
  long tempArray[xsize][ysize];
  memcpy ((void *)tempArray, [site getData], 
          [site getElementCount] * [site getElementSize]);

  for (x = 0; x < xsize; x++) 
    for (y = 0; y < ysize; y++)
      *discrete2dSiteAt (lattice, offsets, x, y) = (id) tempArray[x][y];

  // finally, set Discrete2d to store values
  useValues = YES;
  return self;
}

- _lispInAttrAsObjects_: li
{
  id aZone = [self getZone];  
  id site = [li get]; // index points to first stored lattice coord
  do {
    // expect a `pair' - the co-ordinate & object 
    if (pairp (site))
      { 
        unsigned tempX = 0, tempY = 0;            
        id coordExpr = [site getCar];
        id objExpr = [site getCdr];
        
        if (listp (coordExpr) && ([coordExpr getCount] == 2))
          {
            tempX = [[coordExpr getFirst] getInteger];
            tempY = [[coordExpr getLast] getInteger];
          }
        else
          raiseEvent (InvalidArgument, "Expecting a pair of integers");
        {
          id obj = lispIn (aZone, objExpr);
          *discrete2dSiteAt (lattice, offsets, tempX, tempY) =
            [self _lispInAttr_: obj];
        }
      }
    else
      raiseEvent(InvalidArgument, "Expecting a either cons pair or an array");
  }
  while ((site = [li next]));
  
  // finally, set Discrete2d to store objects
  useObjects = YES;
  return self;
}

- lispIn: expr
{
  id index, val, member;

  [super lispIn: expr];
  index = [(id) expr begin: scratchZone];
  while ((member = [index next]) != nil)
    {
      if (keywordp (member))
        {
          // check the name of the keyword
          if (strcmp ([member getKeywordName], "lattice") == 0)
            {
              if ((val = [index next]) == nil)
                raiseEvent (InvalidArgument, "missing value");

              // keyword `lattice' assumes a custom list to parse
              if (listp (val)) 
                {
                  id site;
                  id li = [(id) val begin: scratchZone];

                  // get first `parse' string
                  site = [li next];
                  if (stringp (site) &&
                      strcmp ([site getC], PARSE_FUNCTION_NAME) == 0)
                    {
                      // skip to next element after `parse' string
                      site = [li next];
                      
                      if (arrayp (site)) // dealing with a lattice of values
                        [self _lispInAttrAsValues_: site];
                      else    // dealing with a lattice of objects
                        [self _lispInAttrAsObjects_: li];
                    }
                  else
                    raiseEvent (InvalidArgument, "Expecting a %s",
                                PARSE_FUNCTION_NAME);
                  [li drop];
                }
              else
                raiseEvent (InvalidArgument, "Argument not a list");
            }
        }
    }
  [index drop];
  return self;
}

- lispOutShallow: outputCharStream
{
  raiseEvent (InvalidOperation, "can't shallow serialize a Discrete2d");
  return self;
}

- _lispOutAttr_: outputCharStream anObject: obj atX: (unsigned)x Y: (unsigned)y
{
  char buffer[2 * DSIZE (int) + 20];
  if (obj != nil)
    {
      sprintf(buffer, "  (cons '(%d %d)\n   ", x, y);
      [outputCharStream catC: buffer];    
      // do a deep serialization of the object at that point
      [obj lispOutDeep: outputCharStream];
      [outputCharStream catC: ")\n"];    
    }
  return self;
}

- lispOutDeep: stream
{
  [stream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " '"];
  [stream catC: [self getTypeName]];
  {
    void store_object (struct objc_ivar *ivar)
      {
        if ((*ivar->ivar_type != _C_PTR))
          {
            [stream catC: " #:"];
            [stream catC: ivar->ivar_name];
            [stream catC: " "];
            lisp_output_type (ivar->ivar_type,
                              (void *) self + ivar->ivar_offset,
                              0,
                              NULL,
                              stream,
                              YES);
          }
        
      }
    // do the general instance serialization
    map_ivars (getClass (self), store_object);

    // do the lattice-specific serialization
    if (useObjects)
      {
        int x, y;
        id obj;
        [stream catC: " #:lattice \n(parse "];
        for (x = 0; x < xsize; x++) {
          for (y = 0; y < ysize; y++)
            {
              obj = *discrete2dSiteAt (lattice, offsets, x, y);
              [self _lispOutAttr_: stream anObject: obj atX: x Y: y];
            }
        }
        [stream catC: ")"];
      }
    else if (useValues)
      {
        long tempArray[xsize][ysize];
        unsigned x, y;        
        char buf[2*DSIZE(unsigned) + 5];
        
        // generate compiler encoding for 2D array
        sprintf (buf, "%c%u%c%u%c%c%c", 
                 _C_ARY_B,xsize,_C_ARY_B, ysize,_C_LNG,_C_ARY_E,_C_ARY_E);
        
        [stream catC: " #:lattice \n (parse "];

        // unpack the array into the lattice data structure
        for (x = 0; x < xsize; x++) 
          for (y = 0; y < ysize; y++)
            tempArray[x][y] = (long) *discrete2dSiteAt (lattice, offsets, x, y);
       
        lisp_output_type (strdup (buf),
                          (void *) tempArray,
                          0,
                          NULL,
                          stream,
                          NO);
        [stream catC: ")"];
      }
    else
      raiseEvent (InvalidCombination, 
                  "need to specify one of useObjects or useValues");      
  }
  [stream catC: ")"];
  return self;
}

// Read in a file in PGM format and load it into a discrete 2d.
// PGM is a simple image format. It stores grey values for a 2d array.
- (int)setDiscrete2d: a toFile: (const char *)filename 
{
  id <InFile> f;
  char c1, c2;
  int maxValue;
  unsigned x, y;
  unsigned fileXSize, fileYSize;
  
  if (![a conformsTo: @protocol (Discrete2d)])
    [ProtocolViolation
      raiseEvent:
        "Object `%s' does not comply to Discrete2d protocol\n",
      [a name]];

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
  unsigned x, y;

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
 
- (void)drop
{
  [[self getZone] free: lattice];
  [super drop];
}
@end
