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

- setSizeX: (unsigned)x Y: (unsigned)y
{
  if (lattice)
    raiseEvent (InvalidArgument,
                "You cannot reset the grid size after creation.\n");
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
  offsets = [[self getZone] alloc: ysize * sizeof (*offsets)];

  for (i = 0; i < ysize; i++)
    offsets[i] = xsize * i; // cache this multipliction
  return self;
}

- createEnd
{
  if (xsize <= 0 || ysize <= 0)
    raiseEvent (InvalidArgument, "invalid size in creation of Discrete2d\n");
  lattice = [self allocLattice];
  [self makeOffsets];
  return self;
}

- lispInCreate: expr
{
  [super lispIn: expr];
  return self;
}

- hdf5InCreate: hdf5Obj
{
  if ([hdf5Obj checkDatasetName: "lattice"])
    {
      id latticeDataset = [[[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                setParent: hdf5Obj]
                               setCreateFlag: NO]
                              setDatasetFlag: YES]
                             setName: "lattice"]
                            createEnd];
      
      if ([latticeDataset getDatasetRank] != 2)
        raiseEvent (InvalidArgument, "Rank of lattice dataset must be 2");
      
      xsize = [latticeDataset getDatasetDimension: 0];
      ysize = [latticeDataset getDatasetDimension: 1];
      
      [latticeDataset drop];
    }
  else if ([hdf5Obj getCompoundType])
    {
      unsigned i, c_count = [hdf5Obj getCount];
      const char **rowNames = [hdf5Obj readRowNames];
      unsigned xmax = 0, ymax =0;
      
      for (i = 0; i < c_count; i++)
        {
          unsigned x, y;

          sscanf (rowNames[i], "%u,%u", &x, &y);
          if (x > xmax)
            xmax = x;
          if (y > ymax)
            ymax = y;
        }
      xsize = xmax + 1;
      ysize = ymax + 1;
      XFREE (rowNames); // but not the contents
    }
  else
    {
      BOOL gotX = NO, gotY = NO;

      int process_object (id component)
        {
          const char *name = [component getName];

          if (strcmp (name, "xsize") == 0)
            {
              [component loadDataset: &xsize];
              gotX = YES;
            }
          else if (strcmp (name, "ysize") == 0)
            {
              [component loadDataset: &ysize];
              gotY = YES;
            }
          return gotX && gotY;
        }
      [hdf5Obj iterate: process_object];
      if (!gotX)
        raiseEvent (InvalidArgument, "missing xsize");
      if (!gotY)
        raiseEvent (InvalidArgument, "missing ysize");
    }
  return self;
}

PHASE(Setting)

- setLattice: (id *)theLattice
{
  lattice = theLattice;
  return self;
}

- hdf5In: hdf5Obj
{
  id aZone = [self getZone];

  if ([hdf5Obj checkDatasetName: "lattice"])
    {
      {
        id latticeDataset = [[[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                  setParent: hdf5Obj]
                                 setCreateFlag: NO]
                                setDatasetFlag: YES]
                               setName: "lattice"]
                              createEnd];
        [latticeDataset loadDataset: lattice];
        [latticeDataset drop];
      }
      {
        id ivarsDataset = [[[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                setParent: hdf5Obj]
                               setCreateFlag: NO]
                              setDatasetFlag: YES]
                             setName: "ivars"]
                            createEnd];
        if (ivarsDataset)
          {
            [super hdf5In: ivarsDataset];
            [ivarsDataset drop];
          }
      }
    }
  else if ([hdf5Obj getCompoundType])
    {
      id aZone = [self getZone];
      Class class = [hdf5Obj getClass];
      unsigned i, c_count = [hdf5Obj getCount];
      const char **rowNames = [hdf5Obj readRowNames];
      
      for (i = 0; i < c_count; i++)
        {
          id obj = [class createBegin: aZone];
          unsigned x, y;

          [hdf5Obj selectRecord: i];
          [hdf5Obj shallowLoadObject: obj];
          sscanf (rowNames[i], "%u,%u", &x, &y);
          obj = [obj createEnd];
          [self putObject: obj atX: x Y: y];
        }
      XFREE (rowNames); // but not the contents
    }
  else
    {
      int process_ivar (id component)
        {
          const char *name = [component getName];
          
          if (strcmp (name, "lattice") == 0)
            {
              int process_lattice (id latticeHdf5Obj)
                {
                  unsigned x, y;
                  const char *key = [latticeHdf5Obj getName];
                  
                  sscanf (key, "%u,%u", &x, &y);
                  
                  [self putObject: hdf5In (aZone, latticeHdf5Obj) atX: x Y: y];
                  return 0;
                }
              [component iterate: process_lattice];
            }
          else
            [component assignIvar: self];
          return 0;
        }
      [hdf5Obj iterate: process_ivar];
    }
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
  return *discrete2dSiteAt (lattice, offsets, x, y);
}

- (long)getValueAtX: (unsigned)x Y: (unsigned)y
{
  return (long) *discrete2dSiteAt (lattice, offsets, x, y);
}

- putObject: anObject atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt (lattice, offsets, x, y) = anObject;
  return self;
}

- putValue: (long)v atX: (unsigned)x Y: (unsigned)y
{
  *discrete2dSiteAt (lattice, offsets, x, y) = (id) v;
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
  unsigned i, lcount = xsize * ysize;

  for (i = 0; i < lcount; i++)
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

static void
lispInLatticeValues (Discrete2d *self, id array)
{
  size_t size = [array getElementSize];

  if (size == sizeof (id))
    memcpy ((void *) self->lattice,
            [array getData], 
            [array getElementCount] * size);
  else
    abort ();
}

static void
lispInLatticeObjects (id self, id expr)
{
  id aZone = [self getZone];
  id site = [expr get]; // index points to first stored lattice coord

  do {
    // expect a `pair' - the co-ordinate & object 
    if (pairp (site))
      { 
        unsigned tempX = 0, tempY = 0;            
        id coord = [site getCar];
        id obj = [site getCdr];
        
        if (pairp (coord))
          {
            tempX = [[coord getCar] getInteger];
            tempY = [[coord getCdr] getInteger];
          }
        else
          raiseEvent (InvalidArgument, "Expecting a pair of integers");
        [self putObject: lispIn (aZone, obj) atX: tempX Y: tempY];
      }
    else
      raiseEvent (InvalidArgument, "Expecting either cons pair or an array");
  }
  while ((site = [expr next]));
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
                  id l = [(id) val begin: scratchZone];

                  // get first `parse' string
                  site = [l next];
                  if (stringp (site) &&
                      strcmp ([site getC], PARSE_FUNCTION_NAME) == 0)
                    {
                      // skip to next element after `parse' string
                      site = [l next];
                      
                      if (arrayp (site)) // dealing with a lattice of values
                        lispInLatticeValues (self, site);
                      else    // dealing with a lattice of objects
                        lispInLatticeObjects (self, l);
                    }
                  else
                    raiseEvent (InvalidArgument, "Expecting `%s'",
                                PARSE_FUNCTION_NAME);
                  [l drop];
                }
              else
                raiseEvent (InvalidArgument, "Argument not a list");
            }
        }
    }
  [index drop];
  return self;
}

static void
lispOutLatticeObjects (Discrete2d *self, id stream)
{
  unsigned x, y;
  
  [stream catC: " #:lattice \n(parse\n"];
  for (x = 0; x < self->xsize; x++)
    for (y = 0; y < self->ysize; y++)
      {
        id obj = [self getObjectAtX: x Y: y];

        if (obj != nil)
          {
            char buffer[2 * DSIZE (int) + 22];

            sprintf (buffer, "  (cons '(%u . %u)\n   ", x, y);
            [stream catC: buffer];    
            [obj lispOutDeep: stream];
            [stream catC: ")\n"];    
          }
      }
  [stream catC: ")"];
}

static void
lispOutLatticeValues (Discrete2d *self, id stream)
{
  char buf[2 * DSIZE(unsigned) + 5 + 1];
  
  // generate compiler encoding for 2D array
  sprintf (buf,
           "%c%u%c%u%c%c%c", 
           _C_ARY_B, self->ysize,
           _C_ARY_B, self->xsize,
           _C_LNG, _C_ARY_E, _C_ARY_E);
  
  [stream catC: " #:lattice \n (parse "];
  
  lisp_output_type (buf,
                    (void *) self->lattice,
                    0,
                    NULL,
                    stream,
                    NO);
  [stream catC: ")"];
}

- lispOutShallow: stream
{
  [stream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " '"];
  [stream catC: [self getTypeName]];
  [self lispOutVars: stream deep: NO];
  lispOutLatticeValues (self, stream);
  [stream catC: ")"];
  return self;
}

- lispOutDeep: stream
{
  [stream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " '"];
  [stream catC: [self getTypeName]];
  [self lispOutVars: stream deep: NO]; // The others ivars are scalar
  lispOutLatticeObjects (self, stream);
  [stream catC: ")"];
  return self;
}

- hdf5OutShallow: hdf5Obj
{
  id group = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                  setParent: hdf5Obj]
                 setCreateFlag: YES]
                setName: [hdf5Obj getName]]
               createEnd];

  [group storeTypeName: [self getTypeName]];
  [group setName: "ivars"];
  [super hdf5OutShallow: group];
  {
    char buf[2 * DSIZE(unsigned) + 5 + 1];
    
    // generate compiler encoding for 2D array
    sprintf (buf,
             "%c%u%c%u%c%c%c", 
             _C_ARY_B, ysize, _C_ARY_B, xsize, _C_LNG, _C_ARY_E, _C_ARY_E);
    
    [group storeAsDataset: "lattice"
           typeName: [self name]
           type: buf
           ptr: lattice];
  }
  [group drop];
  return self;
}

- hdf5OutDeep: hdf5Obj
{
  unsigned x, y;
  id hdf5Zone = [hdf5Obj getZone];
  id latticeHdf5Group;
  
  [super hdf5OutDeep: hdf5Obj];

  latticeHdf5Group = [[[[[HDF5 createBegin: hdf5Zone]
                          setParent: hdf5Obj]
                         setCreateFlag: YES]
                        setName: "lattice"]
                       createEnd];
  
  for (x = 0; x < xsize; x++)
    for (y = 0; y < ysize; y++)
      {
        id obj = [self getObjectAtX: x Y: y];

        if (obj != nil)
          {
            char buf[DSIZE(unsigned) + 1 + 1];
            id group;

            sprintf (buf, "%u,%u", x, y);
            group = [[[[[HDF5 createBegin: hdf5Zone]
                         setCreateFlag: YES]
                        setParent: latticeHdf5Group]
                       setName: buf]
                      createEnd];
            [obj hdf5OutDeep: group];
            [group drop];
          }
      }
  [latticeHdf5Group drop];
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
  f = [InFile create: [self getZone] setName: filename];

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
