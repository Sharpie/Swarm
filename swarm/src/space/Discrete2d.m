// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <space/Discrete2d.h>
#import <space.h>
#import <simtools.h> // InFile
#import <defobj.h> // ProtocolViolation
#import "../defobj/internal.h" // lisp_process_array
#import <defobj/defalloc.h> // getZone
#include <collections/predicates.h> // keywordp, stringp

#include <misc.h> // memset

// Note - this code assumes that ints can be written in where ids have been
// allocated. It uses casts to do this, and I think is portable to all
// modern architectures.

@implementation Discrete2d

PHASE(Creating)

+ create: (id <Zone>)aZone setSizeX: (unsigned)x Y: (unsigned)y
{
  Discrete2d *obj = [self createBegin: aZone];
  obj->xsize = x;
  obj->ysize = y;
  obj->objectFlag = NO;
  return [obj createEnd];
}

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

  p = [getZone (self) alloc: xsize * ysize * sizeof (id)];
  memset (p, 0, xsize * ysize * sizeof (id));

  return p;
}

// Part of createEnd, really, but separated out for ease of inheritance.
- makeOffsets
{
  unsigned i;
  
  // precalculate offsets based on the y coordinate. This lets
  // us avoid arbitrary multiplication in array lookup.
  offsets = [getZone (self) alloc: ysize * sizeof (*offsets)];

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
      id latticeDataset = [[(id <HDF5>)[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                setParent: hdf5Obj]
                               setWriteFlag: NO]
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
      [[hdf5Obj getZone] free: rowNames]; // but not the contents
    }
  else
    {
      BOOL gotX = NO, gotY = NO;

      int process_object (id component)
        {
          const char *name = [component getHDF5Name];

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

- (void)setObjectFlag: (BOOL)_objectFlag
{
  objectFlag = _objectFlag;
}

- hdf5In: hdf5Obj
{
  id aZone = getZone (self);

  if ([hdf5Obj checkDatasetName: "lattice"])
    {
      {
        id latticeDataset = [[(id <HDF5>)[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                  setParent: hdf5Obj]
                                 setWriteFlag: NO]
                                setDatasetFlag: YES]
                               setName: "lattice"]
                              createEnd];
        [latticeDataset loadDataset: lattice];
        [latticeDataset drop];
      }
      {
        id ivarsDataset = [[(id <HDF5>)[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                setParent: hdf5Obj]
                               setWriteFlag: NO]
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
      id aZone = getZone (self);
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
      [[hdf5Obj getZone] free: rowNames]; // but not the contents
    }
  else
    {
      int process_ivar (id component)
        {
          const char *name = [component getHDF5Name];
          
          if (strcmp (name, "lattice") == 0)
            {
              int process_lattice (id latticeHdf5Obj)
                {
                  unsigned x, y;
                  const char *key = [latticeHdf5Obj getHDF5Name];
                  
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
  fcall_type_t type;

  if (sizeof (long) == sizeof (id))
    type = fcall_type_slong;
  else if (sizeof (int) == sizeof (id))
    type = fcall_type_sint;
  else
    abort ();
   
  [array convertToType: type dest: (void *) self->lattice];
}

static void
lispInLatticeObjects (Discrete2d *self, id expr)
{
  id aZone = getZone (self);
  id site = [expr get];

  do {
    // expect a `pair' - the co-ordinate & object 
    if (pairp (site))
      { 
        unsigned tempX = 0, tempY = 0;            
        id coord = [[site getCar] getQuotedObject];
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
              if (archiver_list_p (val)) 
                {
                  id site;
                  id l = [(id) val begin: scratchZone];

                  // get first `parse' string
                  site = [l next];
                  if (stringp (site)
                      && strcmp ([site getC], PARSE_FUNCTION_NAME) == 0)
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
  
  [stream catSeparator];
  [stream catKeyword: "lattice"];
  [stream catSeparator];
  [stream catStartParse];
  for (x = 0; x < self->xsize; x++)
    for (y = 0; y < self->ysize; y++)
      {
        id obj = [self getObjectAtX: x Y: y];

        if (obj != nil)
          {
            [stream catSeparator];
            [stream catStartCons];
            [stream catSeparator];
            [stream catUnsignedPair: x : y];
            [stream catSeparator];
            [obj lispOutDeep: stream];
            [stream catEndCons];
          }
      }
  [stream catEndParse];
}

static void
lispOutLatticeValues (Discrete2d *self, id stream)
{
  unsigned dims[2];

  [stream catSeparator];
  [stream catKeyword: "lattice"];
  [stream catSeparator];
  [stream catStartParse];
  [stream catSeparator];  

  dims[0] = self->ysize;
  dims[1] = self->xsize;
  lisp_process_array (2, dims, fcall_type_slong, (void *) self->lattice,
                      NULL, stream, NO);
  [stream catEndParse];
}

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [stream catStartMakeInstance: [self getTypeName]];
  [self lispOutVars: stream deep: NO];
  lispOutLatticeValues (self, stream);
  [stream catEndMakeInstance];
}

- (void)lispOutDeep: stream
{
  [stream catStartMakeInstance: [self getTypeName]];
  [self lispOutVars: stream deep: NO]; // The others ivars are scalar
  lispOutLatticeObjects (self, stream);
  [stream catEndMakeInstance];
}

- (void)hdf5OutShallow: hdf5Obj
{

  if (objectFlag)
    {
      unsigned xi, yi;
      id proto = nil;
      unsigned count = 0, pos = 0;
      
      for (yi = 0; yi < ysize; yi++)
	for (xi = 0; xi < xsize; xi++)
	  {
	    id obj = *discrete2dSiteAt (lattice, offsets, xi, yi);
	    
	    if (obj != nil)
	      {
		proto = obj;
		count++;
	      }
	  }
      if (proto)
	{
	  id cType = [[(id <HDF5CompoundType>)[HDF5CompoundType createBegin: [hdf5Obj getZone]]
			setPrototype: proto]
		       createEnd];
	  const char *objName = [hdf5Obj getHDF5Name];
	  
	  id cDataset = [[(id <HDF5>)[[[[(id <HDF5>)[HDF5 createBegin: getZone (self)]
			       setName: objName]
			      setWriteFlag: YES]
			     setParent: hdf5Obj]
			    setCompoundType: cType]
			   setCount: count]
			  createEnd];
	  [cDataset storeTypeName: [self getTypeName]];
	  [cDataset storeComponentTypeName: [proto getTypeName]];
	  for (yi = 0; yi < ysize; yi++)
	    for (xi = 0; xi < xsize; xi++)
	      {
		id obj = *discrete2dSiteAt (lattice, offsets, xi, yi);
		
		if (obj != nil)
		  {
		    char buf[64];
		    
		    sprintf (buf, "%u,%u", yi, xi);
		    [cDataset nameRecord: pos name: buf];
		    [cDataset selectRecord: pos];
		    [obj hdf5OutShallow: cDataset];
		    pos++;
		  }
	      }
	  [cDataset writeRowNames];
	  [cDataset drop];
	  [cType drop];
	}
    }
  else
    {
      id group = [[(id <HDF5>)[[[HDF5 createBegin: [hdf5Obj getZone]]
		      setParent: hdf5Obj]
		     setWriteFlag: YES]
		    setName: [hdf5Obj getHDF5Name]]
		   createEnd];

      [group storeTypeName: [self getTypeName]];
      [(id <HDF5>)group setName: "ivars"];
      [super hdf5OutShallow: group];
      {
	unsigned dims[2];
	
	dims[0] = ysize;
	dims[1] = xsize;
	
	[group storeAsDataset: "lattice"
	       typeName: [self name]
           type: fcall_type_slong
	       rank: 2
	       dims: dims
	       ptr: lattice];
	[group drop];
      }
    }
}

- (void)hdf5OutDeep: (id <OutputStream>)hdf5Obj
{
  unsigned x, y;
  id hdf5Zone = [hdf5Obj getZone];
  id latticeHdf5Group;
  
  [super hdf5OutDeep: hdf5Obj];

  latticeHdf5Group = [[(id <HDF5>)[[[HDF5 createBegin: hdf5Zone]
                          setParent: hdf5Obj]
                         setWriteFlag: YES]
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
            group = [[(id <HDF5>)[[[HDF5 createBegin: hdf5Zone]
                         setWriteFlag: YES]
                        setParent: latticeHdf5Group]
                       setName: buf]
                      createEnd];
            [obj hdf5OutDeep: group];
            [group drop];
          }
      }
  [latticeHdf5Group drop];
}
  
// Read in a file in PGM format and load it into a discrete 2d.
// PGM is a simple image format. It stores grey values for a 2d array.
- (int)setDiscrete2d: (id <Discrete2d>)a toFile: (const char *)filename 
{
  id <InFile> f;
  char c1, c2;
  int maxValue;
  unsigned x, y;
  int fileXSize, fileYSize;
  
  // open the file
  f = [InFile create: getZone (self) setName: filename];

  // The first two characters should be P2, the PGM ASCII header (not P5 - raw)
  [f getChar: &c1];
  [f getChar: &c2];
  if (c1 != 'P' && c2 != '2')
    raiseEvent (WarningMessage,
                "File is not in PGM ascii format. Faking it.\n");

  // Next two entries are the size in pixels
  [f getInt: &fileXSize];
  [f getInt: &fileYSize];
  if (fileXSize != [a getSizeX] || fileYSize != [a getSizeY])
    raiseEvent (WarningMessage,
                "PGM File is not the right size. Faking it.\n");

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
              
              raiseEvent (WarningMessage,
                          "Ran out of data reading PGM file. Aborting.\n");
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
- copyDiscrete2d: (id <Discrete2d>)a toDiscrete2d: (id <Discrete2d>)b
{
  unsigned x, y;

  if ([a getSizeX] != [b getSizeX] || [a getSizeY] != [b getSizeY])
    raiseEvent (InvalidArgument, "Two Discrete2ds aren't the same size.");
  for (x = 0; x < [a getSizeX]; x++)
    for (y = 0; y < [b getSizeY]; y++)
      [b putValue: [a getValueAtX: x Y: y] atX: x Y: y];
  return self;
}
 
- (void)drop
{
  [getZone (self) free: lattice];
  [getZone (self) free: offsets];
  [super drop];
}
@end
