#import <simtools.h>
#import <space.h>
#import <space/Discrete2d.h>
#include <swarmconfig.h>

#define OBJNAME "myDiscrete2d"

#define STRVAL "Hello World"
#define OTHERSTRVAL "Other World"
#define ULONGVAL 1000
#define OTHERULONGVAL 10
#define XSIZE 4
#define YSIZE 3
#define OTHERX 2
#define OTHERY 2

@interface MyClass: CreateDrop
{
  const char *strVal;
}
- (BOOL)checkObject;
@end

@interface MyClassOther: MyClass
{
}
@end

@implementation MyClass
+ createBegin: aZone
{
  MyClass *obj = [super createBegin: aZone];
  obj->strVal = STRVAL;
  return obj;
}
- (BOOL)checkObject
{
  if (strcmp (strVal, STRVAL) != 0)
    return NO;
  return YES;
}
@end

@implementation MyClassOther
+ createBegin: aZone
{
  MyClassOther *obj = [super createBegin: aZone];
  obj->strVal = OTHERSTRVAL;
  return obj;
}
- (BOOL)checkObject
{
  if (strcmp (strVal, OTHERSTRVAL) != 0)
    return NO;
  return YES;
}
@end

@interface MyDiscrete2d: Discrete2d
{
  BOOL objectFlag;
}
- setObjectFlag: (BOOL)objectFlag;
- updateArchiver: archiver;
- (BOOL)checkObject;
@end

@implementation MyDiscrete2d
+ createBegin: aZone
{
  MyDiscrete2d *obj = [super createBegin: aZone];
  return obj;
}

- setObjectFlag: (BOOL)theObjectFlag;
{
  objectFlag = theObjectFlag;
  return self;
}

- updateArchiver: archiver
{
  if (objectFlag)
    [archiver putDeep: OBJNAME object: self];
  else
    [archiver putShallow: OBJNAME object: self];
  return self;
}

- (BOOL)checkObject
{
  if (!objectFlag)
    {
      unsigned i, lcount;
      
      lcount = xsize * ysize;
      for (i = 0; i < lcount; i++)
        {
          if (i == (OTHERY * xsize + OTHERX))
            {
              if (lattice[i] != (id) OTHERULONGVAL)
                return NO;
            }
          else if (lattice[i] != (id) ULONGVAL)
            return NO;
        }
    }
  else
    {
      unsigned x, y;
      
      for (x = 0; x < xsize; x++) 
        for (y = 0; y < ysize; y++)
          if ([*discrete2dSiteAt(lattice, offsets, x, y) checkObject] == NO)
            return NO;
    }
  return YES;
}
@end


static id
createArchiver (id aZone, BOOL hdf5Flag, BOOL inhibitLoadFlag, BOOL deepFlag)
{
  return [[[[[Archiver createBegin: aZone]
              setPath: (hdf5Flag
                        ? (deepFlag ? "objects.hdf" : "values.hdf")
                        : (deepFlag ? "objects.scm" : "values.scm"))]
             setHDF5Flag: hdf5Flag]
            setInhibitLoadFlag: inhibitLoadFlag]
           createEnd];
}

static BOOL
checkArchiverDiscrete2d (id aZone, BOOL hdf5Flag, BOOL deepFlag)
{
  id obj;
  BOOL ret;
  id archiver;

  archiver = createArchiver (aZone, hdf5Flag, YES, deepFlag);

  if (!deepFlag)
    {
      obj = [[[[MyDiscrete2d createBegin: aZone]
                setSizeX: XSIZE Y: YSIZE]
               setObjectFlag: NO]
              createEnd];
      [obj fastFillWithValue: ULONGVAL];
      
      // make one cell different from the others
      [obj putValue: OTHERULONGVAL atX: OTHERX Y: OTHERY];
    }
  else
    {
      id latticeObj = [MyClass create: aZone];
      obj = [[[[MyDiscrete2d createBegin: aZone]
                setSizeX: XSIZE Y: YSIZE]
               setObjectFlag: YES]
              createEnd];
      [obj fastFillWithObject: latticeObj];
      
      // make one cell different from the others
      [obj putObject: [MyClassOther create: aZone] atX: OTHERX Y: OTHERY];
    }

  [archiver registerClient: obj];
  [archiver save];
  [obj drop];
  [archiver drop];
      
  archiver = createArchiver (aZone, hdf5Flag, NO, deepFlag);
  obj = [archiver getObject: OBJNAME];
  [archiver drop];

  ret = [obj checkObject];
  [obj drop];
  
  return ret;
}

int
main (int argc, const char **argv)
{
  initSwarmBatch (argc, argv);

  if (checkArchiverDiscrete2d (globalZone, NO, YES) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp serialization of Discrete2d with objects failed");

  if (checkArchiverDiscrete2d (globalZone, NO, NO) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp serialization of Discrete2d with values failed");

#ifdef HAVE_HDF5
  if (checkArchiverDiscrete2d (globalZone, YES, YES) == NO)
    raiseEvent (InternalError, 
                "Deep HDF5 serialization of Discrete2d with objects failed");

  if (checkArchiverDiscrete2d (globalZone, YES, NO) == NO)
    raiseEvent (InternalError, 
                "Shallow HDF5 serialization of Discrete2d with values failed");
#endif

  return 0;
}
