#import <simtools.h>
#import <defobj.h>
#import <defobj/Create.h>
#include <misc.h>
#include <argp.h>

#define OBJNAME "myObj"
id archiver;

BOOL deepFlag;

#define USEHDF5 128
#define USEDEEP 129

#define STRVAL "Hello World"
#define INTVAL -100
#define UNSIGNEDVAL 100
#define SHORTVAL -10
#define USHORTVAL 10
#define LONGVAL -10000
#define ULONGVAL 10000
#define FLOATVAL 500.0
#define DOUBLEVAL 500000.0

@interface MyClass: CreateDrop
{
  const char *strVal;
  int intVal;
  unsigned unsignedVal;
  short shortVal;
  unsigned short ushortVal;
  long longVal;
  unsigned long ulongVal;
  float floatVal;
  double doubleVal;
}
- updateArchiver;
@end

@implementation MyClass
+ createBegin: aZone
{
  MyClass *obj = [super createBegin: aZone];

  obj->strVal = STRVAL;
  obj->intVal = INTVAL;
  obj->unsignedVal = UNSIGNEDVAL;
  obj->shortVal = SHORTVAL;
  obj->ushortVal = USHORTVAL;
  obj->longVal = LONGVAL;
  obj->ulongVal = ULONGVAL;
  obj->floatVal = FLOATVAL;
  obj->doubleVal = DOUBLEVAL;
  return obj;
}

- updateArchiver
{
  if (deepFlag)
    [archiver putDeep: OBJNAME object: self];
  else
    [archiver putShallow: OBJNAME object: self];
  return self;
}

- (BOOL)checkObject
{
  if (strcmp (strVal, STRVAL) != 0)
    return NO;
  if (intVal != INTVAL)
    return NO;
  if (unsignedVal != UNSIGNEDVAL)
    return NO;
  if (shortVal != SHORTVAL)
    return NO;
  if (ushortVal != USHORTVAL)
    return NO;
  if (longVal != LONGVAL)
    return NO;
  if (ulongVal != ULONGVAL)
    return NO;
  if (floatVal != FLOATVAL)
    return NO;
  if (doubleVal != DOUBLEVAL)
    return NO;
  return YES;
}
@end

static id
createArchiver (BOOL hdf5Flag, BOOL inhibitLoadFlag)
{
  return [[[[[Archiver createBegin: globalZone]
              setPath: hdf5Flag ? "archive.hdf" : "archive.scm"]
             setHDF5Flag: hdf5Flag]
            setInhibitLoadFlag: inhibitLoadFlag]
           createEnd];
}

int
main (int argc, const char **argv)
{
  id obj;
  BOOL hdf5Flag = NO;
  int ret;

  struct argp_option options[] = {
    {"hdf5", USEHDF5, 0, 0, "Use HDF5 for archiving", 5 },
    {"deep", USEDEEP, 0, 0, "Use deep serialization", 6 },
    { 0 }
  };

  int parse (int key, const char *arg)
    {
      if (key == USEHDF5)
        {
          hdf5Flag = YES;
          return 0;
        }
      else if (key == USEDEEP)
        {
          deepFlag = YES;
          return 0;
        }
      return ARGP_ERR_UNKNOWN;
    }
  
  deepFlag = NO;

  initSwarmAppOptionsBatch (argc, argv, "1.4.1", "bug-swarm@santafe.edu",
                            options, parse);

  archiver = createArchiver (hdf5Flag, YES);
  obj = [MyClass create: globalZone];
  [archiver registerClient: obj];
  [archiver save];
  [obj drop];
  [archiver drop];

  archiver = createArchiver (hdf5Flag, NO);
  obj = [archiver getObject: OBJNAME];
  [archiver drop];

  ret = [obj checkObject] == YES ? 0 : 1;
  [obj drop];
  
  return ret;
}
