#import <simtools.h>
#import <defobj.h>
#import <defobj/Create.h>
#include <misc.h>
#include <argp.h>

#define OBJNAME "myObj"
id archiver;

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
  BOOL deepFlag;
}
- setDeepFlag: (BOOL)deepFlag;
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

- setDeepFlag: (BOOL)theDeepFlag
{
  deepFlag = theDeepFlag;
  return self;
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
createArchiver (id aZone, BOOL hdf5Flag, BOOL inhibitLoadFlag)
{
  return [[[[[Archiver createBegin: aZone]
              setPath: hdf5Flag ? "archive.hdf" : "archive.scm"]
             setHDF5Flag: hdf5Flag]
            setInhibitLoadFlag: inhibitLoadFlag]
           createEnd];
}

static BOOL
checkArchiver (id aZone, BOOL hdf5Flag, BOOL deepFlag)
{
  id obj;
  BOOL ret;

  archiver = createArchiver (aZone, hdf5Flag, YES);
  obj = [[[MyClass createBegin: aZone]
           setDeepFlag: deepFlag]
          createEnd];
  [archiver registerClient: obj];
  [archiver save];
  [obj drop];
  [archiver drop];
  
  archiver = createArchiver (aZone, hdf5Flag, NO);
  obj = [archiver getObject: OBJNAME];
  [archiver drop];
  
  ret= [obj checkObject];
  [obj drop];
  
  return ret;
}

int
main (int argc, const char **argv)
{
  initSwarmBatch (argc, argv);

  if (checkArchiver (globalZone, NO, NO) == NO)
    raiseEvent (InternalError, "Shallow Lisp serialization failed");
  if (checkArchiver (globalZone, NO, YES) == NO)
    raiseEvent (InternalError, "Deep Lisp serialization failed");
  if (checkArchiver (globalZone, YES, NO) == NO)
    raiseEvent (InternalError, "Shallow HDF5 serialization failed");
  if (checkArchiver (globalZone, YES, YES) == NO)
    raiseEvent (InternalError, "Deep HDF5 serialization failed");
  return 0;
}
