#import <simtools.h>
#import <defobj.h>
#import <defobj/Create.h>
#include <swarmconfig.h>

#define OBJNAME "myObj"

#define STRVAL "Hello World"
#define INTVAL -100
#define UNSIGNEDVAL 100
#define SHORTVAL -10
#define USHORTVAL 10
#define LONGVAL -10000
#define ULONGVAL 10000
#define FLOATVAL 500.0
#define DOUBLEVAL 500000.0

#define COMPONENT_STRVAL "Foo Bar"

@interface MyObj: CreateDrop
{
  const char *name;
}
+ createBegin: aZone;
- (const char *)getName;
@end

@implementation MyObj
+ createBegin: aZone
{
  MyObj *obj = [super createBegin: aZone];

  obj->name = COMPONENT_STRVAL;
  return obj;
}

- (const char *)getName
{
  return name;
}
@end

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
  BOOL updateFlag;
}
- setDeepFlag: (BOOL)deepFlag;
- setUpdateFlag: (BOOL)updateFlag;
- updateArchiver: archiver;
- (BOOL)checkObject;
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
  
  obj->updateFlag = YES;
  return obj;
}

- setDeepFlag: (BOOL)theDeepFlag
{
  deepFlag = theDeepFlag;
  return self;
}

- setUpdateFlag: (BOOL)theUpdateFlag
{
  updateFlag = theUpdateFlag;
  return self;
}

- updateArchiver: archiver
{
  if (updateFlag)
    {
      if (deepFlag)
        [archiver putDeep: OBJNAME object: self];
      else
        [archiver putShallow: OBJNAME object: self];
    }
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

@interface MyClassDeep: MyClass
{
  id objVal;
}
+ createBegin: aZone;
- (BOOL)checkObject;
@end

@implementation MyClassDeep
+ createBegin: aZone
{
  MyClassDeep *obj = [super createBegin: aZone];

  obj->objVal = [MyObj create: aZone];
  return obj;
}

- (BOOL)checkObject
{
  if (![super checkObject])
    return NO;
  
  return strcmp ([objVal getName], COMPONENT_STRVAL) == 0;
}
@end

#define DIM1COUNT 3
#define DIM2COUNT 2

@interface MyClassDeeper: MyClassDeep
{
  int intAryVal[DIM1COUNT][DIM2COUNT];
  double doubleAryVal[DIM1COUNT][DIM2COUNT];
}
+ createBegin: aZone;
@end

@implementation MyClassDeeper: MyClassDeep
+ createBegin: aZone
{
  unsigned i, j;
  MyClassDeeper *obj = [super createBegin: aZone];
  
  for (i = 0; i < DIM1COUNT; i++)
    for (j = 0; j < DIM2COUNT; j++)
      {
        obj->intAryVal[i][j] = (i + 1) * (j + 1);
        obj->doubleAryVal[i][j] = (i + 1) * (j + 1);
      }
  return obj;
}

- (BOOL)checkObject
{
  unsigned i, j;

  if (![super checkObject])
    return NO;

  for (i = 0; i < DIM1COUNT; i++)
    for (j = 0; j < DIM2COUNT; j++)
      {
        if (intAryVal[i][j] != (i + 1) * (j + 1))
          return NO;
        if (doubleAryVal[i][j] != (i + 1) * (j + 1))
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
                        ? (deepFlag ? "deep.hdf" : "shallow.hdf")
                        : (deepFlag ? "deep.scm" : "shallow.scm"))]
             setHDF5Flag: hdf5Flag]
            setInhibitLoadFlag: inhibitLoadFlag]
           createEnd];
}

static BOOL
checkArchiver (id aZone, BOOL hdf5Flag, BOOL deepFlag, BOOL updateFlag)
{
  id obj;
  BOOL ret;
  id archiver;

  archiver = createArchiver (aZone, hdf5Flag, (updateFlag ? YES : NO), 
                             deepFlag);
  if (deepFlag)
    obj = [[[[MyClassDeeper createBegin: aZone]
              setUpdateFlag: updateFlag]
             setDeepFlag: YES]
            createEnd];
  else
    obj = [[[[MyClass createBegin: aZone]
              setUpdateFlag: updateFlag]
             setDeepFlag: NO]
            createEnd];

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

  if (checkArchiver (globalZone, NO, NO, YES) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp serialization with update failed");
  if (checkArchiver (globalZone, NO, NO, NO) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp serialization with no update failed");
  if (checkArchiver (globalZone, NO, YES, YES) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp serialization with update failed");
  if (checkArchiver (globalZone, NO, YES, NO) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp serialization with no update failed");
#ifdef HAVE_HDF5
  if (checkArchiver (globalZone, YES, NO, YES) == NO)
    raiseEvent (InternalError, "Shallow HDF5 serialization failed");
  if (checkArchiver (globalZone, YES, YES, YES) == NO)
    raiseEvent (InternalError, "Deep HDF5 serialization failed");
#endif
  return 0;
}
