#import <simtools.h>
#import <defobj.h>
#import <defobj/Create.h>
#import <defobj/Zone.h> // getZone
#include <swarmconfig.h>
#include <misc.h> // xmalloc, sprintf, sscanf

#define OBJNAME "myObj"
#define COLLNAME "collection"

#define STRVAL "str"
#define INTVAL -100
#define UNSIGNEDVAL 100
#define SHORTVAL -10
#define USHORTVAL 10
#define LONGVAL -10000
#define ULONGVAL 10000
#define FLOATVAL 500.0
#define DOUBLEVAL 500000.0

#define COUNT 10

#define COMPONENT_STRVAL "Foo Bar"

@interface Key: CreateDrop
{
  int offset;
  const char *key;
}
- setOffset: (int)offset;
- (int)getOffset;
- createEnd;
@end

@implementation Key
- setOffset: (int)theOffset
{
  offset = theOffset;
  return self;
}

- createEnd
{
  char buf[3 + DSIZE(int) + 1];
  
  sprintf (buf, "key%d", offset);
  key = strdup (buf);
  return self;
}

- (int)getOffset
{
  return offset;
}
@end

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

  unsigned offset;
}
- setDeepFlag: (BOOL)deepFlag;
- setUpdateFlag: (BOOL)updateFlag;
- setOffset: (int)offset;
- adjustToOffset;
- updateArchiver: archiver;
- (int)getOffset;
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

- setOffset: (int)theOffset
{
  offset = theOffset;
  return self;
}

- adjustToOffset
{
  {
    char *newStr = xmalloc (strlen (strVal) + DSIZE (unsigned) + 1);

    sprintf (newStr, "%s %d", strVal, offset);
    strVal = newStr;
  }
  intVal += offset;
  unsignedVal += offset;
  shortVal += offset;
  ushortVal += offset;
  longVal += offset;
  ulongVal += offset;
  floatVal += offset;
  doubleVal += offset;
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

- (int)getOffset
{
  return offset;
}

- (BOOL)checkObject
{
  {
    int strNum;
    char buf[strlen (strVal) + 1];
    sscanf (strVal, "%s %d", buf, &strNum);

    if (strcmp (buf, STRVAL) != 0)
      return NO;
    if (strNum != offset)
      return NO;
  }
  if (intVal - offset != INTVAL)
    return NO;
  if (unsignedVal - offset != UNSIGNEDVAL)
    return NO;
  if (shortVal - offset != SHORTVAL)
    return NO;
  if (ushortVal - offset != USHORTVAL)
    return NO;
  if (longVal - offset != LONGVAL)
    return NO;
  if (ulongVal - offset != ULONGVAL)
    return NO;
  if (floatVal - offset != FLOATVAL)
    return NO;
  if (doubleVal - offset != DOUBLEVAL)
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

@interface Controller: CreateDrop
{
  BOOL hdf5Flag;
  BOOL inhibitLoadFlag;
  BOOL deepFlag;
  BOOL mapFlag;
  BOOL updateFlag;
  id myArchiver;
  id coll;
}
- createEnd;
- updateArchiver: archiver;
- setHDF5Flag: (BOOL)hdf5Flag;
- setDeepFlag: (BOOL)deepFlag;
- setUpdateFlag: (BOOL)updateFlag;
- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- setMapFlag: (BOOL)mapFlag;
- (BOOL)loadAndCheckCollection;
- createCollectionAndSave;
- updateArchiver: archiver;
@end

@implementation Controller
- setHDF5Flag: (BOOL)theHDF5Flag
{
  hdf5Flag = theHDF5Flag;
  return self;
}

- setInhibitLoadFlag: (BOOL)theInhibitLoadFlag
{
  inhibitLoadFlag = theInhibitLoadFlag;
  return self;
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

- setMapFlag: (BOOL)theMapFlag
{
  mapFlag = theMapFlag;
  return self;
}

- createEnd
{
  id aZone = getZone (self);
  myArchiver = [[[[[Archiver createBegin: aZone]
                    setPath:
                      (mapFlag
                       ? (hdf5Flag
                          ? (deepFlag ? "deepMap.hdf" : "shallowMap.hdf")
                          : (deepFlag ? "deepMap.scm" : "shallowMap.scm"))
                       : (hdf5Flag
                          ? (deepFlag ? "deepList.hdf" : "shallowList.hdf")
                          : (deepFlag ? "deepList.scm" : "shallowList.scm")))]
                   setHDF5Flag: hdf5Flag]
                  setInhibitLoadFlag: inhibitLoadFlag]
                 createEnd];
  if (mapFlag)
    {
      if (deepFlag)
        coll = [Map create: aZone];
      else
        coll = [[[Map createBegin: aZone]
                  setCompareFunction: compareCStrings]
                 createEnd];
    }
  else
    coll = [List create: aZone];
  [myArchiver registerClient: self];
  return self;
}

- updateArchiver: archiver
{
  if (updateFlag)
    {
      if (deepFlag)
        [archiver putDeep: COLLNAME object: coll];
      else
        [archiver putShallow: COLLNAME object: coll];
    }
  
  return self;
}

- createCollectionAndSave
{
  unsigned i;
  id aZone = getZone (self);

  for (i = 0; i < COUNT; i++)
    {
      id obj;
      
      if (deepFlag)
        obj = [[[[[MyClassDeep createBegin: aZone]
                  setDeepFlag: YES]
                  setUpdateFlag: updateFlag]
                 setOffset: i]
                createEnd];
      else
        obj = [[[[[MyClass createBegin: aZone]
                   setDeepFlag: NO]
                  setUpdateFlag: updateFlag]
                 setOffset: i]
                createEnd];
      [obj adjustToOffset];
      if (mapFlag)
        {
          if (deepFlag)
            {
              id key = [[[Key createBegin: aZone] setOffset: i] createEnd];

              [coll at: key insert: obj];
            }
          else
            {
              char buf[3 + DSIZE(unsigned) + 1];

              sprintf (buf, "key%d", i);
              [coll at: (id) strdup (buf) insert: obj];
            }
        }
      else
        [coll addLast: obj];
    }
  [myArchiver save];
  return self;
}

- (BOOL)loadAndCheckCollection
{
  id aZone = getZone (self);
  id obj;
  BOOL ret = YES;

  coll = [myArchiver getObject: COLLNAME];

  if ([coll getCount] != COUNT)
    {
      raiseEvent (WarningMessage,
                  "collection not right size: %u vs %u\n",
                  [coll getCount], COUNT);
      return NO;
    }

  if (mapFlag)
    {
      if (deepFlag)
        {
          id key;
          id <MapIndex> mi = [coll begin: aZone];

          while ((obj = [mi next: &key]))
            {
              if (![obj checkObject])
                {
                  ret = NO;
                  break;
                }
              if ([key getOffset] != [obj getOffset])
                {
                  ret = NO;
                  break;
                }
            }
          [mi drop];
        }
      else
        {
          const char *key;
          id <MapIndex> mi = [coll begin: aZone];

          while ((obj = [mi next: (id *) &key]))
            {
              if (![obj checkObject])
                {
                  ret = NO;
                  break;
                }
              {
                char buf[3 + DSIZE(int) + 1];

                sprintf (buf, "key%d", [obj getOffset]);

                if (strcmp (key, buf) != 0)
                  {
                    ret = NO;
                    break;
                  }
              }
            }
          [mi drop];
        }
    }
  else
    {
      id <Index> li = [coll begin: aZone];

      while ((obj = [li next]))
        {
          if (![obj checkObject])
            {
              ret = NO;
              break;
            }
        }
      [li drop];
    }
  return ret;
}

- (void)drop
{
  [myArchiver unregisterClient: self];
  [coll deleteAll];
  [coll drop];
  [myArchiver drop];
  [super drop];
}

@end

 
static BOOL
checkArchiver (id aZone, BOOL hdf5Flag, BOOL deepFlag, BOOL mapFlag, 
               BOOL updateFlag)
{
  id controller;
  BOOL ret;
  
  controller = [[[[[[[Controller createBegin: aZone]
                      setHDF5Flag: hdf5Flag]
                     setMapFlag: mapFlag]
                    setDeepFlag: deepFlag]
                   setUpdateFlag: updateFlag]
                  setInhibitLoadFlag: (updateFlag ? YES: NO)]
                 createEnd];
  
  [controller createCollectionAndSave];
  [controller drop];

  controller = [[[[[[[Controller createBegin: aZone]
                      setHDF5Flag: hdf5Flag]
                     setMapFlag: mapFlag]
                    setDeepFlag: deepFlag]
                   setUpdateFlag: updateFlag]
                  setInhibitLoadFlag: NO]
                 createEnd];
  ret = [controller loadAndCheckCollection];
  [controller drop];
  
  return ret;
}

int
main (int argc, const char **argv)
{
  initSwarmBatch (argc, argv);

  if (checkArchiver (globalZone, NO, NO, NO, YES) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp List serialization with update failed");
  if (checkArchiver (globalZone, NO, NO, NO, NO) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp List serialization without update failed");

  if (checkArchiver (globalZone, NO, YES, NO, YES) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp List serialization with update failed");
  if (checkArchiver (globalZone, NO, YES, NO, NO) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp List serialization without update failed");

  if (checkArchiver (globalZone, NO, NO, YES, YES) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp Map serialization with update failed");
  if (checkArchiver (globalZone, NO, NO, YES, NO) == NO)
    raiseEvent (InternalError, 
                "Shallow Lisp Map serialization without update failed");

  if (checkArchiver (globalZone, NO, YES, YES, YES) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp Map serialization with update failed");
  if (checkArchiver (globalZone, NO, YES, YES, NO) == NO)
    raiseEvent (InternalError, 
                "Deep Lisp Map serialization without update failed");
#ifdef HAVE_HDF5
  if (checkArchiver (globalZone, YES, NO, NO, YES) == NO)
    raiseEvent (InternalError, "Shallow HDF5 List serialization failed");
  if (checkArchiver (globalZone, YES, YES, NO, YES) == NO)
    raiseEvent (InternalError, "Deep HDF5 List serialization failed");
  if (checkArchiver (globalZone, YES, NO, YES, YES) == NO)
    raiseEvent (InternalError, "Shallow HDF5 Map serialization failed");
  if (checkArchiver (globalZone, YES, YES, YES, YES) == NO)
    raiseEvent (InternalError, "Deep HDF5 Map serialization failed");
#endif

  return 0;
}
