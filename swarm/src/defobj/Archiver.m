// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#import <collections.h>
#import <collections/predicates.h> // list_literal_p, listp, pairp, stringp
#import <defobj.h> // arguments
#import <defobj/HDF5Object.h>
#include <misc.h> // access, getenv, xmalloc, stpcpy, strdup

#define SWARMARCHIVER_HDF5 "swarmArchiver.hdf"
#define SWARMARCHIVER_LISP ".swarmArchiver"

#define ARCHIVER_FUNCTION_NAME "archiver"

Archiver_c *hdf5Archiver;
Archiver_c *lispArchiver;

@interface Application: CreateDrop
{
  const char *name;
  id <Map> deepMap;
  id <Map> shallowMap;
}
+ createBegin: aZone;
- setName: (const char *)name;
- getDeepMap;
- getShallowMap;
@end

@implementation Application
+ createBegin: aZone
{
  Application *obj = [super createBegin: aZone];

  obj->deepMap = [Map create: aZone];
  obj->shallowMap = [Map create: aZone];
  obj->name = "EMPTY";

  return obj;
}

- setName: (const char *)theName
{
  name = strdup (theName);
  return self;
}

- getDeepMap
{
  return deepMap;
}

- getShallowMap
{
  return shallowMap;
}

- (void)drop
{
  [shallowMap drop];
  [deepMap drop];
  [super drop];
}

@end

static const char *
defaultPath (const char *swarmArchiver)
{
  const char *home = getenv ("HOME");

  if (home)
    {
      char *buf = xmalloc (strlen (home) + 1 + strlen (swarmArchiver) + 1), *p;

      p = stpcpy (buf, home);
      p = stpcpy (p, "/");
      p = stpcpy (p, swarmArchiver);
      
      return buf;
    }
  return NULL;
}

static void
lispProcessPairs (id aZone, 
                  id obj,
                  void (*mapUpdateFunc) (id, id))
{
  if (!listp (obj))
    raiseEvent (InvalidArgument, "argument to processPairs not a list");
  {
    id listExprIndex = [obj begin: scratchZone];
    id listExpr = [listExprIndex next];
    
    if (!list_literal_p (listExpr))
      raiseEvent (InvalidArgument,
                  "first string in processPairs not \"list\"");
    {
      id consObject;

      while ((consObject = [listExprIndex next]) != nil)
        {
          if (!pairp (consObject))
            raiseEvent (InvalidArgument, "Expecting a pair object");

          {
            id key = [[consObject getCar] copy: aZone];
            
            if (listp (key))
              {
                id first = [key getFirst];
                id last = [key getLast];
                
                if (!stringp (first))
                  raiseEvent (InvalidArgument,
                              "first pair item not a string (%s)",
                              [first name]);
                if (!stringp (last))
                  raiseEvent (InvalidArgument,
                              "second pair item not a string (%s)",
                              [last name]);
                [first catC: "/"];
                [first catC: [last getC]];
                key = [first copy: aZone];
              }
            
            if (!stringp (key))
              raiseEvent (InvalidArgument, "key not a string");
            mapUpdateFunc (key, [consObject getCdr]);
          }
        }
    }
    [listExprIndex drop];
  }
}

static void
lispProcessMakeObjcPairs (id aZone, id expr, id app)
{
  {
    void mapUpdate (id key, id valexpr)
      {
        id value = lispIn (aZone, valexpr);
        id objectMap;
        
        objectMap = [app getDeepMap];
        if ([objectMap at: key] == nil)
          [objectMap at: key insert: value];
        else
          {
            raiseEvent (WarningMessage, "Duplicate object key `%s'",
                        [key getC]);
            [key drop];
            [value drop];
          }
      }
    lispProcessPairs (aZone, expr, mapUpdate);
  }
}

static void
lispProcessApplicationPairs (id aZone, id expr, id applicationMap)
{
  void mapUpdate (id key, id value)
    {
      Application *app = [applicationMap at: key];

      if (app == nil)
        {
          app = [[[Application createBegin: aZone]
                   setName: [key getC]]
                  createEnd];
          [applicationMap at: key insert: app];
        }
      else
        [key drop];
      lispProcessMakeObjcPairs (aZone, value, app);
    }
  lispProcessPairs (aZone, expr, mapUpdate);
}

static void
lispLoadArchiver (id applicationMap, id expr)
{
  id archiverCallExprIndex, archiverCallName;
  
  if (!listp (expr))
    raiseEvent (InvalidArgument, "argument to Archiver lispIn not a list");
  
  archiverCallExprIndex = [expr begin: scratchZone];
  archiverCallName = [archiverCallExprIndex next];

  if (!stringp (archiverCallName))
    raiseEvent (InvalidArgument, "Archiver function not a string");
  
  if (strcmp ([archiverCallName getC], ARCHIVER_FUNCTION_NAME) != 0)
    raiseEvent (InvalidArgument,
                "Archiver function name incorrect: [%s]",
                [archiverCallName getC]);

  lispProcessApplicationPairs ([applicationMap getZone],
                               [archiverCallExprIndex next],
                               applicationMap);
  [archiverCallExprIndex drop];
}

static void
hdf5LoadArchiver (id applicationMap, id hdf5file)
{
  id aZone = [applicationMap getZone];

  int appIterateFunc (id appHDF5Obj)
    {
      int modeIterateFunc (id modeHDF5Obj)
        {
          id app;
          id appKey;

          int objIterateFunc (id hdf5Obj)
            {
              id key = [String create: aZone setC: [hdf5Obj getName]];
              id value = hdf5In (aZone, hdf5Obj);
              id objectMap = ([hdf5Obj getDatasetFlag]
                              ? [app getShallowMap]
                              : [app getDeepMap]);

              if ([objectMap at: key] == nil)
                [objectMap at: key insert: value];
              else
                {
                  raiseEvent (WarningMessage, "Duplicate HDF5 object key `%s'",
                              [key getC]);
                  [key drop];
                  [value drop];
                }
              
              return 0;
            }
          
          appKey = [String create: aZone setC: [appHDF5Obj getName]];
          [appKey catC: "/"];
          [appKey catC: [modeHDF5Obj getName]];

          if ((app = [applicationMap at: appKey]) == nil)
            {
              app = [[[Application createBegin: aZone]
                       setName: [appKey getC]]
                      createEnd];
              
              [applicationMap at: appKey insert: app];
            }
          else
            [appKey drop];
          
          [modeHDF5Obj iterate: objIterateFunc];
          return 0;
        }
      [appHDF5Obj iterate: modeIterateFunc];
      return 0;
    }
  [hdf5file iterate: appIterateFunc];
}

@implementation Archiver_c
PHASE(Creating)

+ createBegin: aZone
{
  Archiver_c *newArchiver = [super createBegin: aZone];

  newArchiver->applicationMap = [Map create: aZone];
  newArchiver->classes = [List create: aZone];
  newArchiver->instances = [List create: aZone];
  newArchiver->path = NULL;
  newArchiver->hdf5Flag = NO;
  newArchiver->inhibitLoadFlag = NO;
  return newArchiver;
}

- setInhibitLoadFlag: (BOOL)theInhibitLoadFlag
{
  inhibitLoadFlag = theInhibitLoadFlag;
  return self;
}

- setPath: (const char *)thePath
{
  path = thePath;
  return self;
}

- setHDF5Flag: (BOOL)theHDF5Flag
{
  hdf5Flag = theHDF5Flag;
  return self;
}

- setDefaultLispPath
{
  path = defaultPath (SWARMARCHIVER_LISP);
  hdf5Flag = NO;
  return self;
}

- setDefaultHDF5Path
{
  path = defaultPath (SWARMARCHIVER_HDF5);
  hdf5Flag = YES;
  return self;
}

- createEnd
{
  const char *appName = [arguments getAppName];
  const char *appModeString = [arguments getAppModeString];
  id aZone = [self getZone];

  [super createEnd];

  currentApplicationKey = [String create: aZone setC: appName];  
  [currentApplicationKey catC: "/"];
  [currentApplicationKey catC: appModeString];

  if (!inhibitLoadFlag)
    {
      if (hdf5Flag)
        {
          if (access (path, R_OK) != -1)
            {
              id file = [[[[[HDF5 createBegin: aZone]
                             setCreateFlag: NO]
                            setParent: nil]
                           setName: path]
                          createEnd];
              
              hdf5LoadArchiver (applicationMap, file);
              [file drop];
            }
        }
      else
        {
          FILE *fp = fopen (path, "r");
          
          if (fp != NULL)
            {
              // Create a temporary zone to simplify destruction of expression
              id inStreamZone = [Zone create: scratchZone];
              id inStream =
                [InputStream create: inStreamZone setFileStream: fp];
              
              lispLoadArchiver (applicationMap, [inStream getExpr]);
              [inStreamZone drop]; 
              fclose (fp);
            }
        }
    }
  return self;
}

PHASE(Using)
     
- getApplication
{
  id app = [applicationMap at: currentApplicationKey];
  
  if (app == nil)
    {
      app = [Application create: [self getZone]];
      [applicationMap at: currentApplicationKey insert: app];
    }
  return app;
}

static void
lisp_print_appkey (const char *appKey, id <OutputStream> outputCharStream)
{
  FILE *fp = [outputCharStream getFileStream];
  
  [outputCharStream catC: "'("];
  while (*appKey && *appKey != '/')
    {
      fputc (*appKey, fp);
      appKey++;
    }
  if (*appKey == '/')
    {
      fputc (' ', fp);
      appKey++;
      while (*appKey)
        {
          fputc (*appKey, fp);
          appKey++;
        }
    }
  [outputCharStream catC: ")"];
}

static void
lisp_output_objects (id <Map> objectMap, id outputCharStream, BOOL deepFlag)
{
  id index = [objectMap begin: scratchZone];
  id key, member;

  for (member = [index next: &key];
       [index getLoc] == (id) Member;
       member = [index next: &key])
    {
      if (member)
        {
          [outputCharStream catC: "\n        (cons '"];
          [outputCharStream catC: [key getC]];
          [outputCharStream catC: "\n          "];
          if (![member isClass])
            {
              if (deepFlag)
                [member lispOutDeep: outputCharStream];
              else
                [member lispOutShallow: outputCharStream];
            }
          else
            {
              SEL sel = M(lispOutShallow:);
              IMP func = get_imp (id_CreatedClass_s, sel);
              
              func (member, sel, outputCharStream);
            }
          [outputCharStream catC: ")"];
        }
    }
}

- _lispOut_: outputCharStream
{
  id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  [outputCharStream catC: "(" ARCHIVER_FUNCTION_NAME "\n  (list"];
  
  while ((app = [appMapIndex next: &appKey]))
    {
      [outputCharStream catC: "\n    (cons "];
      lisp_print_appkey ([appKey getC], outputCharStream);
      [outputCharStream catC: "\n      (list"];
      lisp_output_objects ([app getShallowMap], outputCharStream, NO);
      lisp_output_objects ([app getDeepMap], outputCharStream, YES);
      [outputCharStream catC: "))"];
    }
  [outputCharStream catC: "))\n"];
  [appMapIndex drop];
  return self;
}

static id
hdf5_create_app_group (const char *appKey, id hdf5Obj, id *hdf5AppObjPtr)
{
  id hdf5AppObj = hdf5Obj;
  char *newAppKey, *modeKey;

  newAppKey = strdup (appKey);
  modeKey = newAppKey;
  
  while (*modeKey && *modeKey != '/')
    modeKey++;
  if (*modeKey == '/')
    {
      *modeKey = '\0';
      modeKey++;
      hdf5AppObj = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                        setCreateFlag: YES]
                       setParent: hdf5Obj]
                      setName: newAppKey]
                     createEnd];
      *hdf5AppObjPtr = hdf5AppObj;
    }
  else
    raiseEvent (InvalidArgument, "expecting composite app/mode key");
  return [[[[[HDF5 createBegin: [hdf5AppObj getZone]]
              setParent: hdf5AppObj]
             setName: modeKey]
            setCreateFlag: YES]
           createEnd];
}

static void
hdf5_output_objects (id <Map> objectMap, id hdf5Obj, BOOL deepFlag)
{
  id index = [objectMap begin: scratchZone];
  id key, member;
  
  for (member = [index next: &key];
       [index getLoc] == (id) Member;
       member = [index next: &key])
    {
      if (member)
        {
          if (deepFlag && !stringp (member))
            {
              id memberGroup = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                    setCreateFlag: YES]
                                   setParent: hdf5Obj]
                                  setName: [key getC]]
                                 createEnd];
              
              [member hdf5OutDeep: memberGroup];
              [memberGroup drop];
            }
          else
            {
              id dataset = [[[[[[HDF5 createBegin: [hdf5Obj getZone]]
                             setCreateFlag: YES]
                                setParent: hdf5Obj]
                               setDatasetFlag: YES]
                              setName: [key getC]]
                             createEnd];
              
              [member hdf5OutShallow: dataset];
              [dataset drop];
            }
        }
    }
}

- registerClient: client
{
  if ([client isClass])
    {
      if (![classes contains: client])
        [classes addLast: client];
    }
  else if (![instances contains: client])
    [instances addLast: client];
  return self;
}

- unregisterClient: client
{
  if ([client isClass])
    [classes remove: client];
  else
    [instances remove: client];
  return self;
}

static void
archiverPut (const char *keyStr, id value, id addMap, id removeMap)
{
  id key = [String create: [addMap getZone] setC: keyStr];
  
  if ([addMap at: key])
    [addMap at: key replace: value];
  else
    [addMap at: key insert: value];
  if ([removeMap at: key])
    [removeMap removeKey: key];
}

- putDeep: (const char *)key object: object
{
  id app = [self getApplication];

  archiverPut (key, object, [app getDeepMap], [app getShallowMap]);
  return self;
}

- putShallow: (const char *)key object: object
{
  id app = [self getApplication];

  archiverPut (key, object, [app getShallowMap], [app getDeepMap]);
  return self;
}

- getObject: (const char *)key
{
  id string = [String create: [self getZone] setC: key];
  id app = [self getApplication];
  id result;
  
  result = [[app getDeepMap] at: string];
  if (result == nil)
    result = [[app getShallowMap] at: string];
  
  [string drop];
  return result;
}

- (unsigned)countObjects: (BOOL)deepFlag
{
  id <MapIndex> index = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  unsigned count = 0;
  
  while ((app = [index next: &appKey]))
    count += [(deepFlag ? [app getDeepMap] : [app getShallowMap])
               getCount];
  [index drop];
  return count;
}

- _hdf5Out_: hdf5Obj
{
  id <MapIndex> index = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  while ((app = [index next: &appKey]))
    {
      id appGroup;
      id modeGroup = hdf5_create_app_group ([appKey getC], hdf5Obj, &appGroup);
      
      hdf5_output_objects ([app getShallowMap], modeGroup, NO);
      hdf5_output_objects ([app getDeepMap], modeGroup, YES);
      
      [modeGroup drop];
      [appGroup drop];
    }
  [index drop];
  return self;
}

- updateArchiver
{
  id <Index> index;
  id item;
  IMP func = get_imp (id_CreatedClass_s, M(updateArchiver));
  
  index = [classes begin: [self getZone]];
  while ((item = [index next]))
    func (item, M(updateArchiver));
  [index drop];
  [instances forEach: @selector (updateArchiver)];
  return self;
}

- save
{
  [self updateArchiver];
  if (hdf5Flag)
    {
      if ([self countObjects: YES] + [self countObjects: NO] > 0)
        {
          id hdf5Obj = [[[[[HDF5 createBegin: [self getZone]]
                            setCreateFlag: YES]
                           setParent: nil]
                          setName: path]
                         createEnd];
          
          [self _hdf5Out_: hdf5Obj];
          [hdf5Obj drop];
        }
    }
  else
    {
      FILE *fp = fopen (path, "w");
      id outStream;
      
      if (fp == NULL)
        raiseEvent (SaveError, "Cannot open lisp archive %s", path);
      outStream = [OutputStream create: scratchZone setFileStream: fp];
      [self _lispOut_: outStream];
      fclose (fp);
      [outStream drop];
    }
  return self;
}

- (void)drop
{
  [applicationMap deleteAll];
  [applicationMap drop];
  [classes drop];
  [instances drop];
  [super drop];
}

@end
