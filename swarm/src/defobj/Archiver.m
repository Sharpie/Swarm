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

#define SWARMARCHIVER_HDF5_SUFFIX ".hdf"
#define SWARMARCHIVER_LISP_SUFFIX ".scm"

#define ARCHIVER_FUNCTION_NAME "archiver"

externvardef Archiver_c *hdf5Archiver;
externvardef Archiver_c *lispArchiver;
externvardef Archiver_c *hdf5AppArchiver;
externvardef Archiver_c *lispAppArchiver;

@interface ArchiverObject: CreateDrop
{
  id expr;
  id object;
}
+ create: aZone withExpr: valexpr;
+ create: aZone withObject: theObj;
- setExpr: valexpr;
- getExpr;
- setObject: theObj;
- getObject;
@end

@implementation ArchiverObject
+ create: aZone withExpr: valexpr
{
  id obj = [self createBegin: aZone];
  [obj setExpr: valexpr];
  [obj setObject: nil];
  return [obj createEnd];
}

+ create: aZone withObject: theObj
{
  id obj = [self createBegin: aZone];
  [obj setExpr: nil];
  [obj setObject: theObj];
  return [obj createEnd];
}

- setExpr: valexpr
{
  expr = valexpr;
  return self;
}
- getExpr
{
  return expr;
}

- setObject: obj
{
  object = obj;
  return self;
}

- getObject
{
  return object;
}
@end

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
      size_t homelen = strlen (home);
      char *buf = xmalloc (homelen + 1 + strlen (swarmArchiver) + 1), *p;

      p = stpcpy (buf, home);
      if (homelen == 0 || home[homelen - 1] != '/')
        p = stpcpy (p, "/");

      p = stpcpy (p, swarmArchiver);
      
      return buf;
    }
  return NULL;
}

static const char *
defaultAppPath (const char *appDataPath, const char *appName,
                const char *suffix)
{
  char *buf = 
    xmalloc (strlen (appDataPath) + strlen (appName) + strlen (suffix) + 1);
  char *p;
  
  p = stpcpy (buf, appDataPath);
  p = stpcpy (p, appName);
  p = stpcpy (p, suffix);
  return buf;
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
        id objectMap;

        objectMap = [app getDeepMap];
        if ([objectMap at: key] == nil)
          [objectMap at: key insert: 
                       [ArchiverObject create: aZone withExpr: valexpr]];
        else
          {
            raiseEvent (WarningMessage, "Duplicate object key `%s'",
                        [key getC]);
            [key drop];
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
  newArchiver->systemArchiverFlag = NO;
  return newArchiver;
}

+ create: aZone setPath: (const char *)thePath setHDF5Flag: (BOOL)theHDF5Flag
{
  Archiver_c *obj = [self createBegin: aZone];
  obj->hdf5Flag = theHDF5Flag;
  obj->path = thePath;
  return [obj createEnd];
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

- setDefaultAppLispPath
{
  path = defaultAppPath ([arguments getAppDataPath],
                         [arguments getAppName],
                         SWARMARCHIVER_LISP_SUFFIX);
  hdf5Flag = NO;
  return self;
}

- setDefaultAppHDF5Path
{
  path = defaultAppPath ([arguments getAppDataPath],
                         [arguments getAppName],
                         SWARMARCHIVER_HDF5_SUFFIX);
  hdf5Flag = YES;
  return self;
}

- setSystemArchiverFlag: (BOOL)theSystemArchiverFlag
{
  systemArchiverFlag = theSystemArchiverFlag;
  return self;
}

- createEnd
{
  id aZone = [self getZone];

  [super createEnd];
  
  currentApplicationKey = [self createAppKey: [arguments getAppName]
                                mode: [arguments getAppModeString]];

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
              
              [self hdf5LoadArchiver: file];
              [file drop];
            }
        }
      else
        {
          FILE *fp = fopen (path, "r");
          
          if (fp != NULL)
            {
              // Create zone for easy destruction of expressions,
              // but don't drop it yet, since we will be doing
              // lazy evaluation on the saved pairs
              id inStream;
              inStreamZone = [Zone create: [self getZone]];
              inStream = 
                [InputStream create: inStreamZone setFileStream: fp];  
              [self lispLoadArchiver: [inStream getExpr]];
              fclose (fp);
            }
        }
    }
  return self;
}

PHASE(Setting)

- hdf5LoadObjectMap: topHDF5Obj key: appKey
{
  id app;
  id aZone = [self getZone];

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
          raiseEvent (WarningMessage,
                      "Duplicate HDF5 object key `%s'",
                      [key getC]);
          [key drop];
          [value drop];
        }
      return 0;
    }
  app = [self ensureApp: appKey];
  [topHDF5Obj iterate: objIterateFunc];
  return self; 
}

- hdf5LoadArchiver: hdf5File
{
  if (systemArchiverFlag)
    {
      int appIterateFunc (id appHDF5Obj)
        {
          int modeIterateFunc (id modeHDF5Obj)
            {
              [self hdf5LoadObjectMap: modeHDF5Obj key: 
                      [self createAppKey: [appHDF5Obj getName]
                            mode: [modeHDF5Obj getName]]];
              return 0;
            }
          [appHDF5Obj iterate: modeIterateFunc];
          return 0;
        }
      [hdf5File iterate: appIterateFunc];
    }
  else
    [self hdf5LoadObjectMap: hdf5File key: currentApplicationKey];
  return self;
}

- lispLoadArchiver: expr
{
  id aZone = [self getZone];

  if (systemArchiverFlag)
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
      
      lispProcessApplicationPairs (aZone,
                                   [archiverCallExprIndex next],
                                   applicationMap);
      [archiverCallExprIndex drop];
    }
  else 
    lispProcessMakeObjcPairs (aZone, expr,
                              [self ensureApp: currentApplicationKey]);
  return self;
}

PHASE(Using)

- createAppKey: (const char *)appName mode: (const char *)modeName
{
  id appKey = [String create: [self getZone] setC: appName];

  [appKey catC: "/"];
  [appKey catC: modeName];
  return appKey;
}

- ensureApp: appKey
{
  id app;
  id aZone = [self getZone];
  
  if ((app = [applicationMap at: appKey]) == nil)
    {
      app = [[[Application createBegin: aZone]
               setName: [appKey getC]]
              createEnd];
      
      [applicationMap at: appKey insert: app];
    }
  return app;
}
    
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
lisp_output_objects (id <Map> objectMap, id outputCharStream,
                     BOOL deepFlag, BOOL systemArchiverFlag)
{

  if ([objectMap getCount] > 0)
    {
      id index = [objectMap begin: scratchZone];
      id key, member;
      
      member = [index next: &key];
      for (;;)
        {
          if (systemArchiverFlag)
            [outputCharStream catC: "      "];
          [outputCharStream catC: "  (cons '"];
          [outputCharStream catC: [key getC]];
          [outputCharStream catC: "\n"];
          
          if (systemArchiverFlag)
            [outputCharStream catC: "      "];
          [outputCharStream catC: "    "];
          
          if (member == nil)
            [outputCharStream catC: "#f"];
          else
            {
              id obj;
              if ((obj = [member getObject]))
                {
                  if ([obj isInstance])
                    {
                      if (deepFlag)
                        [obj lispOutDeep: outputCharStream];
                      else
                        [obj lispOutShallow: outputCharStream];
                    }
                  else
                    {
                      SEL sel = M(lispOutShallow:);
                      IMP func = get_imp (id_CreatedClass_s, sel);
                      
                      func (obj, sel, outputCharStream);
                    }
                }
              else
                {
                  // if we're not storing an object, we must be
                  // serialize the unchanged contents of the parsed
                  // ArchiverList instance
                  id listexpr = [member getExpr];
                  if (listp (listexpr))
                    [listexpr lispOutDeep: outputCharStream];
                  else
                    raiseEvent(InvalidOperation,
                               "parsed ArchiverList instance expected");
                }
            }
          [outputCharStream catC: ")"];
          member = [index next: &key];
          if ([index getLoc] == (id) Member)
            [outputCharStream catC: "\n"];
          else
            break;
        }
    }
}

static void
lisp_output_app_objects (id app, id outputCharStream, BOOL systemArchiverFlag)
{
  [outputCharStream catC: "(list\n"];
  lisp_output_objects ([app getShallowMap], outputCharStream,
                       NO, systemArchiverFlag);
  lisp_output_objects ([app getDeepMap], outputCharStream,
                       YES, systemArchiverFlag);
  [outputCharStream catC: ")"];
}


- _lispOut_: outputCharStream
{
  if (systemArchiverFlag)
    {
      id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
      id app;
      id <String> appKey;
      
      [outputCharStream catC: "(" ARCHIVER_FUNCTION_NAME "\n  (list"];
      
      while ((app = [appMapIndex next: &appKey]))
        {
          [outputCharStream catC: "\n    (cons "];
          lisp_print_appkey ([appKey getC], outputCharStream);
          [outputCharStream catC: "\n      "];
          lisp_output_app_objects (app, outputCharStream, YES);
          [outputCharStream catC: ")"];
        }
      [outputCharStream catC: "))\n"];
      [appMapIndex drop];
    }
  else
    lisp_output_app_objects ([self ensureApp: currentApplicationKey],
                             outputCharStream, NO);
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
  if (![client isInstance])
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
  if (![client isInstance])
    [classes remove: client];
  else
    [instances remove: client];
  return self;
}

static void
archiverPut (id aZone, const char *keyStr, id value, id addMap, id removeMap, 
             BOOL hdf5Flag)
{
  id key = [String create: [addMap getZone] setC: keyStr];
  
  if (hdf5Flag)
    {
      if ([addMap at: key])
        [addMap at: key replace: value];
      else
        [addMap at: key insert: value];
    }
  else
    {
      id item;
      if ((item = [addMap at: key]))
        {
          [item setObject: value];
          [addMap at: key replace: item];        
        }
      else
        {
          item = [ArchiverObject create: aZone withObject: value];
          [addMap at: key insert: item];
        }
    }
  if ([removeMap at: key])
    [removeMap removeKey: key];
}

- putDeep: (const char *)key object: object
{
  id app = [self getApplication];

  archiverPut ([self getZone], key, object, [app getDeepMap], 
               [app getShallowMap], hdf5Flag);
  return self;
}

- putShallow: (const char *)key object: object
{
  id app = [self getApplication];

  archiverPut ([self getZone], key, object, [app getShallowMap], 
               [app getDeepMap], hdf5Flag);
  return self;
}

static id
archiverGet (id aZone, id string, id app, BOOL hdf5Flag)
{
  id result;

  if (hdf5Flag)
    {
      result = [[app getDeepMap] at: string];
      if (result == nil)
        result = [[app getShallowMap] at: string];
    }
  else
    {
      id valexpr = [[[app getDeepMap] at: string] getExpr];
      if (valexpr == nil)
        valexpr = [[[app getShallowMap] at: string] getExpr];
      
      if (valexpr != nil)
        result = lispIn (aZone, valexpr);
      else
        return nil;
    }
  return result;
}

- _getWithZone_: aZone _object_: (const char *)key 
{
  id string = [String create: [self getZone] setC: key];
  id app = [self getApplication];
  id result; 
  
  result = archiverGet (aZone, string, app, hdf5Flag);

  [string drop];
  return result;
}

- getObject: (const char *)key
{
  return ([self _getWithZone_: [self getZone] _object_: key]);
}

- getWithZone: aZone object: (const char *)key
{
  return ([self _getWithZone_: aZone _object_: key]);
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
      if (systemArchiverFlag)
        {
          id appGroup;
          id modeGroup = hdf5_create_app_group ([appKey getC],
                                                hdf5Obj, &appGroup);
          
          hdf5_output_objects ([app getShallowMap], modeGroup, NO);
          hdf5_output_objects ([app getDeepMap], modeGroup, YES);
          
          [modeGroup drop];
          [appGroup drop];
        }
      else
        {
          hdf5_output_objects ([app getShallowMap], hdf5Obj, NO);
          hdf5_output_objects ([app getDeepMap], hdf5Obj, YES);
        }
    }
  [index drop];
  return self;
}

- updateArchiver
{
  id <Index> index;
  id item;
  IMP func = get_imp (id_CreatedClass_s, M(updateArchiver:));
  
  index = [classes begin: [self getZone]];
  while ((item = [index next]))
    func (item, M(updateArchiver:), self);
  [index drop];
  [instances forEach: @selector (updateArchiver:) : self];
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
  [inStreamZone drop];

  [super drop];
}

@end
