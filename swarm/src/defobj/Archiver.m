// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#import <collections.h>
#import <collections/predicates.h> // list_literal_p, listp, pairp, stringp
#import <defobj.h> // arguments

#include <misc.h> // access, getenv, xmalloc, stpcpy, strdup

#ifdef HAVE_HDF5
#define id hdf5id
#include <hdf5.h>
#undef id
#endif

#define ARCHIVER_FUNCTION_NAME "archiver"

#define SWARMARCHIVER ".swarmArchiver"

Archiver_c *archiver;

static int
compareStrings (id val1, id val2)
{
  return [val1 compare: val2];
}

@interface Application: CreateDrop
{
  const char *name;
  id <Map> lispMap;
#ifdef HAVE_HDF5
  id <Map> HDF5Map;
#endif
}
+ createBegin: aZone;
- setName: (const char *)name;
- getLispMap;
#ifdef HAVE_HDF5
- getHDF5Map;
#endif
@end

@implementation Application
+ createBegin: aZone
{
  Application *obj = [super createBegin: aZone];

  obj->lispMap =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
#ifdef HAVE_HDF5
  obj->HDF5Map =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
#endif
  obj->name = "EMPTY";

  return obj;
}

- setName: (const char *)theName
{
  name = strdup (theName);
  return self;
}

- getLispMap
{
  return lispMap;
}

#ifdef HAVE_HDF5
- getHDF5Map
{
  return HDF5Map;
}
#endif

- (void)drop
{
  [lispMap deleteAll];
#ifdef HAVE_HDF5
  [HDF5Map deleteAll];
#endif
  [super drop];
}

@end

#ifdef HAVE_HDF5
@interface _HDF5: CreateDrop
{
  id parent;
}
- setParent: parent;
- createEnd;
@end

@implementation _HDF5
PHASE(Creating)
- setParent: theParent
{
  parent = theParent;
  return self;
}

- createEnd
{
  return self;
}
PHASE(Using)
@end
#endif

static const char *
lispDefaultPath (void)
{
  const char *home = getenv ("HOME");

  if (home)
    {
      char *buf = xmalloc (strlen (home) + 1 + strlen (SWARMARCHIVER) + 1), *p;

      p = stpcpy (buf, home);
      p = stpcpy (p, "/");
      p = stpcpy (p, SWARMARCHIVER);
      
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
lispProcessMakeObjcPairs (id aZone, id expr, id objectMap)
{
  {
    void mapUpdate (id key, id valexpr)
      {
        id value = lispIn (aZone, valexpr);

        if ([objectMap at: key])
          [objectMap at: key replace: value];
        else
          [objectMap at: key insert: value];
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
      lispProcessMakeObjcPairs (aZone, value, [app getLispMap]);
    }
  lispProcessPairs (aZone, expr, mapUpdate);
}

static void
lispLoadArchiverExpr (id applicationMap, id expr)
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

void
archiverRegister (id client)
{

  if ([client isClass])
    {
      if (![archiver->classes contains: client])
        [archiver->classes addLast: client];
    }
  else if (![archiver->instances contains: client])
    [archiver->instances addLast: client];
}

void
archiverUnregister (id client)
{
  if ([client isClass])
    [archiver->classes remove: client];
  else
    [archiver->instances remove: client];
}

void
lispArchiverPut (const char *key, id object)
{
  id map = [[archiver getApplication] getLispMap];
  id keyObj = [String create: [archiver getZone] setC: key];
  
  if ([map at: keyObj])
    [map at: keyObj replace: object];
  else
    [map at: keyObj insert: object];
}

id
lispArchiverGet (const char *key)
{
  id string = [String create: [archiver getZone] setC: key];
  id result = [[[archiver getApplication] getLispMap] at: string];
  
  [string drop];
  return result;
}

void
archiverSave (void)
{
  [archiver save];
}

@implementation Archiver_c
PHASE(Creating)

+ createBegin: aZone
{
  Archiver_c *newArchiver = [super createBegin: aZone];

  newArchiver->applicationMap = 
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
  newArchiver->classes = [List create: aZone];
  newArchiver->instances = [List create: aZone];
  newArchiver->lispPath = lispDefaultPath ();
  return newArchiver;
}

- createEnd
{
  const char *appName = [arguments getAppName];
  const char *appModeString = [arguments getAppModeString];

  [super createEnd];
  currentApplicationKey = 
    [String create: [self getZone] setC: appName];
  
  [currentApplicationKey catC: "/"];
  [currentApplicationKey catC: appModeString];
  
  if (lispPath)
    {
      FILE *fp = fopen (lispPath, "r");
      
      if (fp != NULL)
        {
          // Create a temporary zone to simplify destruction of expression
          id inStreamZone = [Zone create: scratchZone];
          id inStream = [InputStream create: inStreamZone setFileStream: fp];
          
	  lispLoadArchiverExpr (applicationMap,
				[inStream getExpr]);
          [inStreamZone drop]; 
          fclose (fp);
        }
    }
#ifdef HAVE_HDF5
  if (HDF5Path)
    {
      // printf ("post:[%s]\n", [self name]);
    }
#endif
  return self;
}

PHASE(Setting)

- setLispPath: (const char *)thePath
{
  lispPath = strdup (thePath);

  return self;
}

#ifdef HAVE_HDF5
- setHDF5Path: (const char *)thePath
{
  HDF5Path = strdup (thePath);
  
  return self;
}
#endif

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

- lispOut: outputCharStream
{
  id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  [outputCharStream catC: "(" ARCHIVER_FUNCTION_NAME "\n  (list"];
  
  while ((app = [appMapIndex next: &appKey]))
    {
      id objectMap = [app getLispMap];
      id objectMapIndex = [objectMap begin: scratchZone];
      id key, member;
      
      [outputCharStream catC: "\n    (cons '("];
      {
        const char *str = [appKey getC];
        FILE *fp = [outputCharStream getFileStream];
        
        while (*str && *str != '/')
          {
            fputc (*str, fp);
            str++;
          }
        if (*str == '/')
          {
            fputc (' ', fp);
            str++;
            while (*str)
              {
                fputc (*str, fp);
                str++;
              }
          }
      }
      [outputCharStream catC: ")"];
      [outputCharStream catC: "\n      (list"];
      while ((member = [objectMapIndex next: &key]))
        {
          [outputCharStream catC: "\n        (cons '"];
          [outputCharStream catC: [key getC]];
          [outputCharStream catC: "\n          "];
          if (![member isClass])
            [member lispOut: outputCharStream];
          else
            {
              IMP func = get_imp (id_CreatedClass_s, M(lispOut:));
              
              func (member, M(lispOut:), outputCharStream);
            }
          [outputCharStream catC: ")"];
        }
      [outputCharStream catC: "))"];
    }
  [outputCharStream catC: "))\n"];
  [appMapIndex drop];
  return self;
}

- save
{
  if (lispPath)
    {
      FILE *fp = fopen (lispPath, "w");
      id outStream;
      
      if (fp == NULL)
        raiseEvent (InvalidArgument, "Cannot open lisp archive %s", lispPath);
      outStream = [OutputStream create: scratchZone setFileStream: fp];
      {
        id <Index> index;
        id item;
        IMP func = get_imp (id_CreatedClass_s, M(updateArchiver));

        index = [classes begin: [self getZone]];
        while ((item = [index next]))
          func (item, M(updateArchiver));
        [index drop];
      }
      [instances forEach: @selector (updateArchiver)];
      [self lispOut: outStream];
      fclose (fp);
      [outStream drop];
    }
  return self;
}

- (void)drop
{
  [applicationMap deleteAll];
  [applicationMap drop];
  if (lispPath)
    XFREE (lispPath);
  [classes drop];
  [instances drop];
  [super drop];
}

@end
