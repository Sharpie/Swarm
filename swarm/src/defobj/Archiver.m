// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#import <collections.h>
#import <collections/predicates.h>
#import <defobj.h> // arguments

#import <objc/objc-api.h>

#include <misc.h> // access, getenv, xmalloc, stpcpy, strdup

#define ARCHIVER_FUNCTION_NAME "archiver"

#define SWARMARCHIVER ".swarmArchiver"

Archiver *archiver;

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

static int
compareStrings (id val1, id val2)
{
  return [val1 compare: val2];
}

static void
lispProcessPairs (id aZone, id obj, id (*func)(id, id), id map)
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
            {
              id value = func (aZone, [consObject getCdr]);
              
              if ([map at: key])
                [map at: key replace: value];
              else
                [map at: key insert: value];
            }
          }
        }
    }
    [listExprIndex drop];
  }
}

static id
lispProcessMakeObjcPairs (id aZone, id expr)
{
  id objectMap = [Map createBegin: aZone];
  [objectMap setCompareFunction: &compareStrings];
  objectMap = [objectMap createEnd];
  
  lispProcessPairs (aZone, expr, lispIn, objectMap);
          
  return objectMap;
}

static void
lispProcessApplicationPairs (id aZone, id expr, id applicationMap)
{
  lispProcessPairs (aZone, expr, lispProcessMakeObjcPairs, applicationMap);
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
  id clients = archiver->clients;

  if (![clients contains: client])
    [clients addLast: client];
}

void
archiverUnregister (id client)
{
  id clients = archiver->clients;
  
  [clients remove: client];
}

void
archiverPut (const char *key, id object)
{
  id map = [archiver getMap];
  id keyObj = [String create: [archiver getZone] setC: key];
  
  if ([map at: keyObj])
    [map at: keyObj replace: object];
  else
    [map at: keyObj insert: object];
}

id
archiverGet (const char *key)
{
  id string = [String create: [archiver getZone] setC: key];
  id result = [[archiver getMap] at: string];
  
  [string drop];
  return result;
}

void
archiverSave (void)
{
  [archiver save];
}

@implementation Archiver

PHASE(Creating)

+ createBegin: aZone
{
  Archiver *newArchiver = [super createBegin: aZone];
  id map = [Map createBegin: aZone];

  [map setCompareFunction: &compareStrings];
  newArchiver->applicationMap = [map createEnd];
  newArchiver->clients = [List create: aZone];
  newArchiver->lispPath = lispDefaultPath ();
  return newArchiver;
}

- createEnd
{
  const char *appName = [arguments getAppName];
  const char *appModeString = [arguments getAppModeString];

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
          
          lispLoadArchiverExpr (applicationMap, [inStream getExpr]);
          [inStreamZone drop]; 
          fclose (fp);
        }
    }
  return self;
}

PHASE(Setting)

- setLispPath: (const char *)thePath
{
  lispPath = strdup (thePath);

  return self;
}

PHASE(Using)

- getMap
{
  id objectMap;

  objectMap = [applicationMap at: currentApplicationKey];

  if (objectMap == nil)
    {
      objectMap = [Map createBegin: [self getZone]];
      [objectMap setCompareFunction: &compareStrings];
      objectMap = [objectMap createEnd];
      
      [applicationMap at: currentApplicationKey insert: objectMap];
    }
  return objectMap;
}

- lispOut: outputCharStream
{
  id appMapIndex = [applicationMap begin: scratchZone];
  id objectMap, appKey;
  
  [outputCharStream catC: "(" ARCHIVER_FUNCTION_NAME "\n  (list"];
  
  while ((objectMap = [appMapIndex next: &appKey]))
    {
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
          [member lispOut: outputCharStream];
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
      [clients forEach: @selector (updateArchiver)];
      [self lispOut: outStream];
      fclose (fp);
      [outStream drop];
    }
  return self;
}

- (void)drop
{
  [applicationMap drop];
  if (lispPath)
    XFREE (lispPath);
  [clients drop];
  [super drop];
}

@end
