// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/Archiver.h>

#import <collections.h>
#import <collections/predicates.h>
#import <objectbase.h> // arguments

#import <objc/objc-api.h>

#include <misc.h> // access, getenv, xmalloc, stpcpy, strdup

#define ARCHIVER_FUNCTION_NAME "archiver"
#define MAKE_OBJC_FUNCTION_NAME "make-objc"

#define SWARMARCHIVER ".swarmArchiver"

id archiver;

@implementation Archiver

static const char *
defaultPath (void)
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

+ createBegin: aZone
{
  Archiver *newArchiver = [super createBegin: aZone];
  id map = [Map createBegin: aZone];

  [map setCompareFunction: &compareStrings];
  newArchiver->applicationMap = [map createEnd];
  newArchiver->clients = [List create: aZone];
  newArchiver->path = defaultPath ();
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
  return self;
}

- setPath: (const char *)thePath
{
  path = strdup (thePath);

  return self;
}

- _badValue_: obj
{
  abort ();
}

- _badType_: obj
{
  id oStream = [OutputStream create: scratchZone setFileStream: stdout];

  [obj describe: oStream];
  [oStream drop];
  abort ();
}

- processQuotedExpr: expr
{
  id value;

  if (!listp (expr))
    [self _badType_: expr];
  value = [expr getFirst];
  if (value != ArchiverLiteral)
    [self _badValue_: value];
  value = [expr getLast];
  return value;
}

- _collectRemaining_: makeExprIndex
{
  id obj;
  id newList = [List create: [self getZone]];

  while ((obj = [makeExprIndex next]))
      [newList addLast: obj];

  return newList;
}

- processMakeExpr: expr
{
  if (!listp (expr))
    [self _badType_: expr];

  {    
    id aZone = [self getZone];
    id makeExprIndex = [expr begin: scratchZone];
    
    {
      id makeExprObj = [makeExprIndex next];
      
      if (!stringp (makeExprObj))
        [self _badType_: makeExprObj];
      if (strcmp ([makeExprObj getC], MAKE_OBJC_FUNCTION_NAME) != 0)
        [self _badValue_: makeExprObj];
    }
    
    {
      id classNameString;
      Class classObject;
      id result;
      
      classNameString = [self processQuotedExpr: [makeExprIndex next]];
      if (!stringp (classNameString))
        [self _badValue_: classNameString];
      classObject = objc_lookup_class ([classNameString getC]);
      if (!classObject)
        [self _badValue_: classNameString];
      result = [classObject in: aZone
                            expr: [self _collectRemaining_: makeExprIndex]];
      
      [makeExprIndex drop];
      return result;
    }
  }
}
  
- processPairs: obj method: (SEL)method map: map
{
  if (!listp (obj))
    [self _badType_: obj];
  {
    id aZone = [self getZone];
    id listExprIndex = [obj begin: scratchZone];
    id listExpr = [listExprIndex next];
    
    if (!stringp (listExpr))
      [self _badType_: listExpr];
    if (strcmp ([listExpr getC], "list") != 0)
      [self _badValue_: listExpr];
    
    {
      id keyValue;

      while ((keyValue = [listExprIndex next]))
        {
          id consIndex = [keyValue begin: scratchZone];
          id consFuncString = [consIndex next];
          
          if (!stringp (consFuncString))
            [self _badType_: consFuncString];
          if (strcmp ([consFuncString getC], "cons") != 0)
            [self _badValue_: consFuncString];

          {
            id key = [[self processQuotedExpr: [consIndex next]] copy: aZone];

            if (listp (key))
              {
                id first = [key getFirst];
                id last = [key getLast];
                
                if (!stringp (first))
                  [self _badType_: first];
                if (!stringp (last))
                  [self _badType_: last];
                [first catC: "/"];
                [first catC: [last getC]];
                key = [first copy: aZone];
              }
            
            if (!stringp (key))
              [self _badType_: key];

            {
              id value = [self perform: method with: [consIndex next]];
              
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
  return self;
}

- processMakeObjcPairs: obj
{
  id objectMap = [Map createBegin: [self getZone]];
  [objectMap setCompareFunction: &compareStrings];
  objectMap = [objectMap createEnd];
  
  [self processPairs: obj
        method: @selector(processMakeExpr:)
        map: objectMap];
          
  return objectMap;
}

- processApplicationPairs: obj
{
  return 
    [self processPairs: obj
          method: @selector(processMakeObjcPairs:)
          map: applicationMap];
}
  
- in: expr
{
  id archiverCallExprIndex, archiverCallName;
  
  if (!listp (expr))
    [self _badType_: expr];
  
  archiverCallExprIndex = [expr begin: scratchZone];
  archiverCallName = [archiverCallExprIndex next];

  if (!stringp (archiverCallName))
    [self _badType_: archiverCallName];
  
  if (strcmp ([archiverCallName getC], ARCHIVER_FUNCTION_NAME) != 0)
    [self _badValue_: archiverCallName];

  [self processApplicationPairs: [archiverCallExprIndex next]];
  [archiverCallExprIndex drop];
  return self;
}

+ in: aZone expr: expr
{
  return [[Archiver create: aZone] in: expr];
}

+ load: aZone fromPath: (const char *)archivePath
{
  if (archivePath && access (archivePath, R_OK) != -1)
    {
      FILE *fp = fopen (archivePath, "r");

      // Create a temporary zone to simplify destruction of expression
      id inStreamZone = [Zone create: scratchZone];
      id inStream = [InputStream create: inStreamZone setFileStream: fp];
      id newArchiver = [Archiver in: aZone expr: [inStream getExpr]];

      [newArchiver setPath: archivePath];
      [inStreamZone drop]; 
      fclose (fp);
      return newArchiver;
    }
  return nil;
}

+ load: aZone
{
  return [Archiver load: aZone fromPath: defaultPath ()];
}

+ ensure: aZone path: (const char *)archivePath
{
  Archiver *newArchiver = [Archiver load: aZone fromPath: archivePath];

  if (newArchiver == nil)
    {
      newArchiver = [Archiver create: aZone];
      [newArchiver setPath: archivePath];
    }
  return newArchiver;
}

+ ensure: aZone
{
  Archiver *newArchiver;

  newArchiver = [Archiver load: aZone];
  if (newArchiver == nil)
    newArchiver = [Archiver create: aZone];
  return newArchiver;
}

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

- out: outputCharStream
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
          [outputCharStream catC: "\n          (" MAKE_OBJC_FUNCTION_NAME " '"];
          [outputCharStream catC: [member name]];
          [outputCharStream catC: " "];
          [member out: outputCharStream];
          [outputCharStream catC: "))"];
        }
      [outputCharStream catC: "))"];
    }
  [outputCharStream catC: "))\n"];
  [appMapIndex drop];
  return self;
}

- save
{
  if (path)
    {
      FILE *fp = fopen (path, "w");
      id outStream;
      
      if (fp == NULL)
        return nil;
      outStream = [OutputStream create: scratchZone setFileStream: fp];
      [clients forEach: @selector(updateArchiver)];
      [self out: outStream];
      fclose (fp);
      [outStream drop];
    }
  return self;
}

- (void)drop
{
  [applicationMap drop];
  if (path)
    XFREE (path);
  [clients drop];
  [super drop];
}

- _register_: client
{
  if (![clients contains: client])
    [clients addLast: client];
  return self;
}

- _unregister_: client
{
  [clients remove: client];
  return self;
}

void
archiverRegister (id client)
{
  [archiver _register_: client];
}

void
archiverUnregister (id client)
{
  [archiver _unregister_: client];
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

@end
