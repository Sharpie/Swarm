// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/LispArchiver.h>
#import <collections/predicates.h>

externvardef LispArchiver_c *lispArchiver;
externvardef LispArchiver_c *lispAppArchiver;

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

@implementation LispArchiver_c
PHASE(Creating)

+ createBegin: aZone
{
  LispArchiver_c *newArchiver = [super createBegin: aZone];
  return newArchiver;
}

+ create: aZone setPath: (const char *)thePath
{
  return [super create: aZone setPath: thePath];
}

- setDefaultPath
{
  path = defaultPath (SWARMARCHIVER_LISP);
  return self;
}

- setDefaultAppPath
{
  path = defaultAppPath ([arguments getAppDataPath],
                         [arguments getAppName],
                         SWARMARCHIVER_LISP_SUFFIX);
  return self;
}

- createEnd
{
  [super createEnd];
  
  currentApplicationKey = [self createAppKey: [arguments getAppName]
                                mode: [arguments getAppModeString]];

  if (!inhibitLoadFlag)
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
  return self;
}

PHASE(Setting)

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

static void
archiverLispPut (id aZone, const char *keyStr, id value, id addMap, 
                 id removeMap)
{
  id key = [String create: [addMap getZone] setC: keyStr];
  
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

  archiverLispPut ([self getZone], key, object, [app getDeepMap], 
               [app getShallowMap]);
  return self;
}

- putShallow: (const char *)key object: object
{
  id app = [self getApplication];

  archiverLispPut ([self getZone], key, object, [app getShallowMap], 
               [app getDeepMap]);
  return self;
}

static id
archiverLispGet (id aZone, id string, id app)
{
  id result;

  id valexpr = [[[app getDeepMap] at: string] getExpr];
  if (valexpr == nil)
    valexpr = [[[app getShallowMap] at: string] getExpr];
  
  if (valexpr != nil)
    result = lispIn (aZone, valexpr);
  else
    return nil;
  return result;
}

- _getWithZone_: aZone _object_: (const char *)key 
{
  id string = [String create: [self getZone] setC: key];
  id app = [self getApplication];
  id result; 
  
  result = archiverLispGet (aZone, string, app);

  [string drop];
  return result;
}

- getObject: (const char *)key
{
  return [self _getWithZone_: [self getZone] _object_: key];
}

- getWithZone: aZone object: (const char *)key
{
  return [self _getWithZone_: aZone _object_: key];
}

- save
{
  FILE *fp = fopen (path, "w");
  id outStream;
  
  [super updateArchiver];

  if (fp == NULL)
    raiseEvent (SaveError, "Cannot open lisp archive %s", path);
  outStream = [OutputStream create: scratchZone setFileStream: fp];
  [self _lispOut_: outStream];
  fclose (fp);
  [outStream drop];

  return self;
}

- (void)drop
{
  if (inStreamZone)
    [inStreamZone drop];
  [super drop];
}

@end
