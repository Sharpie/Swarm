// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <defobj/LispArchiver.h>
#import <collections/predicates.h>
#import <defobj/defalloc.h>
#import <collections.h> // Map

@implementation Application
+ createBegin: aZone
{
  Application *obj = [super createBegin: aZone];

  obj->streamMap = [Map create: aZone];
  obj->name = "EMPTY";

  return obj;
}

- setName: (const char *)theName
{
  name = STRDUP (theName);
  return self;
}

- getStreamMap
{
  return streamMap;
}

- (void)drop
{
  [streamMap drop];
  [super drop];
}
@end

static void
lispProcessPairs (id aZone, 
                  id stream,
                  void (*mapUpdateFunc) (id <String>, id <InputStream> stream))
{
  id obj = [stream getExpr];
  if (!archiver_list_p (obj))
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
            id key = [consObject getCar];
            
            if (quotedp (key))
              key = [key getQuotedObject];
            
            key = [(id <Copy>)key copy: aZone];
            
            if (archiver_list_p (key))
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
                key = [(id <Copy>)first copy: aZone];
              }
            
            if (!stringp (key))
              raiseEvent (InvalidArgument, "key not a string");
            mapUpdateFunc (key,
                           [InputStream create: aZone
                                        setExpr: [consObject getCdr]]);
          }
        }
    }
    [listExprIndex drop];
  }
}

static void
lispProcessMakeObjcPairs (id aZone, id stream, id app)
{
  void mapUpdate (id <String> key, id <InputStream> stream)
    {
      id objectMap;
      
      objectMap = [app getStreamMap];
      if ([objectMap at: key] == nil)
        [objectMap at: key insert: stream];
      else
	{
	  raiseEvent (WarningMessage, "Duplicate object key `%s'",
		      [key getC]);
            [key drop];
	}
    }
  lispProcessPairs (aZone, stream, mapUpdate);
}

static void
lispProcessApplicationPairs (id aZone,
                             id <InputStream> stream,
                             id <Map> applicationMap)
{
  void mapUpdate (id key, id substream)
    {
      Application *app = [applicationMap at: key];

      if (app == nil)
        {
          app = [[(Application *)[Application createBegin: aZone]
                   setName: [key getC]]
                  createEnd];
          [applicationMap at: key insert: app];
        }
      else
        [key drop];
      lispProcessMakeObjcPairs (aZone, substream, app);
    }
  lispProcessPairs (aZone, stream, mapUpdate);
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
  
  [self ensureApp: currentApplicationKey];

  if (!inhibitLoadFlag)
    {
      // Create zone for easy destruction of expressions,
      // but don't drop it yet, since we will be doing
      // lazy evaluation on the saved pairs
      inStreamZone = [SwarmZone create: getZone (self)];
      [self _load_];
    }
  else
    inStreamZone = nil;
  return self;
}

PHASE(Setting)

- ensureApp: appKey
{
  id app;
  
  if ((app = [applicationMap at: appKey]) == nil)
    {
      app = [[(Application *)[Application createBegin: getZone (self)]
               setName: [appKey getC]]
              createEnd];
      
      [applicationMap at: appKey insert: app];
    }
  return app;
}

- (void)lispLoadArchiver: (id <InputStream>)stream
{
  id aZone = getZone (self);

  if (systemArchiverFlag)
    {
      id archiverCallExprIndex, archiverCallName;
      id expr = [stream getExpr];
      
      if (!archiver_list_p (expr))
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
                                   [[[InputStream createBegin: aZone]
                                      setExpr: [archiverCallExprIndex next]]
                                     createEnd],
                                   applicationMap);
      [archiverCallExprIndex drop];
    }
  else 
    lispProcessMakeObjcPairs (aZone,
                              stream,
                              [self ensureApp: currentApplicationKey]);
}

PHASE(Using)

- (BOOL)_load_
{
  FILE *fp = fopen (path, "r");
  id <InputStream> inStream;
  
  if (fp == NULL)
    return NO;

  inStream = [InputStream create: inStreamZone setFileStream: fp];  
  [self lispLoadArchiver: inStream];
  fclose (fp);
  return YES;
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
lisp_output_objects (id app, id outputCharStream,
                     BOOL deepFlag, BOOL systemArchiverFlag)
{
  id <Map> streamMap = [app getStreamMap];

  if ([streamMap getCount] > 0)
    {
      id index = [streamMap begin: scratchZone];
      id key, member;
      
      for (member = [index next: &key];
           [index getLoc] == (id) Member;
           member = [index next: &key])
        {
          id expr = [member getExpr];
          [outputCharStream catC: "\n"];
          if (systemArchiverFlag)
            [outputCharStream catC: "      "];
	  [outputCharStream catC: " "];
          [outputCharStream catSeparator];
          [outputCharStream catStartCons];
          [outputCharStream catSeparator];
          [outputCharStream catSymbol: [key getC]];
          [outputCharStream catC: "\n"];
          if (systemArchiverFlag)
            [outputCharStream catC: "      "];
          [outputCharStream catC: "    "];
          [outputCharStream catSeparator];
          
          if (!archiver_list_p (expr) && !nil_value_p (expr))
            raiseEvent (InvalidOperation,
                        "parsed ArchiverList instance expected or nil");
          [expr lispOutDeep: outputCharStream];
          [outputCharStream catEndCons];
        }
    }
}

static void
lisp_output_app_objects (id app, id outputCharStream, BOOL systemArchiverFlag)
{
  [outputCharStream catStartList];
  lisp_output_objects (app, outputCharStream, YES, systemArchiverFlag);
  [outputCharStream catEndList];
}


- _lispOut_: outputCharStream
{
  if (systemArchiverFlag)
    {
      id appMapIndex = [applicationMap begin: scratchZone];
      id app;
      id <String> appKey;
      
      [outputCharStream catStartFunction: ARCHIVER_FUNCTION_NAME];
      [outputCharStream catC: "\n "];
      [outputCharStream catSeparator];
      [outputCharStream catStartList];
      
      while ((app = [appMapIndex next: &appKey]))
        {
          [outputCharStream catC: "\n   "];
          [outputCharStream catSeparator];
          [outputCharStream catStartCons];
          [outputCharStream catSeparator];
          lisp_print_appkey ([appKey getC], outputCharStream);
          [outputCharStream catC: "\n      "];
          lisp_output_app_objects (app, outputCharStream, YES);
          [outputCharStream catEndCons];
        }
      [outputCharStream catEndList];
      [outputCharStream catEndFunction];
      [outputCharStream catC: "\n"];
      [appMapIndex drop];
    }
  else
    lisp_output_app_objects ([self ensureApp: currentApplicationKey],
                             outputCharStream, NO);
  return self;
}

static void
archiverLispPut (id app, const char *keyStr, id value, BOOL deepFlag)
{
  id <Map> streamMap = [app getStreamMap];
  id <Zone> aZone = [streamMap getZone];
  id <String> key = [String create: aZone setC: keyStr];
  id stream = [[[OutputStream createBegin: aZone] setExprFlag: YES] createEnd];

  if (value)
    {
      if (deepFlag)
        [value lispOutDeep: stream];
      else
        [value lispOutShallow: stream];
    }
  else
    [stream catNil];

  if ([streamMap at: key])
    [streamMap at: key replace: stream];
  else
    [streamMap at: key insert: stream];
}

- (void)putDeep: (const char *)key object: object
{
  archiverLispPut ([self getApplication], key, object, YES);
}

- (void)putShallow: (const char *)key object: object
{
  archiverLispPut ([self getApplication], key, object, NO);
}

static id
archiverLispGet (id aZone, id string, id app)
{
  id stream = [[app getStreamMap] at: string];
  id obj = nil;

  if (stream)
    obj = lispIn (aZone, [stream getExpr]);
  return obj;
}

- _getWithZone_: aZone key: (const char *)key 
{
  id string = [String create: getZone (self) setC: key];
  id app = [self getApplication];
  id result; 
  
  result = archiverLispGet (aZone, string, app);

  [string drop];
  return result;
}

- getObject: (const char *)key
{
  return [self _getWithZone_: getZone (self) key: key];
}

- getWithZone: aZone key: (const char *)key
{
  return [self _getWithZone_: aZone key: key];
}

- (void)sync
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
}

- (void)drop
{
  if (inStreamZone)
    [inStreamZone drop];
  [super drop];
}

@end
