// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Symbol.m
Description:  classes to define distinct global id constants   
Library:      defobj
*/

#import <defobj/Symbol.h>
#import <collections.h>

#define __USE_FIXED_PROTOTYPES__ // for gcc headers
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

// standard program-wide allocation zones

id  _obj_globalZone;
id  _obj_scratchZone;
id  _obj_initZone;
id  _obj_sessionZone;


@implementation Symbol_c

PHASE(Creating)

- createEnd
{
  raiseEvent (SourceMessage,
    "> must use create:setName: to create a new symbol constant\n");
  return nil;
}

+ create: aZone setName: (const char *)symbolName
{
  Symbol_c *newSymbol;
  char *newNameBuf;

  newSymbol = [aZone allocIVars: getNextPhase (self)];
  newNameBuf = [aZone alloc: strlen (symbolName) + 1];
  strcpy (newNameBuf, symbolName);
  newSymbol->name = newNameBuf;
  return newSymbol;
}

PHASE(Using)

- (const char *) getName
{
  return name;
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
  [outputCharStream catC: ">symbol name: "];
  [outputCharStream catC: name];
  [outputCharStream catC: "\n"];
}

@end


@implementation EventType_c

- (void) raiseEvent
{
  // currently ignored
}

- (void) raiseEvent: (const void *)msgString, ...
{
  // currently ignored
}

@end


//
// printMessage() -- function shared by raiseEvent: in Warning_c and Error_c
//
static void
printMessage (const char *eventClass,
              const char *eventName,
              const void *eventData,
              va_list argptr,
              const char *messageString)
{
  const char *function, *file;
  int line;

  fprintf (_obj_xerror, "*** event raised for %s: %s\n",
           eventClass, eventName);

  if (((char *)eventData)[0] == '\r')
    {
      function = eventData + 1;
      file = va_arg (argptr, char *);
      line = va_arg (argptr, int);
      fprintf (_obj_xerror,
               "*** function: %s(), file: %s, line: %d\n",
               function, file, line);
      eventData = va_arg (argptr, char *);
    }
  if (eventData) 
    vfprintf (_obj_xerror, eventData, argptr);
  else if (messageString)
    vfprintf (_obj_xerror, messageString, argptr);
}

@implementation Warning_c

- (void)setMessageString: (const char *)messageStr
{
  messageString = messageStr;
}

- (const char *)getMessageString
{
  return messageString;
}

- (void)raiseEvent
{
  fprintf (_obj_xerror, "*** event raised for warning: %s\n", name);
  if (messageString)
    fputs (messageString, _obj_xerror);
  fprintf (_obj_xerror, "*** execution continuing...\n");
}

- (void)raiseEvent: (const void *)eventData, ...
{
  va_list argptr;

  if (! eventData)
    [self raiseEvent];
  va_start (argptr, eventData);
  printMessage ("warning", name, eventData, argptr, messageString);
  fprintf (_obj_xerror, "*** execution continuing...\n");
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
  if (messageString) 
    {
      [outputCharStream catC: "> default message:\n"];
      [outputCharStream catC: messageString];
    }
  else
    [outputCharStream catC: "> (no default message)\n"];
}

@end


@implementation Error_c

- (void)raiseEvent
{
  fprintf (_obj_xerror, "*** event raised for error: %s\n", name);
  if (messageString)
    fputs (messageString, _obj_xerror);
  fprintf (_obj_xerror, "*** execution terminating due to error\n");
  abort();
}

- (void)raiseEvent: (const void *)eventData, ...
{
  va_list argptr;

  if (! eventData)
    [self raiseEvent];
  va_start (argptr, eventData);
  printMessage ("error", name, eventData, argptr, messageString);
  fprintf (_obj_xerror, "*** execution terminating due to error\n");
  abort();
}

@end
